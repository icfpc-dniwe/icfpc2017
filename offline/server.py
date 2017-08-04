from subprocess import *
from enum import Enum
import json

SETUP_TIMEOUT = 10
GAMEPLAY_TIMEOUT = 1
UNSPECIFIED_TIMEOUT = 100

class ParseResult(Enum):
	OK 					= 0
	TimedOut 			= 1
	MissingSeparator 	= 2
	NotJSON 			= 3
	DataError			= 4
	GeneralError 		= 5
	Empty				= 6

	@staticmethod
	def IsValid(value):
		return value == OK

class Parser:
	@staticmethod
	def IsJSON(jsonInput):
		try:
			parsed = json.loads(jsonInput)
			return True, parsed
		except ValueError, e:
			return False, {}

	@staticmethod
	def ToJSON(d):
		return json.dumps(d), ParseResult.OK

	@staticmethod
	def FromJSON(jsonData):
		valid, d = IsJSON(jsonData)
		if not valid:
			return {}, ParseResult.NotJSON
		return d, ParseResult.OK

	## GENERAL ##
	@staticmethod
	def ParseRawMove(rawMove):
		if 'claim' in rawMove:
			if 'source' not in rawMove or 'target' not in rawMove:
				return {}, ParseResult.DataError
			return {rawMove['punter']: {rawMove['source']: rawMove['target']}}, ParseResult.OK
		elif 'pass' in rawMove:
			return {rawMove['punter']: {}}, ParseResult.OK
		else:
			return {}, ParseResult.DataError

	@staticmethod
	def ParseRawFutures(rawFutures):
		if not all(lambda ftr: 'source' in ftr and 'target' in ftr):
			return {}, ParseResult.DataError
		return zip(map(lambda ftr: (ftr['source'], ftr['target']), rawFutures.items())), ParseResult.OK

	## HANDSHAKE ##

	@staticmethod
	def PutHandshakePunterName(name):
		j, _ = ToJSON({'you': name})
		return j, ParseResult.OK

	@staticmethod
	def GetHandshakePunterName(jsonInput):
		valid, data = IsJSON(jsonInput)
		if not valid:
			return "", ParseResult.NotJSON
		return valid['me'], ParseResult.OK

	## SETUP ##

	@staticmethod
	def PutSetupPuntersMapSettings(punterId, puntersCount, settings, theMap):
		j, _ = ToJSON({'punter': punterId, 'punters': puntersCount, 'map': theMap, 'settings': settings})
		return j, ParseResult.OK

	@staticmethod
	def GetSetupReadinessState(jsonInput):
		valid, data = IsJSON(jsonInput)
		if not valid:
			return -1, "", {}, ParseResult.NotJSON
		return data['ready'], data['state'], data['futures'], ParseResult.OK

	## GAMEPLAY ##

	@staticmethod
	def PutGameplayMovesState(moves, state):
		j, _ = ToJSON({'move': {'moves': moves}, 'state': state})
		return j, ParseResult.OK

	@staticmethod
	def GetGameplayMoveState(jsonInput):
		valid, data = IsJSON(jsonInput)
		if not valid:
			return {}, "", ParseResult.NotJSON

		parsedMove, res = ParseRawMove(data['move'])
		if not ParseResult.IsValid(res):
			return {}, "", res
		return parsedMove, data['state'], ParseResult.OK

	## SCORING ##

	@staticmethod
	def PutScoringMovesScores(moves, scores, state):
		j, _ = ToJSON({'stop': {'moves': moves, 'scores': scores}, 'state': state})
		return j, ParseResult.OK

class Protocol:
	SEPARATOR = ":"

	@staticmethod
	def FromMessage(msg):
		if SEPARATOR not in msg:
			return "", ParseResult.MissingSeparator
		length, data = out.split(SEPARATOR)
		
		if len(data) != length:
			return "", ParseResult.DataError
		return data, ParseResult.OK

	@staticmethod
	def ToMessage(data):
		msg = SEPARATOR.join([len(data), data])
		return msg, ParseResult.OK

	@staticmethod
	def Communicate(pObj, message, timeout = UNSPECIFIED_TIMEOUT):
		try:
			out, err = pObj.communicate(input=message, timeout=timeout)
			if len(out) == 0:
				return "", ParseResult.Empty
			return out, ParseResult.OK
		except TimeoutExpired, e:
			return "", ParseResult.TimedOut

	def Query(pObj, timeout = UNSPECIFIED_TIMEOUT):
		msg, res = Communicate(pObj, None, timeout)
		if not ParseResult.IsValid(res):
			return "", res
		
		j, res = FromMessage(msg)
		if not not ParseResult.IsValid(res):
			return "", res
		return j

	def AnswerQuery(pObj, jsonMsg, timeout = UNSPECIFIED_TIMEOUT):
		msg, _ = ToMessage(jsonMsg)

		answer, res = Communicate(pObj, msg, timeout)
		if not ParseResult.IsValid(res):
			return "", res

		j, res = FromMessage(msg)
		if not ParseResult.IsValid(res):
			return "", res
		return j

class Game:
	mines = []
	rivers = {}
	futures = {}

	moves = {}
	sitesClaim = {}
	
	nSites = 0
	nPunters = 0
	_iPunter = 0

	class MoveKind(Enum):
		Pass = 0
		Claim = 1

	@staticmethod
	def SiteToString(siteId):
		return {'id': siteId}

	@staticmethod
	def RiverToString(src, dst):
		return {'source': SiteToString(src), 'target': SiteToString(dst)}

	@staticmethod
	def MapToString(sites, rivers, mines):
		return {\
			'sites': map(SiteToString, sites),\
			'rivers': map(RiverToString, rivers),\
			'mines': mines\
		}

	def SanityCheck(self):
		return
			all(lambda mine: mine < nSites && mine >= 0, self.mines) and\
			all(lambda k, v: k < nSites && k >= 0 && v < nSites && v >= 0, self.rivers.items()) and\
			all(lambda s, p: s < nSites && s >= 0 && p < nPunters && p >= 0, self.sitesClaim.items()) and\
			len(self.sitesClaim) == len(set(self.sitesClaim))


	def __init__(self, nPunters, nSites, mines, rivers):
		self.nSites = nSites
		self.nPunters = nPunters
		self.mines = mines
		self.rivers = rivers

	def CreatePunter(self):
		self._iPunter += 1
		return self._iPunter

	def RegisterMove(self, move):
		pnt, clm = move.items()[0]
		
		# punter passed
		if len(clm) == 0:
			return

		src, dst = clm.items()[0]
		