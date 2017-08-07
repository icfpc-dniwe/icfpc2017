import numpy as np
import socket
import json
import argparse

from players import *
from train import GetProbFunctions


def StartPlayer(num_features, learn=True, learning_rate=1e-4, history_level=1,
                history_coeff=0.9, experiment_name='deploy_test'):
    if learn:
        net, a, u = GetProbFunctions(num_features, learning_rate=learning_rate, ret_updates=True)
    else:
        net, a = GetProbFunctions(num_features, learning_rate=learning_rate, ret_updates=False)
        u = None
    return NetworkPlayer(net, a, u, reward_length=history_level, history_coeff=history_coeff,
                         save_iter=1, experiment_name=experiment_name)


class IPCWatcher(object):

    def __init__(self):
        pass

    #query {"action": "settings"}
    #answer {"action": "settings", "feature_count": int}
    def readSettings(self):
        pass

    #query {"action": "incidence_matrix"}
    #answer {"action": "incidence_matrix", "values": [{"edge": {src": int, "dst": int}, "features": double4, "valid": bool}]}
    def readIncidenceMartix(self):
        # Read incidence matrix from json
        # Read features from json (every row has N features for edge) -- "features"
        # Read binary mask for filtering desired edges (=1 if interested in edge, i.e. valid claim) - "valid"
        pass

    #query {"action": "put_probabilities", "values": [{"edge": {src": int, "dst": int}, "probability": double}]}
    #answer {"action": "put_probabilities", "reward": double}
    def writeProbabilities(self, prod):
        # Write probabilities and for each interested edge and read reward
        pass

    # query {"action": "claim", "edge": {"src": int, "dst": int}}
    # answer {"action": "claim", "reward": double}
    def writeAction(self, edge):
        # Write probabilities and for each interested edge and read reward
        pass

    # query {"action": "finished"}
    # answer {"action": "finished", "finished": bool}
    def readContinueGame(self):
        pass


def Deploy()
    watcher = IPCWatcher()
    num_features = watcher.readSettings()
    player = StartPlayer(num_features)
    while watcher.readContinueGame():
        inc_matrix, features, valid = watcher.readIncidenceMartix()
        adj_matrix = (np.dot(inc_matrix.T, inc_matrix) > 0).astype('int8')
        action = player.getAction(adj_matrix, features, valid)
        edge = np.where(inc_matrix[:, action] == 1)[0]
        reward = watcher.writeAction(edge)
        player.updateParams(adj_matrix, features, valid, reward)
    player.onGameEnd(0)


def ParseArguments(argc, argv):
    pass

if __name__ == '__main__':
    Deploy()
