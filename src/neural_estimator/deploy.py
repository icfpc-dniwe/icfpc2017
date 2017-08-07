import numpy as np
import socket
import json
import argparse
import sys

from players import *
from train import GetProbFunctions
from common import ProdFeatures


def StartPlayer(num_features, learn=True, learning_rate=1e-4, history_level=1,
                history_coeff=0.9, experiment_name='deploy_test'):
    if learn:
        net, a, u = GetProbFunctions(num_features, learning_rate=learning_rate, ret_updates=True)
    else:
        net, a = GetProbFunctions(num_features, learning_rate=learning_rate, ret_updates=False)
        u = None
    return NetworkPlayer(net, a, u, reward_length=history_level, history_coeff=history_coeff,
                         save_iter=10000, experiment_name=experiment_name)


class IPCWatcher(object):

    def __init__(self):
        self.num_features = 0

    #query {"action": "settings"}
    #answer {"action": "settings", "feature_count": int, "return_prob": bool}
    def readSettings(self):
        query = {'action': 'settings'}
        self.writeJSON(json.dumps(query))
        answer = json.loads(self.readJSON())
        if answer['action'] == 'settings':
            settings = dict()
            settings['feature_count'] = answer['feature_count']
            settings['return_prob'] = answer['return_prob']
            self.num_features = settings['feature_count']
        else:
            raise RuntimeError

    #query {"action": "incidence_matrix"}
    #answer {"action": "incidence_matrix", "node_count": int, "edges": [{"src": int, "dst": int, "features": [double], "valid": bool}]}
    def readIncidenceMartix(self):
        # Read incidence matrix from json
        # Read features from json (every row has N features for edge) -- "features"
        # Read binary mask for filtering desired edges (=1 if interested in edge, i.e. valid claim) - "valid"
        query = {'action': 'incidence_matrix'}
        self.writeJSON(json.dumps(query))
        answer = json.loads(self.readJSON())
        if answer['action'] != 'settings':
            raise RuntimeError
        num_edges = len(answer['edges'])
        incidence_matrix = np.zeros((answer['node_count'], num_edges), dtype='int8')
        edges = np.zeros((num_edges, 2), dtype='int32')
        features = np.zeros((num_edges, self.num_features), dtype='float32')
        mask = np.zeros((num_edges,), dtype='int8')
        for edge_idx, edge in enumerate(answer['edges']):
            src = edge['src']
            dst = edge['dst']
            edges[edge_idx, 0] = src
            edges[edge_idx, 1] = dst
            features[edge_idx, :] = np.array(edge['features'])
            mask[edge_idx] = 1 if edge['valid'] else 0
            incidence_matrix[src, edge_idx] = 1
            incidence_matrix[dst, edge_idx] = 1
        return edges, incidence_matrix, features, mask

    #query {"action": "put_probabilities", "values": [{"src": int, "dst": int, "probability": double}]}
    #answer {"action": "put_probabilities", "reward": double}
    def writeProbabilities(self, edges, prob):
        # Write probabilities and for each interested edge and read reward
        values = [{'src': e[0], 'dst': e[1], 'probability': p} for (e, p) in zip(edges, prob)]
        query = {'action': 'put_probabilities', 'values': values}
        self.writeJSON(json.dumps(query))
        answer = json.loads(self.readJSON())
        if answer['action'] != 'put_probabilities':
            raise RuntimeError
        reward = answer['reward']
        return reward

    # query {"action": "put_action", "edge": {"src": int, "dst": int}}
    # answer {"action": "put_action", "reward": double}
    def writeAction(self, edge):
        # Write probabilities and for each interested edge and read reward
        query = {'action': 'put_action', 'edge': {'src': edge[0], 'dst': edge[1]}}
        self.writeJSON(json.dumps(query))
        answer = json.loads(self.readJSON())
        if answer['action'] != 'put_action':
            raise RuntimeError
        reward = answer['reward']
        return reward

    # query {"action": "is_finished"}
    # answer {"action": "is_finished", "is_finished": bool}
    def readContinueGame(self):
        query = {'action': 'finished'}
        self.writeJSON(json.dumps(query))
        answer = json.loads(self.readJSON())
        if answer['action'] != 'finished':
            raise RuntimeError
        return not answer['finished']

    def writeJSON(self, json_string):
        print('{}:{}'.format(len(json_string), json_string))

    def readJSON(self):
        answer = input()
        splitter = answer.find(':')
        json_len = int(answer[:splitter])
        json_string = answer[splitter+1:splitter+json_len+1]
        return json_string


def Deploy():
    watcher = IPCWatcher()
    settings = watcher.readSettings()
    num_features = settings['feature_count']
    return_prob = settings['return_prob']
    num_features = num_features + ((num_features-1) * num_features) // 2
    player = StartPlayer(num_features, False)
    while watcher.readContinueGame():
        edges, inc_matrix, features, valid = watcher.readIncidenceMartix()
        adj_matrix = (np.dot(inc_matrix.T, inc_matrix) > 0).astype('int8')
        features = ProdFeatures(features)
        edges = edges[valid == 1]
        if return_prob:
            action, prob = player.getAction(adj_matrix, features, valid, return_prob=True)
            # edge = np.where(inc_matrix[:, action] == 1)[0]
            reward = watcher.writeProbabilities(edges, prob)
        else:
            action = player.getAction(adj_matrix, features, valid)
            edge = edges[np.where(inc_matrix[:, action] == 1)[0]]
            reward = watcher.writeAction(edge)
        player.updateParams(adj_matrix, features, valid, reward)
    player.onGameEnd(0)


def ParseArguments(argc, argv):
    pass

if __name__ == '__main__':
    Deploy()
