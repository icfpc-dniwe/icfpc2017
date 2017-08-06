import theano
from theano import tensor as T
from theano import sparse
from theano import config as Tconfig
from theano.tensor import shared_randomstreams
import lasagne
from copy import deepcopy
import networkx as nx
import numpy as np
from os.path import join
import sys

from build_net import BuildGraphNetwork
from operators import msoftmax
from network_tools import ClaimEdge, CalcFeatures, CalcMask
from net_tools import SaveNet
from common import ProdFeatures


def GetProbFunctions(num_features, learning_rate=1e-4, ret_updates=True):
    adjustment_var = T.bmatrix(name='Adjustment matrix')
    features_var = T.fmatrix(name='Features')
    mask_var = T.bvector(name='Filter mask')
    reward_var = T.scalar(name='Reward')
    net = BuildGraphNetwork(adjustment_var, features_var, mask_var, num_features)
    desc = lasagne.layers.get_output(net['desc'])
    prob = msoftmax(desc)
    reward_grad = reward_var / prob
    params = lasagne.layers.get_all_params(net['desc'], trainable=True)
    grads = theano.grad(None, params, known_grads={prob: reward_grad})
    updates = lasagne.updates.momentum(grads, params, learning_rate=learning_rate)
    action_fn = theano.function([adjustment_var, features_var, mask_var], prob)
    if ret_updates:
        updates_fn = theano.function([adjustment_var, features_var, mask_var, reward_var], desc, updates=updates, allow_input_downcast=True)
        return net, action_fn, updates_fn
    else:
        return net, action_fn


def TrainPlayer(game, net, action_fn, update_fn, num_games=1, random_player_prob=0.5, experiment_name='test', weights_path='weights'):
    num_turns = len(game['graph'].edges())
    adjustment_matrix = nx.incidence_matrix(game['graph']).astype('int8').todense()
    adjustment_matrix = (np.dot(adjustment_matrix.T, adjustment_matrix) > 0).astype('int8')
    for cur_game_idx in range(num_games):
        print('Game idx:', cur_game_idx)
        current_game = deepcopy(game)
        for turn_idx in range(num_turns):
            print('Turn idx:', turn_idx)
            print('Player:', current_game['current_player'])
            random_player = False
            if np.random.rand() < random_player_prob:
                random_player = True
                print('!Random player!')
                edges = game['graph'].edges()
                chosen_action = np.random.choice(len(edges), size=1)[0]
                chosen_edge = edges[chosen_action]
            else:
                mask = CalcMask(current_game)
                action_idx = np.where(mask == 1)[0]
                if len(action_idx) >= 2:
                    features = ProdFeatures(CalcFeatures(current_game))
                    action_prob = action_fn(adjustment_matrix, features, mask)
                    chosen_action = np.random.choice(action_idx, size=1, p=action_prob.squeeze())[0]
                    chosen_edge = game['graph'].edges()[chosen_action]
                else:
                    break
            print('Chosen action:', chosen_edge)
            current_game, reward = ClaimEdge(game, chosen_edge)
            print('Reward:', reward)
            print('Scores:')
            [print(' :', el) for el in game['player_reward']]
            if not random_player:
                update_fn(adjustment_matrix, features, mask, reward)
        SaveNet(net['desc'], join(weights_path, experiment_name + '_' + str(cur_game_idx) + '.npz'))
