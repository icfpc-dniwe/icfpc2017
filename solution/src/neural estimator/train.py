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
import time
from collections import Counter

from build_net import BuildGraphNetwork
from operators import msoftmax
from network_tools import ClaimEdge, CalcFeatures, CalcMask
from common import ProdFeatures


def GetProbFunctions(num_features, learning_rate=1e-4, ret_updates=True):
    adjustment_var = T.bmatrix(name='Adjustment matrix')
    features_var = T.fmatrix(name='Features')
    mask_var = T.bvector(name='Filter mask')
    reward_var = T.scalar(name='Reward')
    net = BuildGraphNetwork(adjustment_var, features_var, mask_var, num_features)
    desc = lasagne.layers.get_output(net['desc'])
    prob = msoftmax(theano.gradient.grad_clip(desc, -1, 1))
    reward_grad = reward_var / prob
    params = lasagne.layers.get_all_params(net['desc'], trainable=True)
    grads = theano.grad(None, params, known_grads={prob: reward_grad})
    updates = lasagne.updates.momentum(grads, params, learning_rate=learning_rate)
    action_fn = theano.function([adjustment_var, features_var, mask_var], prob)
    if ret_updates:
        updates_fn = theano.function([adjustment_var, features_var, mask_var, reward_var], [], updates=updates, allow_input_downcast=True)
        return net, action_fn, updates_fn
    else:
        return net, action_fn


def TrainPlayers(old_game, players, num_games=1, random_player_prob=0.5, echo_idx=1):
    num_turns = len(old_game['graph'].edges())
    # if num_turns > 10000:
    #     num_turns //= 10
    # elif num_turns > 1000:
    #     num_turns //= 4
    # elif num_turns > 100:
    #     num_turns //= 2
    adjustment_matrix = nx.incidence_matrix(old_game['graph']).astype('int8').todense()
    adjustment_matrix = (np.dot(adjustment_matrix.T, adjustment_matrix) > 0).astype('int8')
    final_winners = [0] * num_games
    prev_game_idx = 0
    start = time.time()
    for cur_game_idx in range(num_games):
        if (cur_game_idx + 1) % echo_idx == 0:
            print('Game idx:', cur_game_idx)
            print('Winners:', Counter(final_winners[prev_game_idx:cur_game_idx]))
            end = time.time()
            print('Time elapsed:', end - start)
            start = end
            prev_game_idx = cur_game_idx
        game = deepcopy(old_game)
        game['current_player'] = np.random.randint(0, game['num_players'])
        for turn_idx in range(num_turns):
            # print('Game idx:', cur_game_idx)
            # print('Turn idx:', turn_idx)
            # print('Player:', game['current_player'])
            random_player = False
            player = players[game['current_player']]
            if np.random.rand() < random_player_prob:
                random_player = True
                # print('!Random player!')
                edges = game['graph'].edges()
                chosen_action = np.random.choice(len(edges), size=1)[0]
                chosen_edge = edges[chosen_action]
            else:
                mask = CalcMask(game)
                action_idx = np.where(mask == 1)[0]
                if len(action_idx) >= 2:
                    features = ProdFeatures(CalcFeatures(game))
                    player.getAction(adjustment_matrix, features, mask)
                    chosen_action = player.getAction(adjustment_matrix, features, mask)
                    chosen_edge = game['graph'].edges()[chosen_action]
                else:
                    break
            # print('Chosen action:', chosen_edge)
            current_game, reward = ClaimEdge(game, chosen_edge)
            # print('Reward:', reward)
            # print('Scores:')
            # [print(' :', el) for el in game['player_reward']]
            if not random_player:
                player.updateParams(adjustment_matrix, features, mask, reward)
            # input()
        # SaveNet(net['desc'], join(weights_path, experiment_name + '_' + str(cur_game_idx) + '.npz'))
        for player in players:
            player.onGameEnd(cur_game_idx)
        final_winners[cur_game_idx] = np.argmax(game['player_reward'])
    return final_winners
