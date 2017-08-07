#!/usr/bin/python3
# -*- coding: utf-8 -*-
from network_tools import *
from train import *
from players import *
from net_tools import *
import sys
from collections import Counter


def main(t):
    net, a, u = GetProbFunctions(10)

    if t == '0':
        num_players = 2
        num_games = 100
        rpb = 0.1
        game = GenerateGame(num_players, 2, 'small')
        # TrainPlayer(game, net, a, u, num_games=20, random_player_prob=0.3, experiment_name='small_test')
        net_player = lambda x: NetworkPlayer(net, a, u, 1, 0.5, save_iter=50, experiment_name='small_test' + str(x))
        players = [net_player(_) for _ in range(num_players)]
    elif t == '1':
        num_players = 5
        num_games = 10
        rpb = 0.2
        game = GenerateGame(num_players, 20, 'newman')
        # TrainPlayer(game, net, a, u, num_games=20, random_player_prob=0.3, experiment_name='newman_most_test')
        net_player = lambda x: NetworkPlayer(net, a, u, 1, 0.5, save_iter=5, experiment_name='newman_most_test' + str(x))
        players = [net_player(_) for _ in range(num_players)]
    elif t == '2':
        num_players = 4
        num_games = 1000
        rpb = 0.0
        game = GenerateGame(num_players, 10, 'newman')
        # TrainPlayer(game, net, a, u, num_games=100, random_player_prob=0.1, experiment_name='newman_test')
        net_player = lambda x, l: NetworkPlayer(net, a, u, l, 0.6, save_iter=10, experiment_name='newman_test' + str(x))
        players = [net_player(0, 2), net_player(1, 8), net_player(1, 16), RandomPlayer()]
    elif t == '3':
        num_players = 4
        num_games = 100
        rpb = 0.3
        game = GenerateGame(num_players, 8, 'newman')
        # TrainPlayer(game, net, a, u, num_games=100, random_player_prob=0.1, experiment_name='newman_more_test')
        net_player = lambda x: NetworkPlayer(net, a, u, 1, 0.5, save_iter=50, experiment_name='newman_more_test' + str(x))
        players = [net_player(_) for _ in range(num_players)]
    else:
        print('Unknown argument:', t, type(t))
        return
    # players = [net_player] * num_players
    TrainPlayers(game, players, num_games, rpb, echo_idx=10)


def prep():
    def net_player(name, length, history_coeff=0.5, net=None, a=None, u=None, save_iter=50):
        if net is None:
            net, a, u = GetProbFunctions(10)
        return NetworkPlayer(net, a, u, length, history_coeff, save_iter=save_iter, experiment_name='test_middle' + name)
    # game = GenerateGame(2, 2, 'small')
    # net, a, u = GetProbFunctions(10, learning_rate=1e-3)
    # LoadNet(net['desc'], 'weights/test_middle_small1_999.npz')
    # players = [net_player('_small0', 1, 0, net, a, u, 500), RandomPlayer()]
    # winners = TrainPlayers(game, players, 10000, 0.0, echo_idx=1000)
    # print('_small0:', Counter(winners))
    net2, a2, u2 = GetProbFunctions(10, learning_rate=1e-5)
    # LoadNet(net2['desc'], 'weights/test_middle_small0_4999.npz')
    # players = [net_player('_small1', 2, 0.5, net, a, u, 500),
    #            net_player('05', 1, 0, net2, a2, None, save_iter=10000)]
    # winners = TrainPlayers(game, players, 10000, 0.00, echo_idx=1000)
    # print('_small1:', Counter(winners))
    # game = GenerateGame(2, 4, 'newman')
    # net, a, u = GetProbFunctions(10, learning_rate=1e-4)
    # LoadNet(net['desc'], 'weights/test_middle_newman0_309.npz')
    # LoadNet(net2['desc'], 'weights/test_middle_newman0_209.npz')
    # players = [net_player('_newman00', 10, 0.8, net, a, u, save_iter=10),
    #            net_player('_newman1', 4, 0.9, net2, a2, None, save_iter=100000)]
    # winners = TrainPlayers(game, players, 100, 0.0, echo_idx=10)
    # print('_newman0:', Counter(winners))
    # net, a, u = GetProbFunctions(10, learning_rate=1e-5)
    # LoadNet(net['desc'], 'weights/test_middle_newman00_99.npz')
    # LoadNet(net2['desc'], 'weights/test_middle_newman00_99.npz')
    # players = [net_player('_newman1', 10, 0.8, net, a, u, save_iter=10),
    #            net_player('_newman2', 4, 0.9, net2, a2, None, save_iter=100000)]
    # winners = TrainPlayers(game, players, 1000, 0.0, echo_idx=10)
    # print('_newman1:', Counter(winners))
    net, a, u = GetProbFunctions(10, learning_rate=1e-5)
    net3, a3, u3 = GetProbFunctions(10, learning_rate=1e-5)
    LoadNet(net['desc'], 'weights/test_middle_newman1_999.npz')
    LoadNet(net2['desc'], 'weights/test_middle_newman1_999.npz')
    LoadNet(net3['desc'], 'weights/test_middle_newman1_999.npz')
    game = GenerateGame(4, 5, 'newman')
    players = [net_player('_newman000', 10, 0.8, net, a, u, save_iter=5),
               net_player('_newman111', 4, 0.9, net2, a2, u2, save_iter=5),
               net_player('_newman222', 16, 0.8, net3, a3, u3, save_iter=5),
               RandomPlayer()]
    winners = TrainPlayers(game, players, 200, 0.0, echo_idx=5)
    print('_newman2:', Counter(winners))


if __name__ == '__main__':
    # t = sys.argv[1]
    # main(t)
    prep()