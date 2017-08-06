from network_tools import *
from train import *
import sys


if __name__ == '__main__':
    net, a, u = GetProbFunctions(10)
    t = sys.argv[1]

    if t == '0':
        game = GenerateGame(2, 2, 'small')
        TrainPlayer(game, net, a, u, num_games=100, random_player_prob=0.3, experiment_name='small_test')
    elif t == '1':
        game = GenerateGame(5, 20, 'random')
        TrainPlayer(game, net, a, u, num_games=20, random_player_prob=0.3, experiment_name='random_most_test')
    elif t == '2':
        game = GenerateGame(3, 4, 'random')
        TrainPlayer(game, net, a, u, num_games=10, random_player_prob=0.1, experiment_name='random_test')
    elif t == '3':
        game = GenerateGame(4, 8, 'random')
        TrainPlayer(game, net, a, u, num_games=10, random_player_prob=0.1, experiment_name='random_more_test')
    else:
        print('Unknown argument:', t, type(t))