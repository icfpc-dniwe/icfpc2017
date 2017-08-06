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
        num_players = 3
        num_games = 100
        rpb = 0.05
        game = GenerateGame(num_players, 4, 'newman')
        # TrainPlayer(game, net, a, u, num_games=100, random_player_prob=0.1, experiment_name='newman_test')
        net_player = lambda x, l: NetworkPlayer(net, a, u, l, 0.8, save_iter=50, experiment_name='newman_test' + str(x))
        players = [net_player(0, 1), net_player(1, 10), RandomPlayer()]
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
    TrainPlayers(game, players, num_games, rpb)


def prep():
    def net_player(name, length, history_coeff=0.5, net=None, a=None, u=None, save_iter=50):
        if net is None or a is None or u is None:
            net, a, u = GetProbFunctions(10)
        return NetworkPlayer(net, a, u, length, history_coeff, save_iter=save_iter, experiment_name='test' + name)
    game = GenerateGame(2, 2, 'small')
    net, a, u = GetProbFunctions(10)
    players = [net_player('_small0', 1, 0, net, a, u, 500), RandomPlayer()]
    winners = TrainPlayers(game, players, 1000, 0.1, echo_idx=500)
    print('_small0:', Counter(winners))
    net2, a2, u2 = GetProbFunctions(10)
    LoadNet(net2['desc'], 'weights/test_small0_499.npz')
    players = [net_player('_small1', 2, 0.5, net, a, u, 500), net_player('05', 1, 0, net2, a2, None, save_iter=10000)]
    winners = TrainPlayers(game, players, 1000, 0.01, echo_idx=500)
    print('_small1:', Counter(winners))
    game = GenerateGame(3, 5, 'newman')
    LoadNet(net2['desc'], 'weights/test_small1_999.npz')
    players = [net_player('_newman2', 10, 0.8, net, a, u, save_iter=5),
               net_player('15', 0, 0, net2, a2, None, save_iter=10000),
               RandomPlayer()]
    winners = TrainPlayers(game, players, 100, 0.01, echo_idx=2)
    print('_newman2:', Counter(winners))


if __name__ == '__main__':
    # t = sys.argv[1]
    # main(t)
    prep()