from network_tools import *
from train import *
from players import *
import sys


def main():
    net, a, u = GetProbFunctions(10)
    t = sys.argv[1]

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
        rpb = 0.3
        game = GenerateGame(num_players, 4, 'newman')
        # TrainPlayer(game, net, a, u, num_games=100, random_player_prob=0.1, experiment_name='newman_test')
        net_player = lambda x: NetworkPlayer(net, a, u, 1, 0.5, save_iter=50, experiment_name='newman_test' + str(x))
        players = [net_player(_) for _ in range(num_players)]
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


if __name__ == '__main__':
    main()