import networkx as nx
import numpy as np


def GenerateGame(num_players=2, num_mines=2, graph_type='small'):
    if graph_type == 'small':
        graph = nx.house_x_graph()
    elif graph_type == 'star':
        graph = nx.generators.classic.star_graph(num_players * num_mines * 25)
    elif graph_type == 'newman':
        graph = nx.generators.random_graphs.newman_watts_strogatz_graph(num_players * num_mines * 5, num_players * 2, 0.15)
    for node in graph:
        graph.node[node]['mine'] = 0
    print('Graph:', len(graph.nodes()), len(graph.edges()))
    # num_mines = 2
    # num_players = 2
    mine_nodes = np.random.choice(graph.nodes(), num_mines, replace=False)
    for mine in mine_nodes:
        graph.node[mine]['mine'] = 1
    for source, target in graph.edges_iter():
        graph[source][target]['claimed'] = -1
    game = dict()
    game['num_players'] = num_players
    game['current_player'] = 0
    game['player_mines'] = [[] for _ in range(num_players)]
    game['player_score'] = [0] * num_players
    game['player_reward'] = [0] * num_players
    game['player_paths'] = [dict() for _ in range(num_players)]
    game['latest_path_id'] = [0] * num_players
    game['graph'] = graph
    game['mines'] = mine_nodes
    CalcPoints(game)
    return game


# Precalculating points for every node (not squared)
def CalcPoints(game):
    graph = game['graph']
    for node in graph:
        graph.node[node]['points'] = None
        graph.node[node]['claimed'] = []
        graph.node[node]['path'] = [None for _ in range(game['num_players'])]
    for mine_node in game['mines']:
        points = 1
        for source, target in nx.bfs_edges(graph, mine_node):
            if source != mine_node:
                points = graph.node[source]['points'] + 1
            if graph.node[target]['points'] is None or graph.node[target]['points'] > points:
                graph.node[target]['points'] = points
    for node in graph:
        if graph.node[node]['points'] is None:
            graph.node[node]['points'] = 0
    return None


def EdgePoints(graph, edge):
    return graph.node[edge[0]]['points'] ** 2 + graph.node[edge[1]]['points'] ** 2


def CalcScore(game):
    player = game['current_player']
    score = 0
    # print('SSS', game['player_paths'][player])
    for _, cur_path in game['player_paths'][player].items():
        score += cur_path[0] * len(cur_path[1])
    return score


def ClaimEdge(game, edge):
    graph = game['graph']
    player = game['current_player']
    player_paths = game['player_paths'][player]
    # player_mines = game['player_mines'][player]
    # non_mined_claim = 1e-3
    # check if edge is already claimed
    if graph[edge[0]][edge[1]]['claimed'] >= 0:
        reward_points = 0
    else:
        paths = 0
        scores = 0
        added_mines = []
        if graph.node[edge[0]]['mine']:
            added_mines += [edge[0]]
        if graph.node[edge[1]]['mine']:
            added_mines += [edge[1]]
        added_mines = set(added_mines)
        if player in graph.node[edge[0]]['claimed']:
            if player in graph.node[edge[1]]['claimed']:
                # merge paths
                left_id = graph.node[edge[0]]['path'][player]
                left_path = player_paths[left_id]
                right_id = graph.node[edge[1]]['path'][player]
                right_path = player_paths[right_id]
                new_path = (left_path[0] + right_path[0], left_path[1].union(right_path[1]).union(added_mines))
                new_id = left_id
                player_paths.pop(right_id, None)
            else:
                # add point (edge[1]) to path
                new_id = graph.node[edge[0]]['path'][player]
                added_score = graph.node[edge[1]]['points'] ** 2
                new_path = player_paths[new_id]
                new_path[0] += added_score
                new_path[1] = new_path[1].union(added_mines)
        else:
            if player in graph.node[edge[1]]['claimed']:
                # add point (edge[0]) to path
                new_id = graph.node[edge[0]]['path'][player]
                added_score = graph.node[edge[1]]['points'] ** 2
                new_path = player_paths[new_id]
                new_path[0] += added_score
                new_path[1] = new_path[1].union(added_mines)
            else:
                # add new path
                added_score = EdgePoints(graph, edge)
                new_path = (added_score, added_mines)
                game['latest_path_id'][player] += 1
                new_id = game['latest_path_id'][player]
        player_paths[new_id] = new_path
        game['player_paths'][player] = player_paths
        new_score = CalcScore(game)
        score_diff = new_score - game['player_score'][player]
        game['player_score'][player] = new_score
        game['player_reward'][player] += score_diff
        for other_player in range(game['num_players']):
            if other_player == player:
                continue
            game['player_reward'][other_player] -= score_diff / game['num_players']
        reward_points = game['player_reward'][player]
    graph[edge[0]][edge[1]]['claimed'] = player
    game['current_player'] += 1
    if game['current_player'] >= game['num_players']:
        game['current_player'] = 0
    return game, (reward_points - 0.9) * 1e-2


def CalcFeatures(game):
    graph = game['graph']
    features = np.zeros((len(graph.edges()), 4), dtype='float32')
    for idx, (source, target) in enumerate(graph.edges_iter()):
        features[idx, 0] = EdgePoints(graph, (source, target)) * 1e-2
        if graph.node[source]['mine'] > 0:
            features[idx, 1] = 1
        if graph[source][target]['claimed'] == game['current_player']:
            features[idx, 2] = 1
        elif graph[source][target]['claimed'] >= 0:
            features[idx, 3] = 1
    return features


def CalcMask(game):
    graph = game['graph']
    mask = np.ones((len(graph.edges()),), dtype='int8')
    for idx, (source, target) in enumerate(graph.edges_iter()):
        if graph[source][target]['claimed'] >= 0:
            mask[idx] = 0
    return mask
