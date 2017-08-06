import networkx as nx
import numpy as np


def GenerateGame(size='small'):
    if size == 'small':
        graph = nx.house_x_graph()
    else:
        pass #TODO:
    for node in graph:
        graph.node[node]['mine'] = 0
    num_mines = 2
    num_players = 3
    mine_nodes = np.random.choice(graph.nodes(), num_mines, replace=False)
    for mine in mine_nodes:
        graph.node[mine]['mine'] = 1
    for source, target in graph.edges_iter():
        graph[source][target]['claimed'] = -1
    game = dict()
    game['num_players'] = num_players
    game['current_player'] = 0
    game['player_mines'] = [[] for _ in range(num_players)]
    game['graph'] = graph
    game['mines'] = mine_nodes
    CalcPoints(game)
    return game


# Precalculating points for every node (not squared)
def CalcPoints(game):
    graph = game['graph']
    for node in graph:
        graph.node[node]['points'] = None
    for mine_node in game['mines']:
        points = 1
        for source, target in nx.bfs_edges(graph, mine_node):
            if source != mine_node:
                points = graph.node[source]['points'] + 1
            if graph.node[target]['points'] is None or graph.node[target]['points'] > points:
                graph.node[target]['points'] = points
    return None


def EdgePoints(graph, edge):
    return graph.node[edge[0]]['points'] ** 2 + graph.node[edge[1]]['points'] ** 2


def ClaimEdge(game, edge, progress_game=True):
    graph = game['graph']
    player = game['current_player']
    # check if edge is already claimed
    if graph[edge[0]][edge[1]]['claimed'] >= 0:
        # Huge penalty for passing
        return -10
    reward_points = EdgePoints(graph, edge)
    if len(game['player_mines'][player]) == 0:
        # Small bonus for claiming river
        reward_points *= 1e-3
    else:
        # Get reward for claiming new node with mine already
        reward_points *= len(game['player_mines'][player])
    if progress_game:
        graph[edge[0]][edge[1]]['claimed'] = player
        game['current_player'] += 1
        if game['current_player'] >= game['num_players']:
            game['current_player'] = 0
    return game, reward_points * 0.1


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
