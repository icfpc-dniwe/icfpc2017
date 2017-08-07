import numpy as np
from os.path import join

from net_tools import SaveNet


class Player(object):

    def __init__(self):
        pass

    def getAction(self, adjustment_matrix, features, mask):
        raise NotImplementedError

    def updateParams(self, adjustment_matrix, features, mask, reward):
        raise NotImplementedError

    def onGameEnd(self, cur_game_idx):
        pass


class RandomPlayer(Player):

    def __init__(self):
        super(RandomPlayer, self).__init__()

    def getAction(self, adjustment_matrix, features, mask):
        action_idx = np.where(mask == 1)[0]
        chosen_action = np.random.choice(action_idx, size=1)[0]
        return chosen_action

    def updateParams(self, adjustment_matrix, features, mask, reward):
        pass


class NetworkPlayer(Player):

    def __init__(self, net, action_fn, update_fn=None, reward_length=1, history_coeff=0.5,
                 save_iter=1, weights_path='weights', experiment_name='test'):
        super(NetworkPlayer, self).__init__()
        self.net = net
        self.action_fn = action_fn
        self.update_fn = update_fn
        if update_fn is not None:
            self.weights_path = weights_path
            self.experiment_name = experiment_name
            self.history_coeff = history_coeff
            self.save_iter = save_iter
            self.reward_length = reward_length
            self.rewards = [0] * reward_length
            self.inputs = [None for _ in range(reward_length)]
            self.adjustment_matrix = None

    def getAction(self, adjustment_matrix, features, mask):
        self.adjustment_matrix = adjustment_matrix
        action_idx = np.where(mask == 1)[0]
        action_prob = self.action_fn(self.adjustment_matrix, features, mask)
        # print('  Maximum prob:', np.max(action_prob))
        # print('  Random:', 1 / len(action_idx))
        if np.any(np.isnan(action_prob)):
            print('OUCH!!!!')
            input()
        chosen_action = np.random.choice(action_idx, size=1, p=action_prob.squeeze())[0]
        return chosen_action

    def updateParams(self, adjustment_matrix, features, mask, reward):
        if self.update_fn is None:
            return
        self.applyUpdates()
        self.rewards += [reward]
        self.inputs += [[features, mask]]

    def onGameEnd(self, cur_game_idx):
        if self.update_fn is None:
            return
        while len(self.rewards) > 0:
            self.applyUpdates()
        self.rewards = [0] * self.reward_length
        self.inputs = [None for _ in range(self.reward_length)]
        if (cur_game_idx + 1) % self.save_iter == 0:
            SaveNet(self.net['desc'], join(self.weights_path, self.experiment_name + '_' + str(cur_game_idx) + '.npz'))

    def applyUpdates(self):
        if self.update_fn is None or len(self.inputs) <= 0:
            return
        if self.inputs[0] is not None:
            cur_reward = 0
            for old_reward in reversed(self.rewards):
                cur_reward *= self.history_coeff
                cur_reward += old_reward
            self.update_fn(self.adjustment_matrix, self.inputs[0][0], self.inputs[0][1], cur_reward)
        self.rewards = self.rewards[1:]
        self.inputs = self.inputs[1:]
