import numpy as np
import json

from players import *
from train import GetProbFunctions


def StartPlayer(num_features, learn=True, learning_rate=1e-4, history_level=1,
                history_coeff=0.9, experiment_name='deploy_test'):
    net, a, u = GetProbFunctions(num_features, learning_rate=learning_rate)
    return NetworkPlayer()


def ReadIncidenceMartix(json_string):
    # Read incidence matrix from json
    pass


def ReadFeatureMatrix(json_string):
    # Read features from json (every row has N features for edge)
    pass


def ReadMask(json_string):
    # Read binary mask for filtering desired edges (=1 if interested in edge, i.e. valid claim)
    pass


def ReadReward(json_string):
    # Read scalar reward for choosing action
    pass


def WriteProbabilities(json_string):
    # Write probabilities for each interested edge
    pass
