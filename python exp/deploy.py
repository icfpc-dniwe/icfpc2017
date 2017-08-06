import numpy as np
import json

from players import *
from train import GetProbFunctions


def StartPlayer(num_features, learn=True, learning_rate=1e-4, history_level=1,
                history_coeff=0.9, experiment_name='deploy_test'):
    net, a, u = GetProbFunctions(num_features, learning_rate=learning_rate, ret_updates=learn)
    return NetworkPlayer()

#query {"action": "settings"}
#answer {"feature_count": int}
def ReadSettings(json_string):
    pass

#query {"action": "incidence_matrix"}
#answer {"edges": [{"src": int, "dst": int, "features": double4, "valid": bool}]}
def ReadIncidenceMartix(json_string):
    # Read incidence matrix from json
    # Read features from json (every row has N features for edge) -- "features"
    # Read binary mask for filtering desired edges (=1 if interested in edge, i.e. valid claim) - "valid"
    pass


#query {"action": "get_reward", "edge": {"src": int, "dst": int}}
#answer {"edge": {"src": int, "dst": int}, "reward": double}}
def ReadReward(json_string):
    # Read scalar reward for choosing action
    pass

#query {"action": "put_probabilities", "values": [{"src": int, "dst": int, "probability": double}]}
#answer None
def WriteProbabilities(json_string):
    # Write probabilities for each interested edge
    pass
