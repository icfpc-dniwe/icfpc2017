import numpy as np
from itertools import combinations


def ProdFeatures(features):
    num_elements, num_features = features.shape
    appending_features = [features]
    for i, j in combinations(range(num_features), 2):
        appending_features.append((features[:, i] * features[:, j]).reshape(num_elements, 1))
    return np.concatenate(tuple(appending_features), axis=1)