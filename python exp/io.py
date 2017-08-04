import os
import json
import numpy as np


def read_map(json_string):
    raw_map = json.loads(json_string)
    sites = sorted([el["id"] for el in raw_map["sites"]])
    mines = sorted(raw_map["mines"])
    points = np.zeros(len(sites), dtype='float32')
    rivers = np.zeros((len(raw_map["rivers"]), len(raw_map["rivers"])), dtype='bool')
