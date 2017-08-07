#!/usr/bin/python3
# -*- coding: utf-8 -*-
import lasagne
import numpy as np


def SaveNet(net, file_path):
    np.savez_compressed(file_path, lasagne.layers.get_all_param_values(net))


def LoadNet(net, file_path):
    params = np.load(file_path)
    params = [p.astype(np.float32) for p in params['arr_0']]
    lasagne.layers.set_all_param_values(net, params)
    return net