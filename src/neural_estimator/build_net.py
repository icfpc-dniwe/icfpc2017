#!/usr/bin/python3
# -*- coding: utf-8 -*-
import theano
from theano import tensor as T

import lasagne
# Standart layers
from lasagne.layers import DenseLayer, InputLayer, ConcatLayer, NonlinearityLayer, BatchNormLayer, batch_norm
# Nonlinearity functions
from lasagne.nonlinearities import elu, rectify, identity

# Our operations for graph
from operators import GraphConv, DropUnnecessary


def GraphConvCell(input, adjustment_matrix, num_units, nonlinearity=rectify):
    graph_conv = GraphConv(input, adjustment_matrix)
    # bn1 = BatchNormLayer(graph_conv)
    conc = ConcatLayer([input, graph_conv], axis=1)
    dense = DenseLayer(conc, num_units, nonlinearity=identity)
    bn2 = BatchNormLayer(dense)
    return NonlinearityLayer(bn2, nonlinearity=nonlinearity)


def BuildGraphNetwork(adjustment_matrix, features, filter_mask, num_features):
    net = dict()
    # net['input_adj'] = InputLayer((None, None, None), input_var=adjustment_matrix)
    net['adjustment_matrix'] = adjustment_matrix
    net['nonlinearity'] = elu
    net['input_features'] = InputLayer((None, num_features), input_var=features)
    net['input_mask'] = InputLayer((None,), input_var=filter_mask)
    net['dense1'] = batch_norm(DenseLayer(net['input_features'], 32, nonlinearity=net['nonlinearity']))
    net['cell1'] = GraphConvCell(net['dense1'], net['adjustment_matrix'], 16, net['nonlinearity'])
    net['cell2'] = GraphConvCell(net['cell1'], net['adjustment_matrix'], 16, net['nonlinearity'])
    net['cell3'] = GraphConvCell(net['cell2'], net['adjustment_matrix'], 16, net['nonlinearity'])
    net['cell4'] = GraphConvCell(net['cell3'], net['adjustment_matrix'], 16, net['nonlinearity'])
    net['dropped'] = DropUnnecessary(net['cell4'], net['input_mask'], save_dims=True)
    mask = T.eq(filter_mask, 0).nonzero()
    # mask = T.eq(mask, T.swapaxes(mask, 1, 2))
    net['dropped_matrix'] = adjustment_matrix
    # T.set_subtensor(net['dropped_matrix'][mask][:, mask], 0)
    net['cell5'] = GraphConvCell(net['dropped'], net['dropped_matrix'], 16, net['nonlinearity'])
    net['cell6'] = GraphConvCell(net['cell5'], net['dropped_matrix'], 16, net['nonlinearity'])
    net['cell7'] = GraphConvCell(net['cell6'], net['dropped_matrix'], 32, net['nonlinearity'])
    net['cell8'] = GraphConvCell(net['cell7'], net['dropped_matrix'], 256, net['nonlinearity'])
    net['dense_desc'] = DenseLayer(net['cell8'], 1, nonlinearity=identity)
    net['desc'] = DropUnnecessary(net['dense_desc'], net['input_mask'], save_dims=False)
    return net
