import theano
from theano import tensor as T

# CuDNN imports
from lasagne.layers.dnn import BatchNormDNNLayer as BatchNormLayer
from lasagne.layers.dnn import batch_norm_dnn as batch_norm
# Standart layers
from lasagne.layers import DenseLayer, DenseLayer, InputLayer, FlattenLayer
# Nonlinearity functions
from lasagne.nonlinearities import elu, rectify

# Our operations for graph
from operators import GraphConv, DropUnnecessary


def GraphConvCell(input, adjustment_matrix, num_units, nonlinearity=rectify):
    graph_conv = GraphConv(input, adjustment_matrix)
    # bn1 = BatchNormLayer(graph_conv)
    dense = DenseLayer(graph_conv, num_units, nonlinearity=nonlinearity)
    bn2 = batch_norm(dense)
    return bn2


def BuildGraphNetwork(adjustment_matrix, features, unnecessary_mask, num_features):
    net = dict()
    # net['input_adj'] = InputLayer((None, None, None), input_var=adjustment_matrix)
    net['adjustment_matrix'] = adjustment_matrix
    net['nonlinearity'] = elu
    net['input_features'] = InputLayer((None, num_features), input_var=features)
    net['input_mask'] = InputLayer((None,), input_var=unnecessary_mask)
    net['cell1'] = GraphConvCell(net['input_features'], net['adjustment_matrix'], 32, net['nonlinearity'])
    net['cell2'] = GraphConvCell(net['cell1'], net['adjustment_matrix'], 32, net['nonlinearity'])
    net['dropped'] = DropUnnecessary(net['cell2'], net['input_mask'])
    mask = T.eq(unnecessary_mask, 1).nonzero()
    # mask = T.eq(mask, T.swapaxes(mask, 1, 2))
    net['dropped_matrix'] = adjustment_matrix[mask][:, mask]
    net['cell3'] = GraphConvCell(net['dropped'], net['dropped_matrix'], 64, net['nonlinearity'])
    net['cell4'] = GraphConvCell(net['cell3'], net['dropped_matrix'], 64, net['nonlinearity'])
    net['desc'] = DenseLayer(net['cell4'], 1, nonlinearity=net['nonlinearity'])
    return net
