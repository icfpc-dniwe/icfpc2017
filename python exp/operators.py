import theano
from theano import sparse as Tsparse
from theano import tensor as T


def GraphConv(adjustment_matrix, float_features, binary_features=None):
    if binary_features is not None:
        bin_mult = adjustment_matrix * binary_features.dimshuffle(0, 'x')
        bin_final = T.any(bin_mult, 1)
    else:
        bin_final = None
    return adjustment_matrix, theano.dot(adjustment_matrix, float_features), bin_final


def elu(x):
    return T.switch(x > 0, x, T.expm1(x))


class DenseLayer(object):

    def __init__(self, input_size, output_size, weights_init, nonlinearity=T.nnet.relu):
        incoming_shape = input_size
        self.output_size = output_size
        self.output_shape = (incoming_shape, self.output_shape)
        self.W = theano.shared(weights_init)
        self.b = theano.shared(0)
        self.nonlinearity = nonlinearity

    def __call__(self, inputs, binary=False):
        if binary:
            result = 2 * inputs - 1
        else:
            result = inputs
        return self.nonlinearity(T.dot(result, self.W) + self.b)


def GraphPooling(adjustment_matrix, float_features, fn=T.max):
    pass


def GraphMaxPooling(adjustment_matrix, float_features):
    adjustment_features = adjustment_matrix * float_features.dimshuffle(0, 'x')

