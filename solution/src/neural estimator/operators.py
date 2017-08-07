import theano
from theano import tensor as T
from theano import sparse
from lasagne.layers import Layer, MergeLayer


def msoftmax(x):
    e_x = T.exp(x - x.max())
    return e_x / e_x.sum()


class GraphConv(Layer):

    def __init__(self, incoming, adjustment_matrix, **kwargs):
        super(GraphConv, self).__init__(incoming, **kwargs)
        self.adjustment_matrix = adjustment_matrix

    def get_output_shape_for(self, input_shape):
        return input_shape

    def get_output_for(self, incoming, **kwargs):
        return T.dot(self.adjustment_matrix, incoming)


class DropUnnecessary(MergeLayer):

    def __init__(self, incoming_features, incoming_mask, save_dims=True, **kwargs):
        super(DropUnnecessary, self).__init__([incoming_features, incoming_mask], **kwargs)
        self.saveDims = save_dims

    def get_output_shape_for(self, input_shapes):
        return input_shapes[0]

    def get_output_for(self, incomings, **kwargs):
        if self.saveDims:
            mask = T.eq(incomings[1], 0).nonzero()
            ret = incomings[0]
            T.set_subtensor(ret[mask], 0)
        else:
            mask = T.eq(incomings[1], 1).nonzero()
            ret = incomings[0][mask]
        return ret
