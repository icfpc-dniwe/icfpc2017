import theano
from theano import tensor as T
from theano import sparse
from theano import config as Tconfig
from theano.tensor import shared_randomstreams
# from theano.tensor import nnet
import lasagne

from build_net import BuildGraphNetwork
from operators import msoftmax


def GetProbFunctions(num_features, learning_rate=1e-3, seed=444):
    adjustment_var = T.bmatrix(name='Adjustment matrix') #sparse.matrix('csc', dtype=Tconfig.floatX, name='Adjustment matrix')
    features_var = T.fmatrix(name='Features')
    mask_var = T.bvector(name='Filter mask')
    # action_indices = T.ivector(name='Action indices')
    reward_var = T.scalar(name='Reward')
    net = BuildGraphNetwork(adjustment_var, features_var, mask_var, num_features)
    # s_rng = shared_randomstreams.RandomStreams()
    # s_rng.seed(seed)
    desc = lasagne.layers.get_output(net['desc'])
    prob = msoftmax(desc)
    # chosen_action = s_rng.choice(a=action_indices, p=prob, dtype='int32')
    reward_grad = reward_var / prob
    params = lasagne.layers.get_all_params(net['desc'], trainable=True)
    grads = theano.grad(None, params, known_grads={prob: reward_grad})
    updates = lasagne.updates.momentum(grads, params, learning_rate=learning_rate)
    action_fn = theano.function([adjustment_var, features_var, mask_var], prob)
    updates_fn = theano.function([adjustment_var, features_var, mask_var, reward_var], desc, updates=updates, allow_input_downcast=True)
    return action_fn, updates_fn
