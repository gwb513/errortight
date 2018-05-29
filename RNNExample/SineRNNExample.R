# remove(list = ls()) # clear the global environment

# NN links:
# NN tips and tricks
# https://www.cs.toronto.edu/~tijmen/csc321/slides/lecture_slides_lec6.pdf
# RMSprop
# http://climin.readthedocs.io/en/latest/rmsprop.html
# Don't Decay learning rate, increase batch size
# https://arxiv.org/pdf/1711.00489.pdf
# Overview of Gradient Descent (has animations)
# http://ruder.io/optimizing-gradient-descent/index.html#whichoptimizertochoose
# Regularization
# http://neuralnetworksanddeeplearning.com/chap3.html#overfitting_and_regularization
# Graphical representations of backpropagation
# http://www.yann-ollivier.org/rech/publs/artbp.pdf

source('./RNNfunctions.R')
source('./optimMethods.R')

nsamps = 10000
fs = 1000 # the sampling rate
y[1:nsamps,] = sin((1:nsamps)/(fs/20*2*pi))
y = matrix(y, nrow = nsamps, ncol = 1)
plot(y)

v = matrix(0, nrow = 2, ncol = (nsamps - 5))
z_tar = matrix(0, nrow = 1, ncol = (nsamps - 5))
v[1,] = y[4:(nsamps - 2)]
v[2,] = y[5:(nsamps - 1)]
z_tar[1,] = y[6:(nsamps)]

# reshape the data for training
getData <- function(x, y, sps = 10) {
  # y is a 1 dimensional time series
  # sps = steps per sample
  L = dim(x)[2]
  n_inputs = dim(x)[1]
  n_outputs = dim(y)[1]

  X = array(0, dim = c(L - sps, sps, n_inputs))
  Y = array(0, dim = c(L - sps, 1, n_outputs))
  for (j in 1:n_inputs) {
    for (i in 1:(L - sps)) {
      X[i, , j] = x[j, i:(i + sps - 1)]
    }
  }

  for (j in 1:n_outputs) {
    for (i in 1:(L - sps)) {

      Y[i, , j] = y[j, i + sps]
    }
  }

  return(list(X = X, Y = Y))
}
# define training data
tr_data = getData(x = t(y), y = t(y), 7)

# define test data
te_data = getData(x = t(y[600:2600,]), y = t(y[600:2600,]), 7)

# define activation function and its derivative

# ReLU
# f_h <- function(input) { #ReLU
#   input[is.na(input)] = 0
#   out = input*0
#   out[input >= 0] = input[input >= 0]
#
#   return(out)
# }
#
# g_h <- function(input) {
#   input[is.na(input)] = 0
#   out = input*0
#   out[input >= 0] = 1
#   return(out)
# }

# tanh
f_h <- function(input) return(2/(1 + exp(-2*input)) - 1) #tanh
g_h <- function(input) {
  f = 2/(1 + exp(-2*input)) - 1
  return(1 - f^2)
}

# linear
f_o <- function(input) return(input)
g_o <- function(input) return(1)

# Make the RNN
NN <- list()
NN$reg = 'L1'
NN$lambda = .00000001
NN$nlayers = 2
NN$n_inputs = 1
NN$n_outputs = 1
NN$seq_shuffle = TRUE
NN$batch_shuffle = TRUE
NN$f = c(f_h, f_o) # can change the activation functions for hidden and output layers here
NN$g = c(g_h, g_o) # derivatives
NN$n_neurons = c(NN$n_inputs,65,NN$n_outputs)
NN = initialize_layers_rnn(NN)

NN$method = 'SGD'
NN$eta = .01

NN$Tmax = 2001
NN$n_epochs = 30


NN$X_train = tr_data$X
NN$Y_train = tr_data$Y

NN$X_test = te_data$X
NN$Y_test = te_data$Y

NN$method = 'SGD'
NN$eta = .01

NN$Tmax = 2001
NN$n_epochs = 30
NN1 = NN # save to initialize the RNN with different settings
NN1 = bptt_rnn2(NN1)

# needs fixin:
# NN1 = rnn_predict(NN1,v0 = v[,250:252],tind = 1:500)

NN2 = NN
NN2$method = 'adam'
NN3$eta = .000001
NN2 = bptt_rnn2(NN2)
# needs fixin:
# NN2 = rnn_predict(NN2,v0 = v[,250:252],tind = 1:500)

NN3 = NN
NN3$method = 'rmsprop_m'
NN3$eta = .001
NN3 = bptt_rnn2(NN3)
# needs fixin:
#NN3 = rnn_predict(NN3,v0 = v[,250:252],tind = 1:500)