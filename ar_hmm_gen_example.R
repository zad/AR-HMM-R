# an example to run ar_hmm
source("ar_hmm.R")
####################################################
# input examples
# assume we have NT time series
NT = 1
# the length of each time series is N
N = 60
lengths = rep(N,NT)
# then, generate dummy data as follow
# assume the number of states is S
NS = 3
# the transition matrix of the Markov model:
pi <- t(matrix(
  c(0.5, 0.1, 0.4, 
    0.1, 0.7, 0.2, 
    0.1, 0.1, 0.8),
  NS,NS
))

# AR-1 parameters
# 1. y = -x + N(0,0.1)
# 2. y = x + N(0,0.1)
# 3. y = 2x + N(0,0.2)
A = c(-1, 3, 2)
V = c(0.1, 0.3, 0.2)

# generate time series by using ar_hmm model
listTS <- ar_hmm.gen(lengths, NS, pi, A, V)

