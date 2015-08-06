# an example to run ar_hmm
source("ar_hmm.R")

#####################################################
# learn HMM and AR parameters 
# from input time series (observation)
# call ar_hmm.learn
T = 100 # number of iterations
para <- ar_hmm.learn(listTS$obs, NS, T)

#####################################################
# display results

print(para$AR)
print(para$pi)
print(para$states)