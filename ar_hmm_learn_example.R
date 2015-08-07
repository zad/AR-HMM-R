# an example to run ar_hmm
source("ar_hmm.R")

#####################################################
# learn HMM and AR parameters 
# from input time series (observation)
# call ar_hmm.learn
NT = 50 # number of iterations
debug = F
para <- ar_hmm.learn(listTS$obs, NS, NT, debug)

#####################################################
# display results

print(para$AR)
print(para$pi)
print(para$states)
plot(c(1:NT),para$LL,type='l')