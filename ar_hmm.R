# ar_hmm 
# limits:
MIN_TM_C = 1
MIN_V = 0.001
MIN_PROB = 1e-10

source("small_number.R")

# Markov Chain functions
mc.sample <- function(TM, pre_state){
  if(pre_state <0){
    # this is the initial state
    state <- sample_mul(rep(1.0/nrow(TM),nrow(TM)))
  }else{
    state <- sample_mul(TM[pre_state,])
  }
  
  #print(state)
  return(state)
}

# multinormial sampling
sample_mul <- function(p){
  sample <- rmultinom(1,size=1,prob=p)
  return(match(1,sample))
}


# learn parameters by using AR-HMM model
ar_hmm.learn <- function(obs,NS,T){
  print("ar_hmm.learn")
  # randomly initialize states
  states <- initStates(obs,NS)
  # use Gibbs sampling
  debug = TRUE
  for(t in 1:T){
    print(paste("Iteration:",t))
    # update state variables
    NTS <- length(obs)
    print(paste("number of time series:",NTS))
    # counter for transition matrix
    TM_C <- matrix(rep(MIN_TM_C,NS*NS),NS,NS)
    Y = list()
    X = list()
    for(i in 1:NTS){
      N <- length(obs[[i]])
      for(j in 1:N){
        if(j>1){
          cur_s = states[[i]][j]
          pre_s = states[[i]][j-1]
          TM_C[pre_s,cur_s] = TM_C[pre_s,cur_s] + 1
        }
      }
      for(s in 1:NS){
        y_idx = which(states[[i]]==s)  
        y_idx = y_idx[y_idx > 1]
        y = obs[[i]][y_idx]
        x = obs[[i]][y_idx-1]
        if(i == 1)
        {
          Y[[s]] = y
          X[[s]] = x
        }
        else
        {
          Y[[s]] = append(Y[[s]],y)
          X[[s]] = append(X[[s]],x)
        }
      }
    }
    # learn transition matrix
    pi <- learnTM(TM_C)
    # learn AR parameters
    #print(Y[[1]])
    ar <- learnAR(Y,X,debug)
    # use pi, ar to update states
    # (consider special case j = 1)
    for(i in 1:NTS){
      N <- length(obs[[i]])
      g_jp1 = list(1,1,1)
      for(j in N:1){
        prob_j = list()
        g_j = list()
        # update function g at j-th sample
        for(s in 1:NS){
          if(j==1)
          {
            # spercial case j=1
            fx = 1
          }
          else
          {
            fx = f(obs[[i]][j],obs[[i]][j-1],s,ar,debug)
          }
          sx = c(0,0)
          for(k in 1:NS){
            smm = sn.multiply(pi[s,k], g_jp1[[k]])
            sx = sn.add(sx, smm)
          }
          
          if(debug)
            print(c("fx,sx:",fx,sx))
          
          g_j[[s]] = sn.multiply(fx, sx)
          print(paste("g_j[[s]]",g_j[[s]]))
        
        }
        g_jp1 = g_j
        # calculate Prob(s|s_pre,Y,pi)
        if(debug)print(c("g_j:",g_j))
        for(s in 1:NS){
          if(j==1){
            # spercial case j=1
            prob_j[[s]] = g_j[[s]]
          }else{
            prob_j[[s]] = sn.multiply(pi[states[[i]][j-1],s], g_j[[s]])
          }
            
        }
        
        prob_j <- normalize_prob(prob_j, MIN_PROB)
        # sampling new state at j
        if(debug)print(prob_j)
        new_state = sample_mul(prob_j)
        # update state
        states[[i]][j] = new_state
      }
    }
  }
  para <- list(states, ar, pi)
  names(para) = c("states","AR", "pi")
  return(para)
}

# ar_hmm.gen
# generate time series based on input parameters
ar_hmm.gen <- function(N,S,pi,A,V){
  listTS <- list(obs=list(),states=list())
  for(i in 1:length(N)){
    # generate i-th time series
    n = N[i]
    print(paste0("generate time series:",i
                 ," num of samples:",n))
    states <- rep(-1,n)
    obs <- rep(0,n)
    
    for(j in 1:n){
      # sample j-th state at i-th time series
      print(paste0("sample ",j,"-th state at ",i,"-th time series"))
      if(j == 1)
        states[j] <- mc.sample(pi,-1)
      else
        states[j] <- mc.sample(pi,states[j-1])
      # sample current observation by using AR-1
      print(paste("state at", i, " ",n, ":", states[j]))
      s = states[j]
      a = A[s]
      e = V[s]
      
      if(j == 1){
        mu = 0 # assume the previous observation is equal to 0
      }else{
        mu = a*obs[j-1]
      }
      obs[j] = mu + e*rnorm(1)
    }
    listTS$states[[i]] <- states
    listTS$obs[[i]] <- obs
  }
  return(listTS)
}

# randomly initialize states for each observation
initStates <- function(obs,NS){
  NTS = length(obs)
  states = list()
  for(i in 1:NTS){
    NOBS = length(obs[[i]])
    states[[i]] <- sample(NS,NOBS,replace=TRUE)
  }
  return(states)
}

# update transition matrix based on counters on states
learnTM <- function(TM_C){
  NS <- nrow(TM_C)
  pi <- matrix(rep(0,NS*NS),NS,NS)
  for(r in 1:NS){
    sum_r = sum(TM_C[r,])
    for(c in 1:NS){
      pi[r,c] = TM_C[r,c]*1.0/sum_r
    }
  }
  print("pi:")
  print(pi)
  return(pi)
}

learnAR <- function(Y,X,debug){
  NS <- length(Y)
  A = rep(0,NS)
  V = rep(0,NS)
  for(s in 1:NS){
    y = Y[[s]]
    x = X[[s]]
    if(debug)print(paste("length y:",length(y)))
    if(length(y) == 0){
      A[s] = 1
      V[s] = MIN_V
    }
    else if(length(y) == 1){
      A[s] = y/x
      V[s] = MIN_V
    }else{
      fit <- lm(y ~ 0 + x)
      A[s] = fit$coefficients
      V[s] = sd(fit$residuals)  
      if(V[s] < MIN_V) V[S]=MIN_V
    }
    if(debug)print(paste("AR",s,"A:",A[s],"V:",V[s]))
  }
  ar <- list(A,V)
  names(ar) = c("A","V")  
  return(ar)
}


f <- function(y,y_pre,s,ar,debug){
  A = ar$A[s]
  V = ar$V[s]
  x = A*y_pre
  prob = dnorm(y, mean = x, sd = V)
  if(debug)print(paste("y:",y,"x:",x,"y_pre:",y_pre,"A:",A,"V:",V))
  return(prob)
}

normalize_prob <- function(prob_sn, MIN_PROB){
  # prob is a list of small number?
  prob = sn.normalize(prob_sn)
  prob = prob/sum(prob)
  prob[prob<MIN_PROB] = MIN_PROB
  return(prob/sum(prob))
}
