# unit test for ar_hmm

source('ar_hmm.R')
# test mc.sample

test.mc.sample <- function(){
  S = 3
  TM = t(matrix(
    c(0.5, 0.1, 0.4, 
      0.5, 0.2, 0.3, 
      0.2, 0.3, 0.5),
    S,S
  ))
  sum = 0
  # test initial samples
  for(i in 1:10000)
  {
    sum = sum + mc.sample(TM, -1)
  }
  print(sum)
  checkTrue(abs(sum-20000)<100, paste('initial sample doesn\'t work',sum))
  # test sequential samples
  count <- c(0,0,0)
  num = 10000
  for(i in 1:num){
    s <- mc.sample(TM,1)
    count[s] = count[s] + 1
  }
  prob = count[1]*1.0/num
  checkTrue(abs(prob - 0.5) < 0.01, 
            paste("sequential samples doesn\'t work well"
                  ,paste(TM[1,],collapse=" ")
                  ,paste(count,collapse=" ")
            ))
}

test.learnTM <- function(){
  S = 3
  TM_C = t(matrix(
    c(1500, 1000, 500, 
      300, 600, 300, 
      200, 400, 600),
    S,S
  ))  
  pi <- learnTM(TM_C)
  checkTrue(abs(pi[1,1] - 0.5) < 1e-10,
            paste("learnTM Error:",pi[1,1])
            )
  checkTrue(abs(pi[1,2] - 1/3) < 1e-10,
            paste("learnTM Error:",pi[1,2])
  )
  checkTrue(abs(pi[2,2] - 0.5) < 1e-10,
            paste("learnTM Error:",pi[1,2])
  )
  checkTrue(abs(pi[3,2] - 1/3) < 1e-10,
            paste("learnTM Error:",pi[1,2])
  )
}

test.learnAR <- function(){
  n = 100
  a = 2
  b = 1
  e = 0.1
  x = seq(1:n)
  y = a*x + rnorm(n,mean=b,sd=e)
  Y = list(y)
  X = list(x)
  ar <- learnAR(Y,X)
  checkTrue(abs(ar$A[[1]][2]-a)<0.01,
            paste("learnAR Error on a:",ar$A[[1]][2]))
  checkTrue(abs(ar$A[[1]][1]-b)<0.01,
            paste("learnAR Error on b:",ar$A[[1]][1]))
  checkTrue(abs(ar$V[[1]]-e)<0.01,
            paste("learnAR Error on e:",ar$V[[1]]))
}