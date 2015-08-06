# unit test for ar_hmm

source('ar_hmm.R')
source('small_number.R')
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
  checkTrue(abs(sum-20000)<200, paste('initial sample doesn\'t work',sum))
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
  e = 0.1
  x = seq(1:n)
  y = a*x + rnorm(n,mean=0,sd=e)
  Y = list(y)
  X = list(x)
  ar <- learnAR(Y,X,FALSE)
  checkTrue(abs(ar$A[[1]]-a)<0.01,
            paste("learnAR Error on a:",ar$A[[1]][2]))
  checkTrue(abs(ar$V[[1]]-e)<0.01,
            paste("learnAR Error on e:",ar$V[[1]]))
}

test.sn <- function(){
  a = 0.00034
  sn_a = sn.convert(a)
  checkTrue(sn_a[1] == 3.4, paste("sn.convert error:", sn_a[1]))
  checkTrue(sn_a[2] == -4, paste("sn.convert error:", sn_a[2]))
  b = 2.5e-300
  sn_b = sn.convert(b)
  checkTrue(abs(sn_b[1] - 2.5) < 0.001, paste("sn.convert error:", sn_b[1]))
  checkTrue(sn_b[2] == -300, paste("sn.convert error:", sn_b[2]))

  a = c(1.2, -100)
  b = sn.convert_exp(a,-50)
  checkTrue(b[2] == -50, paste("sn.convert_exp error:", b[2]))
  checkTrue(abs(b[1] - 1.2e-50) < 0.001, paste("sn.convert_exp error:", b[1]))
  
  a = 1.5e-100
  b = 0.05e-100
  s = sn.add(a,b)
  
  checkTrue(abs(s[1]-1.55)<0.001, paste("sn.add error:", s[1]))
  checkTrue(s[2]==-100, paste("sn.add error:", s[2]))
  
  a = 1.5e-100
  b = 1e-100
  m = sn.multiply(a,b)
  
  checkTrue(abs(m[1]-1.5)<0.001, paste("sn.multiply error 1:", m[1]))
  checkTrue(m[2]==-200, paste("sn.multiply error 2:", m[2]))
  
  a = c(1.5,-100)
  b = c(1,-400)
  m = sn.multiply(a,b)
  
  checkTrue(abs(m[1]-1.5)<0.001, paste("sn.multiply error 1:", m[1]))
  checkTrue(m[2]==-500, paste("sn.multiply error 2:", m[2]))
  
  prob_sn = list(
    c(1,-400),
    c(2,-400),
    c(0,-399)
  )
  prob = sn.normalize(prob_sn)
  checkTrue(prob[1]==1, paste("sn.normalize error 1:", prob[1]))
  checkTrue(prob[2]==2, paste("sn.normalize error 2:", prob[2]))
  checkTrue(prob[3]==0, paste("sn.normalize error 3:", prob[3]))
  
  a = c(1.5e-10, 20)
  b = sn.standard(a)
  checkTrue(b[1]==1.5, paste("sn.standard error 1:", b[1]))
  checkTrue(b[2]==10, paste("sn.standard error 2:", b[2]))
  
}