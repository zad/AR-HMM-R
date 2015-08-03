# ar_hmm 
# input: a list of time series (y)
ar_hmm <- function(){
  print("ar_hmm")
}

# generate time series based on input parameters
genTS(N,S,pi,A,V){
  
}

# number of time series
NT = 10
# length of a time series
N = 100
# generate dummy data
# number of states
S = 3
# transition matrix
pi <- matrix(
  c(0.5, 0.1, 0.4, 0.5, 0.2, 0.3, 0.2, 0.3, 0.5),
  3,3
)

# AR parameters
A = c(-1, 0, 2)
V = c(0.1, 0.1, 0.2)

listTS <- list()
for(i in 1:NT){
  print(paste0("generate time series:",i))
  listTS(i) <- genTS(N,S,pi,A,V)
}

ar_hmm()