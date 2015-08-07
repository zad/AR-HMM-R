# small number operations in R
sn.is.sn <- function(a){
  if(length(a) == 1)
    return(FALSE)
  else
    return(TRUE)
}

sn.convert <- function(a){
  if(a==0)
    return(c(0,0))
  exp = floor(log10(a))
  sig = a/10^exp
  return(c(sig,exp))
}

sn.convert_exp <- function(a,exp){
  exp_diff = exp - a[2]
  sig = a[1]/10^exp_diff
  return(c(sig,exp))
}

sn.standard <- function(a){
  exp_addi = floor(log10(a[1]))
  sig = a[1]/10^exp_addi
  exp = a[2] + exp_addi
  return(c(sig,exp))
}

sn._add <- function(a,b){
  if(a[2]>b[2]){
    b = sn.convert_exp(b,a[2])
  }else if(a[2]<b[2])
    a = sn.convert_exp(a,b[2])
  sig = a[1] + b[1]
  return(c(sig,a[2]))
}

sn.add <- function(a, b){
  if(!sn.is.sn(a)){
    a = sn.convert(a)
  }
  if(!sn.is.sn(b)){
    b = sn.convert(b)
  }
  if(a[1]==0) return(b)
  if(b[1]==0) return(a)
  return(sn.standard(sn._add(a,b)))
  
}

sn.multiply <- function(a, b){
  if(!sn.is.sn(a)){
    a = sn.convert(a)
  }
  if(!sn.is.sn(b)){
    b = sn.convert(b)
  }
  if(a[1]==0 || b[1]==0)
    return(c(0,0))
  sig = a[1]*b[1]
  exp = a[2]+b[2]
  return(sn.standard(c(sig,exp)))
}

sn.normalize <- function(sn_list){
  exp = c()
  i = 1
  for(sn in sn_list){
    if(sn[1]!=0)
    {
      exp[i] = sn[2]
      i = i+1
    } 
  }
#   if(i==1){
#     print("sn.normalize warning:all inputs are zeros")
#     return(rep(1,length(sn_list)))
#   }
  max_exp = max(exp)
  normalized = c()
  for(i in 1:length(sn_list)){
    sn = sn_list[[i]]
    if(sn[1] == 0)
      normalized[i] = 0
    else{
      if(sn[2]!=max_exp)
        sn = sn.convert_exp(sn_list[[i]],max_exp)
      normalized[i] = sn[1]  
    }
  }
  return(normalized)
}

