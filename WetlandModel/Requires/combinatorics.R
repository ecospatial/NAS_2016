p=function(...,sep="",collapse=""){
  paste(...,sep=sep,collapse=collapse)
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

binComb = binaryCombinations = function(n){
  final = matrix(nrow=2^n,ncol=n)
  for (i in 0:((2^n)-1)) {
    final[i+1,] = number2binary(i, n)
  }
  return(final)
}

getModels = function(set){
  N = length(set)
  result = matrix(c(paste(1:N,"F",sep=""),paste(1:N,"R",sep="")), ncol=N*2)
  for(n in 1:N) {
    tfMatrix = binComb(n)
    combos = t(combn(set,n))
    for(yc in 1:nrow(combos)){ #for each combination of vars (AB, BC, AC)
      comb = combos[yc,]
      for(ytf in 1:nrow(tfMatrix)){ #for each T/F binComb (each model)
        model = matrix(nrow=1, ncol=N*2)
        fixeds = 1
        randoms = 1
        for(xc in 1:length(comb)){ #for each letter
          tf = tfMatrix[ytf,xc]
          a = comb[xc]
          if(!tf){
            model[fixeds] = a
            fixeds = fixeds + 1
          } else{
            model[N+randoms] = a
            randoms = randoms + 1
          }
        }
        result = rbind(result, model)
      }
    }
  }
  colnames(result)=result[1,]
  result=result[-1,]
  return(result)
}


# 
# 
# set=c("A","B","C")
# 
# getCombs = function(set){
#   matches=c()
#   end=length(set)
#   
#   for (i in 1:end){
#     if(i+1<=end){
#       for (j in (i+1):end){
#         x=p(set[i], set[j])
#         matches=matches%>%rbind(x)
#         x=p(p(set[i],"+"), set[j])
#         matches=matches%>%rbind(x)
#         x=p(p(set[i],"+"), p(set[j],"+"))
#         matches=matches%>%rbind(x)
#       }
#     }
#     if(i-1>=1){
#       for (j in (i-1):1){
#         x=p(p(set[j],"+"), set[i])
#         matches=matches%>%rbind(x)
#       }
#     }
#   }
#   
#   return(list(matches=matches,n=nrow(matches)))
# }
# 
# getCombs(set)
# 
# getCombs2 = function(set){
#   matches=c()
#   end=length(set)
#   start=1
#   
#   for (i in 1:end){
#     if(i+1<=end){
#       for (j in (i+1):end){
#         x=p(set[i], set[j])
#         matches=matches%>%rbind(x)
#         x=p(p(set[i],"+"), set[j])
#         matches=matches%>%rbind(x)
#         x=p(p(set[i],"+"), p(set[j],"+"))
#         matches=matches%>%rbind(x)
#       }
#     }
#     if(i-1>=1){
#       for (j in (i-1):1){
#         x=p(p(set[j],"+"), set[i])
#         matches=matches%>%rbind(x)
#       }
#     }
#   }
#   
#   
#   return(list(matches=matches,n=nrow(matches)))
# }
# 
# getCombs2(set)
# 
# 
# 

