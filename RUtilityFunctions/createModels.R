# Create All Combinations of Models from Params ---------------------------
createModel = function(response, fixed, random, randomIntercept = F, properRandomEffects = F){
  additiveTerms = c()
  fixedPriors = c()
  randomPriors = c()
  hyperPriors = c()
  fixedPriorsDef = ""
  randomPriorsDef = ""
  hyperPriorsDef = ""
  
  fixed=na.omit(fixed)
  random=na.omit(random)
  
  if (randomIntercept) {
    additiveTerms = c(additiveTerms, "b0[region[i]]")
    if (properRandomEffects)
    {
      randomPriors = c(randomPriors, "b0[j] ~ dnorm(b0.mu,b0.tau)")
      hyperPriors = c(hyperPriors, "b0.mu ~ dnorm(0,0.00001)")
      hyperPriors = c(hyperPriors, "b0.tau ~ dgamma(1,1)")
    } else {
      randomPriors = c(randomPriors, "b0[j] ~ dnorm(0,0.00001)")
    }
  } else {
    additiveTerms = c(additiveTerms, "b0")
    fixedPriors = c(fixedPriors, "b0 ~ dnorm(0,0.00001)")
  }
  
  additiveTerms = c(additiveTerms, sprintf("b%s[region[i]] * %s[i]", random, random))
  if (properRandomEffects)
  {
    randomPriors = c(randomPriors, sprintf("b%s[j] ~ dnorm(%s.mu,%s.tau)", random, random, random, random))
  } else {
    randomPriors = c(randomPriors, sprintf("b%s[j] ~ dnorm(0,0.00001)", random))
  }
  if (length(randomPriors) > 0)
  {
    randomPriorsDef = sprintf("for(j in 1:Nregion) {\n        %s\n    }", paste0(randomPriors, collapse = "\n        "))
  }
  
  hyperPriors = c(hyperPriors, sprintf("%s.mu ~ dnorm(0,0.00001)\n    %s.tau ~ dgamma(1,1)", random, random))
  if (properRandomEffects)
  {
    hyperPriorsDef = paste0(hyperPriors, collapse="\n    ")
  }

  additiveTerms = c(additiveTerms, sprintf("b%s * %s[i]", fixed, fixed))
  fixedPriors = c(fixedPriors, sprintf("b%s ~ dnorm(0,0.00001)",fixed))
  fixedPriorsDef = paste0(fixedPriors, collapse = "\n    ")
  
  linearEq = paste0(additiveTerms, collapse = " + ")
  
  modelString = sprintf(
    "model {
    for (i in 1:Nobs) {
        %s.mu[i] <- %s #Linear Model
        %s[i] ~ dnorm(%s.mu[i], %s.tau) #Response distribution
    }

    #Random Effect Priors
    %s

    #Hyper Priors
    %s 
    
    #Fixed Effect Priors
    %s
    
    %s.tau ~ dgamma(1,1)
}", response, linearEq, response, response, response, randomPriorsDef, hyperPriorsDef, fixedPriorsDef, response)
  
  return(modelString)
}

createModels = function(response, params, randomIntercept, folderName, properRandomEffects = F)
{
  if (!dir.exists("Models/"))
  {
    dir.create("Models")
  }
  
  modelDir = paste0("Models/", folderName)
  
  if (!dir.exists(modelDir) || length(dir(modelDir)) != nrow(getModels(params)))
  {
    dir.create(modelDir, showWarnings = FALSE)
    models = getModels(params)
    for(i in 1:nrow(models)){
      fileName = sprintf("%s/%s.txt", modelDir, i)
      if (!file.exists(fileName))
      {
        model = models[i,]
        modelTxt=createModel(response=response,
                             fixed=model[1:(ncol(models)/2)],
                             random=model[(ncol(models)/2+1):ncol(models)],
                             randomIntercept = randomIntercept,
                             properRandomEffects = properRandomEffects)
        write(modelTxt, fileName)
      }
    }
  }
  
  return(getModels(params))
}

# Combinatorics -----------------------------------------------------------
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



