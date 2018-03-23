source("../../RUtilityFunctions/combinatorics.R")

# Create All Combinations of Models from Params ---------------------------
createModel = function(response, fixed, random, randomIntercept = F){
  additiveTerms = c()
  fixedPriors = c()
  randomPriors = c()
  hyperPriors = c()
  fixedPriorsDef = ""
  randomPriorsDef = ""
  
  fixed=na.omit(fixed)
  random=na.omit(random)
  
  if (randomIntercept) {
    additiveTerms = c(additiveTerms, "b0[region[i]]")
    randomPriors = c(randomPriors, "b0[j] ~ dnorm(b0.mu,b0.tau)")
    hyperPriors = c(hyperPriors, "b0.mu ~ dnorm(0,0.00001)")
    hyperPriors = c(hyperPriors, "b0.tau ~ dgamma(1,1)")
  } else {
    additiveTerms = c(additiveTerms, "b0")
    fixedPriors = c(fixedPriors, "b0 ~ dnorm(0,0.00001)")
  }
  
  additiveTerms = c(additiveTerms, sprintf("b%s[region[i]] * %s[i]", random, random))
  randomPriors = c(randomPriors, sprintf("b%s[j] ~ dnorm(%s.mu,%s.tau)", random, random, random, random))
  randomPriorsDef = sprintf("for(j in 1:Nregion) {\n        %s\n    }", paste0(randomPriors, collapse = "\n        "))
  
  hyperPriors = c(hyperPriors, sprintf("%s.mu ~ dnorm(0,0.00001)\n    %s.tau ~ dgamma(1,1)", random, random))
  hyperPriorsDef = paste0(hyperPriors, collapse="\n    ")

  additiveTerms = c(additiveTerms, sprintf("b%s * %s[i]", fixed, fixed))
  fixedPriors = paste(sprintf("b%s ~ dnorm(0,0.00001)",fixed),sep="",collapse="\n")
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

createModels = function(response, params, randomIntercept, folderName)
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
                             randomIntercept = randomIntercept)
        write(modelTxt, fileName)
      }
    }
  }
  
  return(getModels(params))
}

