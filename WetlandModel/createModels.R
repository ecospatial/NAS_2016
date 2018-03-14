source("../RUtilityFunctions/combinatorics.R")

# Create All Combinations of Models from Params ---------------------------
createModel = function(response, fixed, random){
  fixed=na.omit(fixed)
  random=na.omit(random)
  
  fixedPriors = ""
  randomPriors = ""
  linearEq = ""
  
  if(length(fixed) > 0){
    linearEq = sprintf("%s + %s", linearEq, paste(sprintf("b%s * %s[i]",fixed,fixed),sep="",collapse=" + "))
    fixedPriors = paste(sprintf("b%s ~ dnorm(0,0.00001)",fixed),sep="",collapse="\n")
  }
  
  if(length(random) > 0){
    linearEq = sprintf("%s + %s", linearEq, paste(sprintf("b%s[region[i]]*%s[i]",random,random),sep="",collapse=" + "))
    randomPriors = sprintf("for(j in 1:Nregion) {\n%s\n    }", paste("      b",random,"[j] ~ dnorm(0,0.00001)",sep="",collapse="\n"))
  }
  
  modelString = sprintf(
    "model {
    for (i in 1:Nobs) {
    %s.mu[i] <- b0%s #Linear Model
    %s[i] ~ dnorm(%s.mu[i], tau)
    }
    
    b0 ~ dnorm(0,0.00001)
    
    %s #Fixed Effect Priors
    
    %s #Random Effect Priors
    
    tau ~ dgamma(1,1)
    sigma <- 1/sqrt(tau)
}", response, linearEq, response, response, fixedPriors, randomPriors)
  
  return(modelString)
}

createModels = function(response, params, folderName)
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
                             random=model[(ncol(models)/2+1):ncol(models)])
        write(modelTxt, fileName)
      }
    }
  }
  
  return(getModels(params))
}

