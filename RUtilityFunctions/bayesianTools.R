library(bayesplot)
library(coda)

getCIs = function(modelNo, modelName, prob=0.95, ...){
  load(file=sprintf("Results/%s/%s.RData", modelName, modelNo))
  print(summary(output$samples))
  mcmc_areas(output$samples, prob = prob, ...) + ggplot2::xlab("Coefficient Value") + ggplot2::ylab("Covariate")
  #return(output)
}

getCI = function(modelNo, modelName = NULL){
  if (is.null(modelName))
  {
    load(file=sprintf("Results/%s.RData", modelNo))
    return(output)
  }
  load(file=sprintf("Results/%s/%s.RData", modelName, modelNo))
  return(output)
}

rescrapeDIC = function(modelName)
{
  resultsDir = sprintf("Results/%s/", modelName)
  write.table("modelNo\tfixed\trandom\tDIC", sprintf("%s/DIC_%s.txt", resultsDir, modelName), row.names=F, quote=F, sep="\t")
  files = list.files(resultsDir, pattern = ".*\\.RData", full.names = TRUE)
  
  for (modelFile in files)
  {
    i = as.numeric(gsub("(\\d*)\\.RData", "\\1", basename(modelFile)))
    
    load(modelFile)
    
    allParams = colnames(output$samples[[1]])
    
    areRandom = grep(".*\\[.*\\]", allParams)
    random = unique(gsub("(.*)\\[.*\\]", "\\1", allParams[areRandom]))
    fixed = allParams[-areRandom]
    
    random = paste(random, collapse=",")
    fixed = paste(fixed, collapse=",")
    
    write(sprintf("%s\t%s\t%s\t%s", i, fixed, random, output$dic$deviance + output$dic$penalty),
          file = sprintf("%s/DIC_%s.txt", resultsDir, folderName),
          append = T)
  }
}

signifExamine = function(modelName, top = 10, all = F, omit=NA) 
{
  dic = read.delim(sprintf("Results/%s/DIC_%s.txt", modelName, modelName), skip = 1)
  dic = dic[order(dic$DIC),]
  
  if (!is.na(omit))
  {
    for (o in omit)
    {
      dic = dic[!(grepl(o, dic$fixed) | grepl(o, dic$random)),]
    }
  }

  if(all)
  {
    dic = dic[dic$DIC <= min(dic$DIC)+2,]
  } else {
    dic = head(dic, top)
  }
  
  dic$sig = rep(NA, nrow(dic))
  dic$non = rep(NA, nrow(dic))
  
  for (i in 1:nrow(dic))
  {
    row = dic[i,]
    
    load(file=sprintf("Results/%s/%s.RData", modelName, row$modelNo))
    
    qts = as.data.frame(summary(output$samples)$quantiles[,c(1,5)])
    
    qts$sig = qts$`2.5%` * qts$`97.5%` > 0
    qts$sign = qts$`97.5%` > 0
    qts$param = gsub("(.*?)(?:\\[\\d*\\]|$)", "\\1", row.names(qts))
    qts$num = gsub("(.*?)(\\[\\d*\\]|$)", "\\2", row.names(qts))
    qts$num = gsub("\\[(\\d*)\\]", "\\1", qts$num)
    qts$num = as.numeric(qts$num)
    qts$num[is.na(qts$num)] = 0
    
    sig = c()
    non = c()
    nums = unique(qts$num)[!is.na(unique(qts$num))]
    for (p in unique(qts$param))
    {
      sigs = qts[qts$param == p & qts$sig,]
      if (nrow(sigs) > 0)
      {
        sigNums = sigs$num
        signs = sigs$sign
        if (all(signs == T)){
          sign = "+"
        }else if (all(signs == F)){
          sign = "-"
        }else{
          sign = ""
          sigNums = paste0(sigNums, sapply(signs, function(x){if (x == T) return ("+") else return("-")}))
        }
        
        if (length(sigNums) == length(nums))
        {
          sigNums = "all"
        }
        if (length(sigNums == 1) & any(is.na(sigNums)))
        {
          sigNums = ""
        } else {
          sigNums = sprintf("[%s]", paste0(sigNums, collapse=","))
        }
        sig = c(sig, paste0(p, sigNums, sign))
      }
      
      nons = qts[qts$param == p & !qts$sig,]
      if (nrow(nons) > 0)
      {
        nonNums = qts[qts$param == p & !qts$sig,]$num
        
        if (length(nonNums) == length(nums))
        {
          nonNums = "all"
        }
        
        if (length(nonNums == 1) & any(is.na(nonNums)))
        {
          nonNums = ""
        } else {
          nonNums = sprintf("[%s]", paste0(nonNums, collapse=","))
        }
        non = c(non, paste0(p, paste0(nonNums, collapse=",")))
      }
    }
    
    dic[dic$modelNo == row$modelNo,]$sig = paste0(sig, collapse=",")
    dic[dic$modelNo == row$modelNo,]$non = paste0(non, collapse=",")
  }
  
  return(dic)
}

DICexamine = function(modelName, omit=NA, all=F, top=NULL, noSig = F){
  dic = read.delim(sprintf("Results/%s/DIC_%s.txt", modelName, modelName), skip = 1)
  print(modelName)
  print(paste0("DIC minimum: ", min(dic$DIC)))
  dic = dic[order(dic$DIC),]
  
  if (!is.na(omit))
  {
    for (o in omit)
    {
      dic = dic[!(grepl(o, dic$fixed) | grepl(o, dic$random)),]
    }
  }
  
  if(!all & is.null(top))
    dic = dic[dic$DIC <= min(dic$DIC)+2,]
  else if(!is.null(top))
    dic = head(dic, top)
  
  if (noSig)
  {
    print(dic)
    return(dic)
  }
  
  dic$sig = rep(NA, nrow(dic))
  dic$non = rep(NA, nrow(dic))
  
  for (i in 1:nrow(dic))
  {
    row = dic[i,]
    
    load(file=sprintf("Results/%s/%s.RData", modelName, row$modelNo))
    
    qts = summary(output$samples)$quantiles[,c(1,5)]
    sig = c()
    non = c()
    for (cov in row.names(qts))
    {
      signif = as.logical(qts[cov,][2]*qts[cov,][1] > 0)
      if (signif)
      {
        if (qts[cov,][2] > 0)
          cov = paste0(cov, "+")
        else
          cov = paste0(cov, "-")
        sig = c(sig, cov)
      }
      else
      {
        non = c(non, cov)
      }
    }
    
    dic[dic$modelNo == row$modelNo,]$sig = paste0(sig, collapse=",")
    dic[dic$modelNo == row$modelNo,]$non = paste0(non, collapse=",")
  }
  
  print(dic)
  return(dic)
}

# panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
# {
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- abs(cor(x, y))
#   txt <- format(c(r, 0.123456789), digits=digits)[1]
#   txt <- paste(prefix, txt, sep="")
#   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# } #https://www.r-bloggers.com/scatterplot-matrices-in-r/
# 
# pairs(~RSLR+NDVI+WH+TR+CS+logPCT, data=dat,
#       lower.panel=panel.smooth, upper.panel=panel.cor, 
#       pch=20, main="Correlation Matrix")
# 
# library(car)
# vif(lm(WET~RSLR+WH+CS+TR+NDMI, data=dat))
# summary(lm(RSLR~WH+CS, data=dat))
# plot(dat$RSLR, predict(lm(RSLR~WH+CS, data=dat)), xlim=c(0,10), ylim=c(0,10))
# abline(1,1)
# 
# vif(lm(WET~RSLR+TR+NDMI, data=dat))