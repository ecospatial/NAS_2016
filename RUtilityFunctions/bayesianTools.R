library(bayesplot)
library(coda)

getCIs = function(modelNo, modelName, prob=0.95, ...){
  load(file=sprintf("Results/%s/%s.RData", modelName, modelNo))
  print(summary(output$samples))
  mcmc_areas(output$samples, prob = prob, ...) + ggplot2::xlab("Coefficient Value") + ggplot2::ylab("Covariate")
  #return(output)
}

DICexamine = function(modelName, omit=NA, all=F, top=NULL){
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