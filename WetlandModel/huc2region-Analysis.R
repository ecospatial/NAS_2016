setwd("WetlandModel/New")

library(bayesplot)

getCI = function(modelNo, modelName, ...){
  load(file=sprintf("Results/%s/%s.RData", modelName, modelNo))
  return(output)
}

getCIs = function(modelNo, modelName, ...){
  load(file=sprintf("Results/%s/%s.RData", modelName, modelNo))
  print(summary(output$samples))
  mcmc_areas(output$samples,...) + ggplot2::xlab("Coefficient Value") + ggplot2::ylab("Covariate")
  #return(output)
}

postExamine = function(coda_output,...){
  print(summary(coda_output$samples))
  print(coda_output$dic)
  mcmc_areas(coda_output$samples,...)
}

DICexamine = function(folderName){
  print(folderName)
  dic = read.delim(sprintf("Results/%s/DIC_%s.txt", folderName, folderName), skip = 1)
  dic = dic[order(dic$DIC),]
  dic = dic[dic$DIC <= min(dic$DIC)+2,]
  dic$sig = rep(NA, nrow(dic))
  dic$non = rep(NA, nrow(dic))
  
  for (i in 1:nrow(dic))
  {
    row = dic[i,]
    
    load(file=sprintf("Results/%s/%s.RData", folderName, row$modelNo))
    
    qts = summary(output$samples)$quantiles[,c(1,5)]
    sig = c()
    non = c()
    for (cov in row.names(qts))
    {
      signif = as.logical(qts[cov,][2]*qts[cov,][1] > 0)
      if (signif)
        sig = c(sig, cov)
      else
        non = c(non, cov)
    }
    
    dic[dic$modelNo == row$modelNo,]$sig = paste0(sig, collapse=",")
    dic[dic$modelNo == row$modelNo,]$non = paste0(non, collapse=",")
  }
  
  print(dic)
}

#########################################
#Colinearity
#########################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
} #https://www.r-bloggers.com/scatterplot-matrices-in-r/

pairs(~RSLR+NDMI+WH+TR+CS+WET+logWET, data=dat,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Correlation Matrix")

library(car)
vif(lm(WET~RSLR+WH+CS+TR+NDMI, data=dat))
summary(lm(RSLR~WH+CS, data=dat))
plot(dat$RSLR, predict(lm(RSLR~WH+CS, data=dat)), xlim=c(0,10), ylim=c(0,10))
abline(1,1)

vif(lm(WET~RSLR+TR+NDMI, data=dat))

#########################################
#Normalizing Data
#########################################

dat$RSLR_n = (dat$RSLR-mean(dat$RSLR))/sd(dat$RSLR)
dat$WH_n = (dat$WH-mean(dat$WH))/sd(dat$WH)

model97=jags.model("Models_Analysis\\norm97.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
output97=coda.samples.dic(model=model97,variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI"), n.iter=20000, thin=1)

model177=jags.model("Models_Analysis\\norm177.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
output177=coda.samples.dic(model=model177,variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI"), n.iter=20000, thin=1)

#Models, non normalized
getCIs(97,pars=c("b0","bNDMI","bRSLR[1]","bRSLR[2]"),prob=0.95)
getCIs(177,pars=c("b0","bNDMI","bRSLR[1]","bRSLR[2]","bWH[1]","bWH[2]"),prob=0.95)

#Models, normalized
postExamine(output97,pars=c("b0","bNDMI","bRSLR[1]","bRSLR[2]"),prob=0.95)
postExamine(output177,pars=c("b0","bNDMI","bRSLR[1]","bRSLR[2]","bWH[1]","bWH[2]"),prob=0.95)


#########################################
#Predictive Posteriors
#########################################

pp_model97 = jags.model("Models_Analysis\\pp_norm97.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
outputpp_97=coda.samples.dic(model=pp_model97,variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI", "predWET"), n.iter=20000, thin=1)
predWETs97 = summary(outputpp_97$samples)[[1]][-1:-6,1]

op=par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(density(predWETs97),xlim=c(-4,4),col="red",main="")
lines(density(dat$logWET),lty="solid")
lines(density(predWETs97),col="red")

plot(density(exp(predWETs97)),col="red",main="")
lines(density(dat$WET),lty="solid")
lines(density(exp(predWETs97)),col="red")
mtext("Model 97", outer = TRUE, cex = 1.5)
par(op)


pp_model177 = jags.model("Models_Analysis\\pp_norm177.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
outputpp_177=coda.samples.dic(model=pp_model177,variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI", "predWET"), n.iter=20000, thin=1)
predWETs177 = summary(outputpp_177$samples)[[1]][-1:-8,1]

op=par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(density(predWETs177),xlim=c(-4,4),col="red",main="")
lines(density(dat$logWET),lty="solid")
lines(density(predWETs177),col="red")

plot(density(exp(predWETs177)),col="red",main="")
lines(density(dat$WET),lty="solid")
lines(density(exp(predWETs177)),col="red")
mtext("Model 177", outer = TRUE, cex = 1.5)
par(op)

#bayes model prediction pvalue
pp_model177_pval = jags.model("Models_Analysis\\pp_norm177_pval.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
op_177_pval=coda.samples.dic(model=pp_model177_pval,variable.names=c("pvalue.cv", "pvalue.mean", "pvalue.fit"), n.iter=20000, thin=1)
summary(op_177_pval$samples)

#bayes model prediction r-squared
pp_model177_r2 = jags.model("Models_Analysis\\pp_norm177_r2.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
op_177_r2=coda.samples.dic(model=pp_model177_pval,variable.names=c("e.y"), n.iter=20000, thin=1)
e.y = op_177_r2$samples[[1]]
# data level summaries
rsquared.y <- 1 - mean (apply (e.y, 1, var))/var(y)
lambda.y <- 1 - var (apply (e.y, 2, mean))/mean(apply (e.y, 1, var))
# # summaries for the intercept model
# rsquared.a <- 1 - mean (apply (e.a, 1, var))/mean (apply (a, 1, var))
# lambda.a <- 1 - var (apply (e.a, 2, mean))/mean(apply (e.a, 1, var))
# # summaries for the slope model
# rsquared.b <- 1 - mean (apply (e.b, 1, var))/mean(apply (b, 1, var))
# lambda.b <- 1 - var (apply (e.b, 2, mean))/mean(apply (e.b, 1, var))

#Hotspot analysis
inlandbuff = readOGR("C:\\Users\\GCRLWuHardy\\Documents\\071416-InlandTHK\\inlandbuff.shp","inlandbuff")

dat_preds = data.frame(cbind(ORIG_FID=dat$ORIG_FID, predlogWET=as.numeric(predWETs177), NDMI=dat$NDMI))
inlandbuff@data = data.frame(merge(inlandbuff@data,dat_preds,by="ORIG_FID",all=T))
writeOGR(inlandbuff,".", "inlandbuff", driver="ESRI Shapefile")

#########################################
#Index of agreement
#########################################

#IoA = 1-[sum(|pred-o|)/sum(|pred-obar|+|o-obar|)] #Measure of match; 1 = perfect, 0 = no agreement

ioa = function(predicted, observed){
  return (1-(sum(abs(predicted-observed))/sum(abs(predicted-mean(observed))+abs(observed-mean(observed)))))
}

ioa(predWETs97, dat$logWET)
ioa(predWETs177, dat$logWET)



#########################################
#New Models Given Colinearity
#########################################

#Model 86

#P-vals
pp_model86_p = jags.model("Models_Analysis\\86\\pp_norm86_p_r2.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
op_86_p=coda.samples.dic(model=pp_model86_p,variable.names=c("pvalue.cv", "pvalue.mean", "pvalue.fit"), n.iter=20000, thin=1)
summary(op_86_p$samples)

#bayes model prediction r-squared
pp_model86_r2 = jags.model("Models_Analysis\\86\\pp_norm86_p_r2.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
op_86_r2=coda.samples.dic(model=pp_model86_r2,variable.names=c("e.y"), n.iter=20000, thin=1)
e.y = op_86_r2$samples[[1]]
# data level summaries
rsquared.y <- 1 - mean (apply (e.y, 1, var))/var(dat$logWET)
lambda.y <- 1 - var (apply (e.y, 2, mean))/mean(apply (e.y, 1, var))

#IoA
pp_model86_ioa = jags.model("Models_Analysis\\86\\pp_norm86.txt", data=append(data,dat), n.chains=3, n.adapt=2000)
op_86_ioa=coda.samples.dic(model=pp_model86_ioa,variable.names=c("predWET"), n.iter=20000, thin=1)
predWETs86 = summary(op_86_ioa$samples)[[1]][-1:-6,1]

#Pred posterior

layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4, 1))
par(mai=rep(0.4, 4))
plot(density(predWETs86),xlim=c(-4,4),col="white", main="(a)", cex.main=0.8, xlab = "log wetland loss (log-ha)")
title("(a)", cex.main=0.8)
lines(density(dat$logWET))
lines(density(predWETs86),col="red",lty="dashed")

plot(density(exp(predWETs86)),col="white",main="(b)", cex.main=0.8, xlab = "Wetland loss (ha)")
lines(density(dat$WET))
lines(density(exp(predWETs86)),col="red",lty="dashed")
mtext("Model 86 Predictive Posterior", outer = TRUE, cex = 1.5)
plot.new()
legend(x="center", ncol=2,legend=c("Data","P. Posterior"), col=c("black","red"), lty=c("solid","dashed"))
par(mai=c(0,0,0,0))

#Model 88
postExamine(getCI(88), par=c("bRSLR[1]","bRSLR[2]","bNDMI[1]","bNDMI[2]"),prob=0.95)
#Model 85
postExamine(getCI(85), par=c("bRSLR","bNDMI","bTR[1]","bTR[2]"),prob=0.95)
#Model 87
postExamine(getCI(87), par=c("bTR","bNDMI","bRSLR[1]","bRSLR[2]"),prob=0.95)
#Model 90
postExamine(getCI(90), par=c("bRSLR[1]","bRSLR[2]","bNDMI[1]","bNDMI[2]","bTR[1]","bTR[2]"),prob=0.95)


fullSignif = function(quants){
  if(!is.numeric(quants))
    return("Fixed cov")
  return(quants[1]*quants[2] > 0)
}
signifRegion = function(modelNo, covar){
  covarReg1 = sprintf("b%s.1.",covar)
  covarReg2 = sprintf("b%s.2.",covar)
  fullDat = data.frame(do.call(rbind, getCI(modelNo)$samples))
  if(is.null(fullDat[[covarReg2]]))
    return("Fixed cov")
  
  return(comparePost(fullDat[[covarReg2]],
                     fullDat[[covarReg1]]
  ))
}
comparePost <- function(x,y,prob=0.95){
  lo = (1-prob)/2
  hi <- 1-lo
  diff <- x-y
  quantile(diff,probs=c(lo,hi))
}

getCIs(88)
isSignif(signifRegion(88,"RSLR"))
isSignif(signifRegion(86,"TR"))
isSignif(signifRegion(88,"NDMI"))
isSignif(signifRegion(86,"NDMI"))

mean(dat$TR[dat$region==1])
mean(dat$TR[dat$region==2])
quantile(dat$TR[dat$region==1], prob=c(0.025,0.975))
quantile(dat$TR[dat$region==2], prob=c(0.025,0.975))

plot(density(dat$TR), ylim=c(0,16))
lines(density(dat$TR[dat$region==1]),col="red")
lines(density(dat$TR[dat$region==2]),col="blue")

plot(logWET~TR,data=dat)
points(logWET~TR,data=dat[dat$region==1,],col="red")
points(logWET~TR,data=dat[dat$region==2,],col="blue")
abline(lm(logWET~TR,data=dat[dat$region==1,]),col="red")
abline(lm(logWET~TR,data=dat[dat$region==2,]),col="blue")

plot(density(dat$CS), ylim=c(0,50))
lines(density(dat$CS[dat$region==1]),col="red")
lines(density(dat$CS[dat$region==2]),col="blue")

library(rgl)
plot3d(dat$logWET,dat$CS,dat$TR)

summary(lm(logWET~TR*CS,data=dat))


#########################################
# Restoration Evaluation
#########################################
#ORIG_FID list for each baseline
raccBaseline = c(415, 420, 407, 433, 484, 430, 442, 445, 457)
trinityBaseline = raccBaseline
gisleBaseline = c(422, 438, 454, 510, 535, 538, 558, 583)
#ORIG_FID list for restoration
raccRestore = c(462, 447, 465)
trinityRestore = c(404, 409, 413)
gisleRestore = c(480, 487)

raccAvgNDMI = mean(dat$NDMI[dat$ORIG_FID %in% raccRestore]) - mean(dat$NDMI[dat$ORIG_FID %in% raccBaseline])
trinityAvgNDMI = mean(dat$NDMI[dat$ORIG_FID %in% trinityRestore]) - mean(dat$NDMI[dat$ORIG_FID %in% trinityBaseline])
gisleAvgNDMI = mean(dat$NDMI[dat$ORIG_FID %in% gisleRestore]) - mean(dat$NDMI[dat$ORIG_FID %in% gisleBaseline])

raccAvgNDMI
trinityAvgNDMI
gisleAvgNDMI

bwNDMI = mean(c(raccAvgNDMI, gisleAvgNDMI))
vegNDMI = mean(c(trinityAvgNDMI))

#########################################
#SLR Scenario Predictions
#########################################
#AD2100, RCP3
# quantileVals=c(0.25,0.40,0.60,0.70)
# quantiles=c(5,17,83,95)
# #AD2100, RCP8.5
# quantileVals=c(0.50,0.70,1.20,1.50)
# quantiles=c(5,17,83,95)
# #AD2300, RCP3
# quantileVals=c(0.50,0.60,1.00,1.20)
# quantiles=c(5,17,83,95)
# #AD2300, RCP8.5
quantileVals=c(1.30,2.00,3.00,4.00)
quantiles=c(5,17,83,95)

# horton = read.delim("HortonScenarios.txt")

p1=quantiles[1]/100
p2=quantiles[4]/100
x1=quantileVals[1]
x2=quantileVals[4]
sd = (x2 - x1) / (qnorm(p2) - qnorm(p1))
mu = x1 - qnorm(p1)*sd

pnorm(q1,mu,sd)
pnorm(q2,mu,sd)

scen=rnorm(20000,mu,sd)
quantile(scen,c(quantiles/100))
a=quantileVals
names(a)=paste(quantiles,"%",sep="")
a




#From Wu, Clark, Vose 2012
# quantiles=c(0.05,0.95)
# quantileVals=c(0.25,0.70)
# mu=sum(quantiles*quantileVals)/sum(quantiles)
# sd=(sum(quantiles*(quantileVals^2))/sum(quantiles)-mutwo^2)^2
# 
# scen=rnorm(20000,mu,sd)
# quantile(scen,c(quantiles/100))
# a=quantileVals
# names(a)=paste(quantiles,"%",sep="")
# a
# 
# #Simulated data experiment. Equation from paper not working!
# mu=7
# sd=2
# 
# pnorm(10.2,mu,sd)
# 
# a=rnorm(100000,mu,sd)
# 
# p1=0.05
# p2=0.95
# x1=quantile(a,p1)
# x2=quantile(a,p2)
# 
# mu_mm = (p1*x1+p2*x2)/(p1+p2)
# v_mm = (p1*x1^2+p2*x2^2)/(p1+p2)-mu_mm^2
# sd_mm=v_mm^2
# 
# mu_mm
# sd_mm
# 
# b=rnorm(100000,mu_mm,sd_mm)
# 
# plot(density(a))
# lines(density(b),col="red")

