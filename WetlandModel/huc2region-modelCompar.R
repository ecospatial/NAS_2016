
library(rjags)
library(sp)
library(magrittr)
library(rgdal)
source("../RUtilityFunctions/codaSamplesDIC.R")
source("../RUtilityFunctions/combinatorics.R")

library(RPostgreSQL)
library(postGIStools)


# Database Connection and Loading -----------------------------------------
source("../DecisionSuppTool/mysqlcfg.R")
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = "postgiz", user = user,
                   host = "52.14.87.100", port = 5432,
                   password = pw)
  rm(pw);rm(user)
}


# TODO: Merge can likely be done on the database side
wetloss = get_postgis_query(con, 'SELECT * FROM wetloss')
inlandbuff = get_postgis_query(con, "SELECT * FROM thkbuffers", geom_name = "geom")
inlandbuff@data = merge(inlandbuff@data, wetloss, by = "ORIG_FID")

dbDisconnect(con2)


# Combine Spatial. Geo, and Wet Data --------------------------------------
huc2AvgDis = readOGR("huc2AvgDis.shp", "huc2AvgDis")
huc2AvgDis = spTransform(huc2AvgDis, proj4string(inlandbuff))

hucZone=over(inlandbuff,huc2AvgDis[,"HUC2"]) #3=FL, 12=TX, 8=LA
hucZone$ORIG_FID=seq(0:(nrow(hucZone)-1))

dat=read.delim("../wetlandLossData.txt")
dat = merge(dat, hucZone, by="ORIG_FID")
dat$region = rep(2,nrow(dat))
dat[dat$HUC2 == "03" | dat$HUC2 == "12",]$region = 1

dat$WET = dat$WET*900/10000
dat$logWET = log(dat$WET)

write.table(dat[c("ORIG_FID",'WET','CS','WH','TR','RSLR','NDMI','DIS','logWET','region')],"mysqldata0517.csv", row.names=F, quote=F, sep=",", col.names=F)
write.table(dat,"data0417.txt", row.names=F, quote=F, sep="\t")

data=list(Nobs=nrow(dat), Nregion=2)


# Create All Combinations of Models from Params ---------------------------
createModel = function(fixed, random){
  fixed=na.omit(fixed)
  random=na.omit(random)
  
  fixedPriors = ""
  randomPriors = ""
  linearEq = ""
  
  if(length(fixed) > 0){
    linearEq = sprintf("%s + %s",linearEq,paste(sprintf("b%s*%s[i]",fixed,fixed),sep="",collapse=" + "))
    fixedPriors = paste(sprintf("b%s ~ dnorm(0,0.00001)",fixed),sep="",collapse="\n")
  }
  
  if(length(random) > 0){
    linearEq = sprintf("%s + %s",linearEq,paste(sprintf("b%s[region[i]]*%s[i]",random,random),sep="",collapse=" + "))
    randomPriors = sprintf("for(j in 1:Nregion) {\n%s\n    }", paste("      b",random,"[j] ~ dnorm(0,0.00001)",sep="",collapse="\n"))
  }
  
  modelString = sprintf(
"model {
    for (i in 1:Nobs) {
      v.mu[i] <- b0 + %s #Linear Model
      logWET[i] ~ dnorm(v.mu[i], v.tau)
    }
    
    b0 ~ dnorm(0,0.00001)

    %s #Fixed Effect Priors

    %s #Random Effect Priors

    v.tau ~ dgamma(1,1)
    v.sigma <- 1/sqrt(v.tau)
}", linearEq, fixedPriors, randomPriors)
  
  return(modelString)
}

#Creates models; already done - commented out
# params=c("RSLR","WH","TR","CS","NDMI")
# models = getModels(params)
# for(i in 1:nrow(models)){
#   model = models[i,]
#   modelTxt=createModel(fixed=model[1:(ncol(models)/2)],
#               random=model[(ncol(models)/2+1):ncol(models)])
#   write(modelTxt,sprintf("Models\\%s.txt",i))
# }


# Run Each Model in JAGS --------------------------------------------------
write.table("modelNo\tfixed\trandom\tDIC","Results\\DIC.txt", row.names=F, quote=F, sep="\t")
for(i in 1:nrow(models)){
  model=jags.model(sprintf("Models\\%s.txt",i), data=append(data,dat), n.chains=3, n.adapt=2000)
  
  output=coda.samples.dic(model=model,variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI"), n.iter=20000, thin=1)
  summary(output$samples)
  output$dic
  
  fixed = paste(na.omit(models[i,1:length(params)]),collapse=",")
  random = paste(na.omit(models[i,(length(params)+1):(length(params)*2)]),collapse=",")
  
  write(sprintf("%s\t%s\t%s\t%s",i,fixed,random,output$dic$deviance+output$dic$penalty),
        "Results\\DIC.txt",append=T)
  save(output,file=sprintf("Results\\%s.RData",i))
}
