library(tidyr)
library(dplyr)
library(rvest)
library(lubridate)
library(dplyr)
library(rjags)
library(bayesplot)


# Set Focus ---------------------------------------------------------------

#Set to "all" to evaluate all studies, or "cw" for only coastal wetlands
esvType = "all"

# Data Prep ---------------------------------------------------------------

esvDat = read.delim("TEEB2.txt")

journalFmt = grep("\\d.*(\\(\\d\\))?: \\d", esvDat$Title, perl=TRUE) #finds 'Eco. Econ. 14(1): 12-100' OR 'Eco. Econ. 14: 12-100'
journalTxt = grep("[jJ]ournal", esvDat$Title, perl=TRUE) #finds Journal or journal in title

esvDat$PeerReviewed = rep(1, nrow(esvDat))
esvDat$PeerReviewed[journalFmt] = 2

switch(esvType,
       all={
         #All biomes
         esvDat = esvDat %>%
           filter(Valuation.Method != 'Benefit Transfer') %>%
           # filter(!Continent %in% c("Various", "World")) %>%
           #Generate country codes for conversion to USD2007; cleanup as well
           mutate(ISO = substring(Unit,1,3)) %>%
           mutate(ISO = sub('\\$','D', ISO)) %>%
           mutate(ISO = sub('Rie','KHR', ISO)) %>%
           #clean up value of null refs/empty values
           filter(!ServiceArea %in% c(NA,"#REF!")) %>%
           filter(!Value %in% c(NA,"#REF!")) %>%
           mutate(Value = as.numeric(levels(Value))[Value]) %>%
           filter(Value > 0)
       },
       cw={
         #Coastal wetlands
         esvDat = esvDat %>%
           filter(Biome %in% c('Coastal','Coastal wetlands')) %>% #,'Coral Reefs','Marine',)) %>%
           filter(Valuation.Method != 'Benefit Transfer') %>%
           filter(!Continent %in% c("Various", "World")) %>%
           #Generate country codes for conversion to USD2007; cleanup as well
           mutate(ISO = substring(Unit,1,3)) %>%
           mutate(ISO = sub('\\$','D', ISO)) %>%
           mutate(ISO = sub('Rie','KHR', ISO)) %>%
           #clean up value of null refs/empty values
           filter(!ServiceArea %in% c(NA,"#REF!")) %>%
           filter(!Value %in% c(NA,"#REF!")) %>%
           mutate(Value = as.numeric(levels(Value))[Value]) %>%
           filter(Value > 0)
       }
)

#Currency conversion (https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/)
url="http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=%s&C2=%s&YA=1&DD1=%s&MM1=%s&YYYY1=%s&B=1&P=&I=1&DD2=%s&MM2=%s&YYYY2=%s&btnOK=Go%%21"

tryCatch({
  conversionDat = read.delim("conversionDat.txt")
},
error = function(e) {
  conversionDat <<- data.frame(curr1=character(0), curr2=character(0), year=integer(0), minRate=numeric(0), avgRate=numeric(0), maxRate=numeric(0), days=numeric(0), stringsAsFactors=FALSE)
},
warning = function(e) {
  conversionDat <<- data.frame(curr1=character(0), curr2=character(0), year=integer(0), minRate=numeric(0), avgRate=numeric(0), maxRate=numeric(0), days=numeric(0), stringsAsFactors=FALSE)
})

getConvRate = function(yr, cur1, cur2){ #year, to, from
  if(is.na(cur1) || is.na(cur2))
    error("NA CURRENCIES")
  
  #Check if conversion is to the same currency
  if(cur1 == cur2)
    return(list(found=TRUE, data=data.frame(year=yr,avgRate=1,minRate=1,maxRate=1,days=365)))
  
  #Check to see if data is already scraped
  results = conversionDat %>% filter(curr1==cur1, curr2==cur2, year==yr)
  if(nrow(results) > 0)
    return(list(found=TRUE, data=data.frame(year=as.numeric(yr), avgRate=as.numeric(results[1,]$avgRate),
                                            minRate=as.numeric(results[1,]$minRate), maxRate=as.numeric(results[1,]$maxRate))))
  
  #Scrape
  new_url=sprintf(url,cur1,cur2,"01","01",yr,"31","12",yr)
  
  page = read_html(new_url)
  
  convRate = page %>% 
    html_node("td td tr:nth-child(9) table") %>%
    html_table(header=TRUE)
  
  if(nrow(convRate) == 0) {
    rate = page %>% html_node("a+ table tr:nth-child(1) td+ td") %>% html_text() %>%
      substring(7,nchar(.)-4) %>% as.numeric()
    yr = page %>% html_node("td td tr:nth-child(5) > td") %>% html_text() %>% 
      substring(1,gregexpr("1 USD", .)[[1]][1]-1) %>% substring(nchar(.)-4+1,nchar(.)) %>% as.numeric()
    convRate = data.frame(year=yr,avgRate=rate,minRate=rate,maxRate=rate,days=365)
  }
  
  colnames(convRate) = c("year", "avgRate","minRate","maxRate","days")
  
  return(list(found=FALSE, data=convRate))
}

#Convert all to USD (in whichever year, NOT all 2007)
esvDat$uninflUSD = rep(NA,nrow(esvDat))
for(i in 1:nrow(esvDat)){
  if(!esvDat[i,]$standardized.2007.value.) {
    year = esvDat[i,]$Year.Of.Validation
    convRate = getConvRate(year,"USD",esvDat[i,]$ISO)
    
    esvDat[i,]$uninflUSD = esvDat[i,]$Value/convRate$data$avgRate
    
    if(!convRate$found)
    {
      if(esvDat[i,]$ISO != "USD")
        conversionDat[nrow(conversionDat)+1,] = c("USD",esvDat[i,]$ISO,year,convRate$data$minRate,convRate$data$avgRate,convRate$data$maxRate,convRate$data$days)
      write.table(conversionDat, "conversionDat.txt", row.names = FALSE, quote = FALSE, sep = "\t")
    }
  }
}

#USD inflation (http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
#Updated download link: https://fred.stlouisfed.org/series/CPIAUCSL
monthlyInflation = read.csv("CPIAUCSL.csv", header = TRUE)
names(monthlyInflation) = c("DATE", "CPIAUCSL")
monthlyInflation$cpi_year = year(monthlyInflation$DATE)
yearlyInflation = monthlyInflation %>% group_by(cpi_year) %>% summarize(cpi = mean(CPIAUCSL))

adjInflation = function(oldYear,currYear) {
  if (oldYear < min(yearlyInflation))
    return(-9999)
  
  return(yearlyInflation$cpi[yearlyInflation$cpi_year == currYear]/
           yearlyInflation$cpi[yearlyInflation$cpi_year == oldYear])
}

#Adjust USD to USD2007
esvDat$USD2007 = esvDat$Value
for(i in 1:nrow(esvDat)){
  if (!esvDat[i,]$standardized.2007.value.)
    esvDat[i,]$USD2007 = esvDat[i,]$uninflUSD*adjInflation(esvDat[i,]$Year.Of.Validation, 2007)
  else
    esvDat[i,]$USD2007 = esvDat[i,]$Value
}

#Prep for models (done after finding log relationships below)
esvDat$logUSD2007 = log(esvDat$USD2007)
esvDat$logServiceArea = log(esvDat$ServiceArea)


# Prelim Analysis ---------------------------------------------------------

plot(log(USD2007)~log(ServiceArea), data=esvDat)
lmod1 = lm(log(USD2007)~log(ServiceArea), data=esvDat)
summary(lmod1)

plot(log(USD2007)~log(ServiceArea), data=esvDat, cex.axis=1.5, cex.lab=1.5)
points(log(USD2007)~log(ServiceArea), data=esvDat[esvDat$PeerReviewed == 2,],col="red")
points(log(USD2007)~log(ServiceArea), data=esvDat[esvDat$PeerReviewed == 1,],col="blue")
#legend(15.75, 22.65, c("Peer", "No Peer"), col=c("red", "blue"), lty=1, cex=1.35)
boxplot(USD2007~PeerReviewed, data=esvDat, names=c("No Peer Review", "Peer Review"), col=c("blue","red"), ylim=c(0,9e3), cex.axis=1.5)
boxplot(log(USD2007)~PeerReviewed, data=esvDat, names=c("No Peer Review", "Peer Review"), col=c("blue","red"), cex.axis=1.5)

# Model 1 -----------------------------------------------------------------
{ # Model 1
  model1 = textConnection("
  model {
    for(i in 1:N)
    {
      logUSD2007[i] ~ dnorm(USD.mu[i], USD.tau)
      USD.mu[i] <- b0 + b1*ServiceArea[i]
      logUSDpred[i] ~ dnorm(USD.mu[i], USD.tau)
      discrepancy[i] <- pow(logUSD2007[i]-logUSDpred[i],2)
    }
    b0 ~ dnorm(0, 1/1E6)
    b1 ~ dnorm(0, 1/1E6)
    USD.sigma ~ dunif(0, 1E2)
    USD.tau <- 1/pow(USD.sigma,2)

    #PPCs
    sd.data<-sd(logUSD2007[])
    sd.sim<-sd(logUSDpred[])
    p.sd<-step(sd.sim-sd.data)
    
    mean.data<-mean(logUSD2007[])
    mean.sim<-mean(logUSDpred[])
    p.mean<-step(mean.sim-mean.data)

    disc <- sum(discrepancy[])
  }
")
}

data = list(N = nrow(esvDat))
jags1 = jags.model(model1, data = append(data,esvDat), n.chains = 3, n.adapt = 50000)
output1 = coda.samples(jags1, variable.names = c("b0","b1","USD.sigma"), n.iter=50000, thin=1)
output1pred = coda.samples(jags1, variable.names = c("logUSDpred"), n.iter=50000, thin=1)
output1ppc = coda.samples(jags1, variable.names = c("p.sd","p.mean","disc"), n.iter=50000, thin=1)
summary(output1)
gelman.diag(output1)$psrf

mcmc_areas(output1, prob=0.95)
mcmc_areas(output1, prob=0.95, regex_pars=c("b1"))


# Model 2 -----------------------------------------------------------------
{ # Model 2
  model2 = textConnection("
  model {
    for(i in 1:N)
    {
      logUSD2007[i] ~ dnorm(USD.mu[i], USD.tau)
      USD.mu[i] <- b0 + b1[PeerReviewed[i]]*logServiceArea[i]
      logUSDpred[i] ~ dnorm(USD.mu[i], USD.tau)
      discrepancy[i] <- pow(logUSD2007[i]-logUSDpred[i],2)
    }
    b0 ~ dnorm(0, 1/1E6)
    USD.sigma ~ dunif(0, 1E6)
    USD.tau <- 1/pow(USD.sigma,2)
    
    for(j in 1:2)
    {
      b1[j] ~ dnorm(b1.mu, b1.tau)
    } 
    
    b1.mu ~ dnorm(0, 1/1E6)
    b1.sigma ~ dunif(0, 1E6)
    b1.tau <- 1/pow(b1.sigma,2)

    #PPCs
    sd.data<-sd(logUSD2007[])
    sd.sim<-sd(logUSDpred[])
    p.sd<-step(sd.sim-sd.data)
    
    mean.data<-mean(logUSD2007[])
    mean.sim<-mean(logUSDpred[])
    p.mean<-step(mean.sim-mean.data)

    disc <- sum(discrepancy[])
  }
")
}

data = list(N = nrow(esvDat))
jags2 = jags.model(model2, data = append(data,esvDat), n.chains = 3, n.adapt = 50000)
output2 = coda.samples(jags2, variable.names = c("b0","b1","USD.sigma", "b1.sigma"), n.iter=50000, thin=1)
output2pred = coda.samples(jags2, variable.names = c("logUSDpred"), n.iter=50000, thin=1)
output2ppc = coda.samples(jags2, variable.names = c("p.sd","p.mean","disc"), n.iter=50000, thin=1)
summary(output2)
gelman.diag(output2)$psrf

mcmc_areas(output2, prob=0.95)
mcmc_areas(output2, prob=0.95, regex_pars=c("b0","b1\\[\\d*\\]"))
mcmc_areas(output2, prob=0.95, regex_pars=c("b1\\[\\d*\\]"))

bigout2 = data.frame(do.call(rbind,output2))
plot(density(bigout2$b1.1.-bigout2$b1.2.))

# Model 3 -----------------------------------------------------------------
{ # Model 3
  model3 = textConnection("
  model {
    for(i in 1:N)
    {
      logUSD2007[i] ~ dnorm(USD.mu[i], USD.tau)
      USD.mu[i] <- b0[PeerReviewed[i]] + b1*logServiceArea[i]
      logUSDpred[i] ~ dnorm(USD.mu[i], USD.tau)
      discrepancy[i] <- pow(logUSD2007[i]-logUSDpred[i],2)
    }
    b1 ~ dnorm(0, 1/1E6)
    USD.sigma ~ dunif(0, 1E6)
    USD.tau <- 1/pow(USD.sigma,2)
    
    for(j in 1:2)
    {
      b0[j] ~ dnorm(b0.mu, b0.tau)
    } 
    
    b0.mu ~ dnorm(0, 1/1E6)
    b0.sigma ~ dunif(0, 1E6)
    b0.tau <- 1/pow(b0.sigma,2)

    #PPCs
    sd.data<-sd(logUSD2007[])
    sd.sim<-sd(logUSDpred[])
    p.sd<-step(sd.sim-sd.data)
    
    mean.data<-mean(logUSD2007[])
    mean.sim<-mean(logUSDpred[])
    p.mean<-step(mean.sim-mean.data)

    disc <- sum(discrepancy[])
  }
")
}

data = list(N = nrow(esvDat))
jags3 = jags.model(model3, data = append(data,esvDat), n.chains = 3, n.adapt = 50000)
output3 = coda.samples(jags3, variable.names = c("b0","b1","USD.sigma", "b0.sigma"), n.iter=50000, thin=1)
output3pred = coda.samples(jags3, variable.names = c("logUSDpred"), n.iter=50000, thin=1)
output3ppc = coda.samples(jags3, variable.names = c("p.sd","p.mean","disc"), n.iter=50000, thin=1)
summary(output3)
gelman.diag(output3)$psrf

mcmc_areas(output3, prob=0.95)
mcmc_areas(output3, prob=0.95, regex_pars=c("b1","b0\\[\\d*\\]"))
mcmc_areas(output3, prob=0.95, regex_pars=c("b0\\[\\d*\\]"))

bigout3 = data.frame(do.call(rbind,output3))
plot(density(bigout3$b0.1.-bigout3$b0.2.))


# Analysis ----------------------------------------------------------------

#par(mar=c(5,4.2,5,0))
par(mar=c(5,4.2,5,2))
plot(density(esvDat.a$logUSD2007), main="Posterior predictive distributions vs Original data", xlab = "USD2007", cex.lab = 1.7, cex.main=1.5, cex.axis=1.5, xlim=c(-10,23))
lines(density(do.call(rbind, output1pred.a)),col="red")
lines(density(do.call(rbind, output2pred.a)),col="blue")
lines(density(do.call(rbind, output3pred.a)),col="turquoise")
legend(11.35, 0.1173, c("Data", "Pooled","Random Intercepts", "Random Slopes"), col=c("black", "red", "blue", "turquoise"), lty=1, cex=1)

#par(mar=c(5,4.2,5,0))
par(mar=c(5,4.2,5,2))
plot(density(esvDat$logUSD2007), main="Posterior predictive distributions vs Original data", xlab = "USD2007", cex.lab = 1.7, cex.main=1.5, cex.axis=1.5, xlim=c(-10,23))
lines(density(do.call(rbind, output1pred)),col="red")
lines(density(do.call(rbind, output2pred)),col="blue")
lines(density(do.call(rbind, output3pred)),col="turquoise")
legend(9.5, 0.11155, c("Data", "Pooled","Random Intercepts", "Random Slopes"), col=c("black", "red", "blue", "turquoise"), lty=1, cex=1)
#legend(11.35, 0.1173, c("Data", "Pooled","Random Intercepts", "Random Slopes"), col=c("black", "red", "blue", "turquoise"), lty=1, cex=1)
summary(output1)
summary(output2)
summary(output3)
summary(output1ppc)
summary(output2ppc)
summary(output3ppc)

rbind(
  c(summary(output1ppc)$statistics[2], summary(output2ppc)$statistics[2], summary(output3ppc)$statistics[2]),
  c(summary(output1ppc)$statistics[3], summary(output2ppc)$statistics[3], summary(output3ppc)$statistics[3]),
  c(summary(output1ppc)$statistics[1], summary(output2ppc)$statistics[1], summary(output3ppc)$statistics[1]),
  abs(0.50-c(summary(output1ppc)$statistics[2], summary(output2ppc)$statistics[2], summary(output3ppc)$statistics[2])),
  abs(0.50-c(summary(output1ppc)$statistics[3], summary(output2ppc)$statistics[3], summary(output3ppc)$statistics[3]))
)
  

mcmc_areas(output1,prob=0.95,cex=3)
mcmc_areas(output2,prob=0.95,regex_pars=c("b0","b.\\[\\d*\\]","USD.sigma"))
mcmc_areas(output2,prob=0.95,regex_pars=c("b.\\[\\d*\\]"))
mcmc_areas(output3,prob=0.95,regex_pars=c("b1","b.\\[\\d*\\]","USD.sigma"))
mcmc_areas(output2,prob=0.95,regex_pars=c("USD.sigma","b1.sigma"))
mcmc_areas(output3,prob=0.95,regex_pars=c("USD.sigma","b0.sigma"))


