library(tidyr)
library(dplyr)
library(rvest)
library(lubridate)
library(dplyr)

esvDat = read.delim('TEEB.txt') %>%
  filter(Biome %in% c('Marine','Coastal','Coral Reefs','Coastal wetlands')) %>%
  filter(Valuation.Method != 'Benefit Transfer') %>%
  filter(!Continent %in% c("Various", "World")) %>%
  #Generate country codes for conversion to USD2007
  mutate(ISO = substring(Unit,1,3)) %>%
  #clean up value of null refs/empty values
  filter(!Value %in% c(NA,"#REF!")) %>%
  mutate(Value = as.numeric(levels(Value))[Value])

#Currency conversion (https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/)
url="http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=%s&C2=%s&YA=1&DD1=%s&MM1=%s&YYYY1=%s&B=1&P=&I=1&DD2=%s&MM2=%s&YYYY2=%s&btnOK=Go%%21"

conversionData = data.frame(curr1=character(0), curr2=character(0), year=integer(0), minRate=numeric(0), avgRate=numeric(0), maxRate=numeric(0), stringsAsFactors=FALSE)
getConvRate = function(year, curr1, curr2){ #year, to, from
  new_url=sprintf(url,curr1,curr2,"01","01",year,"31","12",year)
  
  conv_chart = read_html(new_url) %>% 
    html_node("td td tr:nth-child(9) table") %>%
    html_table(header=TRUE) 
  
  colnames(conv_chart) = c("YEAR", "avgRate","minRate","maxRate","Days")
  conversionData[nrow(conversionData)+1,] = c(curr1,curr2,year,conv_chart$minRate,conv_chart$avgRate,conv_chart$maxRate)
  return(conv_chart[1,]$avgRate)
}

#USD inflation (http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
#Updated download link: https://fred.stlouisfed.org/series/CPIAUCSL
monthlyInflation = read.csv("CPIAUCSL.csv", header = TRUE)
monthlyInflation$cpi_year <- year(monthlyInflation$DATE)
yearlyInflation <- monthlyInflation %>% group_by(cpi_year) %>% summarize(cpi = mean(CPIAUCSL))

adjInflation = function(oldYear,currYear) {
  if (oldYear < min(yearlyInflation))
    return(-9999)
  
  return(yearlyInflation$cpi[yearlyInflation$cpi_year == currYear]/
           yearlyInflation$cpi[yearlyInflation$cpi_year == oldYear])
}

#Convert all to USD (in whichever year, NOT all 2007)
esvDat$uninflUSD = rep(NA,nrow(esvDat))
for(i in 1:nrow(esvDat)){
  if (!esvDat[i,]$standardized.2007.value.) {
    year = esvDat[i,]$Year.Of.Validation
    convRate = getConvRate(year,"USD",esvDat[i,]$ISO)
    esvDat[i,]$uninflUSD = convRate*esvDat[i,]$Value
  }
}

#Adjust USD to USD2007
esvDat$USD2007 = esvDat$Value
for(i in 1:nrow(esvDat)){
  if (!esvDat[i,]$standardized.2007.value.)
    esvDat[i,]$USD2007 = esvDat[i,]$uninflUSD*adjInflation(esvDat[i,]$Year.Of.Validation, 2007)
  else
    esvDat[i,]$USD2007 = esvDat[i,]$Value
}

#Salvador Colon https://books.google.com/books?id=XC8k3GXUKmoC&pg=PA35&lpg=PA35&dq=exchange+rates+1998+salvador+colon++svc&source=bl&ots=XcGUqbF28A&sig=xvXLFuQWQyKL7-J86KZ6_B5cAbM&hl=en&sa=X&ved=0ahUKEwixwZbI-6XPAhXI7D4KHRiNDJY4ChDoAQgnMAI#v=onepage&q=exchange%20rates%201998%20salvador%20colon%20%20svc&f=false
#
# colons = which(is.na(esvDat$USD2007))
# for (i in colons)
# {
#   esvDat[i,]$uninflUSD = esvDat[i,]$Value*0.11400
#   esvDat[i,]$USD2007 = esvDat[i,]$uninflUSD*adjInflation(esvDat[i,]$Year.Of.Validation, 2007)
# }

#write.table(esvDat, "TEEB-NoBT-USD2007.txt", row.names = FALSE, quote = FALSE, sep="\t")


