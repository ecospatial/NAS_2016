library(tidyr)
library(dplyr)
library(rvest)
library(lubridate)
library(dplyr)

esvDat = read.delim('TEEB2.txt') %>%
  filter(Biome %in% c('Coastal','Coastal wetlands')) %>% #,'Coral Reefs','Marine',)) %>% 
  filter(Valuation.Method != 'Benefit Transfer') %>%
  filter(!Continent %in% c("Various", "World")) %>%
  #Generate country codes for conversion to USD2007; cleanup as well
  mutate(ISO = substring(Unit,1,3)) %>%
  mutate(ISO = sub('\\$','D', ISO)) %>%
  #clean up value of null refs/empty values
  filter(!ServiceArea %in% c(NA,"#REF!")) %>%
  filter(!Value %in% c(NA,"#REF!")) %>%
  mutate(Value = as.numeric(levels(Value))[Value])

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
    
    esvDat[i,]$uninflUSD = convRate$data$avgRate*esvDat[i,]$Value
    
    if(!convRate$found)
    {
      if(esvDat[i,]$ISO != "USD")
        conversionDat[nrow(conversionDat)+1,] = c("USD",esvDat[i,]$ISO,year,convRate$data$minRate,convRate$data$avgRate,convRate$data$maxRate,convRate$data$days)
<<<<<<< HEAD
      write.table(conversionDat, "conversionDat.txt", row.names = FALSE, quote = FALSE, sep = "\t")
=======
>>>>>>> e180fec0bca9ba79ca238ab8c82c351a19068455
    }
  }
}
write.table(conversionDat, "conversionDat.txt", row.names = FALSE, quote = FALSE, sep = "\t")

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






source("./population/getPop.R")
