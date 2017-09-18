library(tidyr)
library(dplyr)
library(rvest)
library(lubridate)
library(dplyr)

esv_dat = read.delim('TEEB.txt') %>%
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

getConvRate = function(year, curr1, curr2){ #year, to, from
  new_url=sprintf(url,curr1,curr2,"01","01",year,"31","12",year)
  
  conv_chart = read_html(new_url) %>% 
    html_node("td td tr:nth-child(9) table") %>%
    html_table(header=TRUE) 
  
  colnames(conv_chart) = c("YEAR", "ConvMin","ConvMax","ConvAvg","Days")
  return(conv_chart[1,]$ConvAvg)
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
esv_dat$uninflUSD = rep(NA,nrow(esv_dat))
for(i in 1:nrow(esv_dat)){
  if (!esv_dat[i,]$standardized.2007.value.) {
    year = esv_dat[i,]$Year.Of.Validation
    convRate = getConvRate(year,"USD",esv_dat[i,]$ISO)
    esv_dat[i,]$uninflUSD = convRate*esv_dat[i,]$Value
  }
}
i=154
if (!esv_dat[i,]$standardized.2007.value.) {
  year = esv_dat[i,]$Year.Of.Validation
  convRate = getConvRate(year,"USD",esv_dat[i,]$ISO)
  print(convRate*esv_dat[i,]$Value)
}

#Adjust USD to USD2007
esv_dat$USD2007 = esv_dat$Value
for(i in 1:nrow(esv_dat)){
  if (!esv_dat[i,]$standardized.2007.value.)
    esv_dat[i,]$USD2007 = esv_dat[i,]$uninflUSD*adjInflation(esv_dat[i,]$Year.Of.Validation, 2007)
  else
    esv_dat[i,]$USD2007 = esv_dat[i,]$Value
}

#Salvador Colon https://books.google.com/books?id=XC8k3GXUKmoC&pg=PA35&lpg=PA35&dq=exchange+rates+1998+salvador+colon++svc&source=bl&ots=XcGUqbF28A&sig=xvXLFuQWQyKL7-J86KZ6_B5cAbM&hl=en&sa=X&ved=0ahUKEwixwZbI-6XPAhXI7D4KHRiNDJY4ChDoAQgnMAI#v=onepage&q=exchange%20rates%201998%20salvador%20colon%20%20svc&f=false
#
# colons = which(is.na(esv_dat$USD2007))
# for (i in colons)
# {
#   esv_dat[i,]$uninflUSD = esv_dat[i,]$Value*0.11400
#   esv_dat[i,]$USD2007 = esv_dat[i,]$uninflUSD*adjInflation(esv_dat[i,]$Year.Of.Validation, 2007)
# }

#write.table(esv_dat, "TEEB-NoBT-USD2007.txt", row.names = FALSE, quote = FALSE, sep="\t")


