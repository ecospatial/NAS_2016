library(tidyr)
library(dplyr)
library(dplyr)
library(rjags)
library(bayesplot)

esvDat = read.delim("ESV/TEEB.txt")

  #Coastal wetlands
  esvDat = esvDat %>%
    filter(Biome %in% c('Coastal','Coastal wetlands','Coral Reefs','Marine')) %>%
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
  
  write.table(esvDat, "TEEB-Coastal.txt", sep="\t", row.names = F, quote = F)