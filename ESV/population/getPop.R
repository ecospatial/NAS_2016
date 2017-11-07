library(wpp2017)
library(dplyr)
library(readxl)

# data(pop)
# Above data too coarse (5 year interval, only after 1990)
# Use downloaded data
url = "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2017_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"
file = "./population/wpp2017.xlsx"
download.file(url, destfile = file, mode="wb")
pop = read_xlsx("./population/wpp2017.xlsx")
colnames(pop) = pop[12,]
pop = pop[-(1:12),]
colnames(pop) = gsub("Region, subregion, country or area \\*", "name", colnames(pop))

# # World bank subregions (i=14; western europe, southeast asia)
# levels(esvDat$Region..World.Bank.classes.) = gsub("Australia and New Zealand", "Australia/New Zealand", levels(esvDat$Region..World.Bank.classes.))
# pop = pop %>% filter(tolower(name) %in% tolower(unique(esvDat$Region..World.Bank.classes.)))

# # Continents (i=6)
levels(esvDat$Continent) = gsub("Americas", "Northern America", levels(esvDat$Continent))
pop = pop %>% filter(tolower(name) %in% tolower(unique(esvDat$Continent)))

esvDat$pop = rep(NA,nrow(esvDat))
for(i in 1:nrow(esvDat))
{
  esvDat[i,]$pop = as.numeric(pop[tolower(pop$name) == tolower(esvDat[i,]$Continent),][as.character(esvDat[i,]$Year.Of.Validation)])
}

