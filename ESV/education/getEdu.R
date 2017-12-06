library(wbstats)

wbsearch("enrollment")

enrollDat = wb(indicator = "SE.PRE.ENRR", startdate = 1972, enddate = 1972)
enrollDat2 = wb(indicator = "SE.PRM.NENR", startdate = 1972, enddate = 1972)

for(i in 1:nrow(esvDat))
{
  year = esvDat[i,]$Year.Of.Validation
  enrollDat = wb(indicator = "SE.PRM.NENR", startdate = year, enddate = year)
}
