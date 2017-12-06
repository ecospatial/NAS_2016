library(wbstats)

wbsearch("gross domestic product")
a=wb(country="AFR", indicator="NY.GDP.MKTP.CD", startdate=1972, enddate=1973)
