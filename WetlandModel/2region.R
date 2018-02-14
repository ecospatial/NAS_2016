setwd("WetlandModel")

library(RPostgreSQL)
library(postGIStools)

source("../DecisionSuppTool/mysqlcfg.R")

#########################################
# Database Connection + Loading/Merging
#########################################
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

#########################################
# Drop Database connection
#########################################
dbDisconnect(con)


#########################################
# Evaluation
#########################################