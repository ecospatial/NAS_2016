library(RPostgreSQL)

source("../config/postgresqlcfg.R")

#########################################
# Database Connection + Table Creation
#########################################
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = db, user = user,
                   host = host, port = port,
                   password = pw)
  rm(pw);rm(user)
}

if(exists('esvDat') && exists('conversionDat')){
  dbWriteTable(con, "esv",value=esvDat,row.names=FALSE)
  dbWriteTable(con, "conversion",value=conversionDat,row.names=FALSE)
}

dbDisconnect(con)
