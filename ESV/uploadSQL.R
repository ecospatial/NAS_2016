library(RPostgreSQL)

source("../DecisionSuppTool/mysqlcfg.R")

#########################################
# Database Connection + Table Creation
#########################################
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = "postgiz", user = user,
                   host = "52.14.87.100", port = 5432,
                   password = pw)
  rm(pw);rm(user)
}

if(exists('esvDat') && exists('conversionDat')){
  dbWriteTable(con, "esv",value=esvDat,row.names=FALSE)
  dbWriteTable(con, "conversion",value=conversionDat,row.names=FALSE)
}

dbDisconnect(con)
