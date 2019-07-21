write.table(thk99buff_n, "Data/thk99buff_n.txt", sep="\t", row.names=F, quote=F)
writeOGR(thk99buff, "Data/thk99buff.shp", "thk99buff", driver="ESRI Shapefile")

library(rpostgis)
con = dbConnect(PostgreSQL(), dbname = "gistest", user = "power_user",
                                          host = "13.58.102.61", port = 5432,
                                          password = "powpass17")
pgInsert(con, "thk99buff", thk99buff, new.id = "gid")
dbDisconnect(con)

getModName = function(modelNo, climScen, year, ha=NULL, bw=NULL, vp=NULL, mc=NULL) {
  if (year > 2006 && climScen > 1) { year = paste0("_", year) } else { year = ""}
  climScen = c("", "-RCP3", "-RCP85")[climScen]
  if (all(!is.null(c(ha, bw, vp, mc)))) {
    restore = paste0("-", ha, bw, vp, mc)
  } else {
    restore = ""
  }

  return(sprintf("%s%s%s%s", modelNo, climScen, year, restore))
}
loadScenario = function(modelNo, climScen, year, ha=NULL, bw=NULL, vp=NULL, mc=NULL) {
  modName = getModName(modelNo, climScen, year, ha, bw, vp, mc)

  x = read.delim(sprintf("../../DataPrep/Results/%s.txt", modName), sep = "\t", check.names=F)

  assign(modName, x, envir = .GlobalEnv)
}

write.table(`58-Any-10y-Y-RCP3_2100-od`, "Data/58-RCP3-2100.txt", row.names=F, quote=F, sep="\t")
write.table(`58-Any-10y-Y-RCP3_2300-od`, "Data/58-RCP3-2300.txt", row.names=F, quote=F, sep="\t")
write.table(`58-Any-10y-Y-RCP85_2100-od`, "Data/58-RCP85-2100.txt", row.names=F, quote=F, sep="\t")
write.table(`58-Any-10y-Y-RCP85_2300-od`, "Data/58-RCP85-2300.txt", row.names=F, quote=F, sep="\t")


loadScenario(58, 1, 2100)
loadScenario(58, 2, 2100)
loadScenario(58, 2, 2300)

rsconnect::setAccountInfo(name='wulab', token='C78F6D859E7BC1978D7E64FEAA096D5D', secret='V3gDEbYtaciO/+Ay6rkG8QfKnBWICOOECOggBkOi')
