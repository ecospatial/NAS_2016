library(XML)
library(plyr)
library(rgdal)

convertEEkmlToShp = function(filePath, kmlOutPath, shpOutPath, layerName)
{
  args = c(filePath, kmlOutPath, shpOutPath, layerName)
  if (any(is.null(args)) | any(is.na(args)))
  {
    stop("Error in convertEEkmlToShp: null or missing argument")
  }
  
  doc = xmlTreeParse(filePath, useInternalNodes = TRUE)
  root = xmlRoot(doc)
  placemarks = root[["Document"]]
  
  df = data.frame()
  i=1
  numericRows = list()
  while(!is.null(placemarks[[i]]))
  {
    p = placemarks[[i]]
    values = c()
    colNames = c()
    row = list()
    for (j in 1:length(names(p[["ExtendedData"]])))
    {
      colName = xmlAttrs(p[["ExtendedData"]][[j]])
      value = xmlValue(p[["ExtendedData"]][[j]][["value"]])
      
      # Populate numeric list for holding number of rows with numeric variable.
      if (is.null(numericRows[[colName]]))
      {
        numericRows[[colName]] = 0
      }
      # If variable can be numeric, add 1 for that variable. If this number matches the total rows, variable is numeric.
      if (!is.na(as.numeric(value)))
      {
        numericRows[[colName]] = numericRows[[colName]] + 1
      }
      
      row[[colName]] = value
    }
    
    df = rbind(df, as.data.frame(row))
    i = i + 1
  }
  
  # Convert numeric variables to numerics
  for (name in names(numericRows))
  {
    if (numericRows[[name]] == (i - 1))
    {
      df[[name]] = as.numeric(as.character(df[[name]]))
    }
  }
  
  
  # Modify kml file for R
  kmlDat = readLines(file(filePath))
  kmlDatNew = kmlDat[1:3]
  kmlDatNew = c(kmlDatNew, sprintf("<Folder><name>%s</name>", layerName))
  kmlDatNew = c(kmlDatNew, kmlDat[4:(length(kmlDat)-2)])
  kmlDatNew = c(kmlDatNew, "</Folder>")
  kmlDatNew = c(kmlDatNew, kmlDat[(length(kmlDat)-2+1):length(kmlDat)])
  writeLines(kmlDatNew, kmlOutPath)
  
  spatialKml = readOGR(kmlOutPath, layerName)
  spatialKml@data = df
  writeOGR(spatialKml, shpOutPath, layerName, driver="ESRI Shapefile")
}