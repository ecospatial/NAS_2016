# NAS_2016

Prerequisites
--
Python
---
Python 3.7.4+
pip to install python packages
R
---
R 4.0.2+
GDAL
JAGS
Packages
----
rjags
rgdal
sp
raster
mcmcvis
magrittr
RColorBrewer
XML


Data Folder
--
1. Set up a folder on your computer where you want the GIS data stored.
2. Run the data folder scaffold python script in the DataMiner folder:
`py createDataFolder.py C:/DATA`

CCAP Data (2 Methods)
--
Use Shared Google Earth Engine Data
---
1. Import the ccap9606 variable in googleEE.py from https://code.earthengine.google.com/?asset=users/tylerthardy/ccap_1996_2006
2. Import the thk99 variable in googleEE.py from https://code.earthengine.google.com/?asset=users/tylerthardy/THK99_gulf
3. Run googleEE.py, or run the javascript equivalent in the online Google Earth Engine
4. Save files output by googleEE.py in `[[DATA_FOLDER]]/EarthEngine/T0`
5. Run shape file processing R script from DataCreation folder `processEE.R`

Generation
---
1. Download and extract the CCAP data using the python script in the DataMiner folder:
`py pingCCAP.py C:/DATA`
2. Convert the CCAP data to TIFF using the R script in the DataCreation folder:
`py CAPPtoTiff.R`
3. Create a FeatureCollection on Google Earth Engine containing the TIFF images created
4. Follow steps 1-4 above

