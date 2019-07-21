import ee
import re
import pandas as pd

ee.Initialize()

ccap9606 = ee.ImageCollection("users/tylerthardy/ccap_1996_2006")
thk99 = ee.FeatureCollection("users/tylerthardy/THK99_gulf")
LAMissDelta = ee.Geometry.Point([-89.2928409576416, 29.775180367410503])
ls5 = ee.ImageCollection("LANDSAT/LT05/C01/T1")

# ////////////////// CONFIG ////////////////// 
# ////////////////// CONFIG ////////////////// 

# Specify wetland change parameters
fromRegex = "^.*(Palustrine|Estuarine) (?!Aquatic)"
toRegex = "^.*(Water|Aquatic)"

# Specify bands for normalized difference veg index (X - Y) / (X + Y)
X = 'B4'
Y = 'B3'
vegIdxName = 'NDVI'

# ////////////////// END CONFIG ////////////////// 
# ////////////////// END CONFIG ////////////////// 
# CCAP Change Data Table
wetcodes = pd.read_csv("wetlandCodes.txt", sep='\t')

# //////////////// THK99 BUFFERS ////////////////// 
# Buffer THK99 areas
def Buffer(img):
	return img.buffer(1500)

thk99buff = thk99.map(Buffer)

# //////////////// WETLAND CHANGE ////////////////// 

# Process all change codes for the ones described in the Regex
changeCodes = []
for i, entry in wetcodes.iterrows():
  if re.match(fromRegex, entry.Type96) and re.match(toRegex, entry.Type06):
    changeCodes.append(entry.ID)
	
wetcodes = ee.List(changeCodes)

# Mosaic CCAP9606
ccap9606m = ccap9606.max()

# Filter CCAP change raster to only codes specified by Regex
change9606 = ccap9606m.updateMask(ccap9606m.eq(ee.Image.constant(wetcodes)).reduce(ee.Reducer.anyNonZero()))

# Count number of CCAP changes as specified by Regex in each THK99 buffer
thk99wetloss = change9606.reduceRegions(
  collection =  thk99buff,
  reducer = ee.Reducer.count(),
  scale = 30,
)

# //////////////// VEG INDEX ////////////////// 

#  Grab LS5 images for summer of 1996 in THK99 areas
image = ee.Image(
  ls5.filterBounds(thk99buff)
    .filterDate('1996-06-01', '1996-08-01')
    .median()
)

#  Compute the vegetation index (X and Y are specified above in config section).
first = image.select(X)
second = image.select(Y)
vegIdx = first.subtract(second).divide(first.add(second)).rename(vegIdxName)

#  Mask the vegetation index by the wetland change areas
maskedVegIdx = vegIdx.updateMask(change9606)

#  Calculate mean VegIdx for masked areas in each THK99 buffer area 
thk99vegIdx = maskedVegIdx.reduceRegions(
  collection = thk99buff,
  reducer = ee.Reducer.mean(),
  scale = 30
)

# Save to google drive
ee.batch.Export.table(
  ee.FeatureCollection(thk99vegIdx).select(ee.List(['ORIG_FID','mean'])),
  'thk99' + vegIdxName,
  {
    'fileFormat' : 'CSV'
  }
).start()
ee.batch.Export.table(
  ee.FeatureCollection(thk99wetloss).select(ee.List(['ORIG_FID','count'])),
  'thk99wetloss',
  {
    'fileFormat' : 'CSV'
  }
).start()
ee.batch.Export.table(
  thk99,
  'thk99',
  {
    'fileFormat' : 'CSV'
  }
).start()

