rm(list=ls())

# // make sure to install rgee
library(rgee)
ee_Initialize('athapa2@alaska.edu')

# // fun to use scale factor and upset to gonvert image to reflectance
scaleLandsat = function(img) {
  # // first, extract the year
  imgScaled = img$select('SR_B.')$multiply(0.0000275)$add(-0.2);
  return(imgScaled)
}

# //  function to extract green bands from image collection
extractGreen = function(img) {
  return(img$select('SR_B2'))
}

# // list of names; this is a client-side list
names = c('LANDSAT/LT05/C02/T1_L2/LT05_066017_19940929',
             'LANDSAT/LT05/C02/T1_L2/LT05_066017_20030906',
             'LANDSAT/LT05/C02/T1_L2/LT05_068017_20110926')

# // create ImageCollection
imageCollection = ee$ImageCollection(names); #// from list of image names
ee_print(imageCollection)

# // extract years from image collection
years=substr(ee_get_date_ic(imageCollection)[,2],1,4)
print(years)

# // scale to surface reflectance
imageCollectionScaled=imageCollection$map(scaleLandsat)
ee_print(imageCollectionScaled)

# Define the visualization parameters.
vizParams <- list(
  bands = c("SR_B1", "SR_B2", "SR_B3"),
  min = 0,
  max = 1,
  gamma = c(1, 1, 1)
)

# // Zoom to Columbia Glacier.
Map$setCenter(-146.9, 61.1, 10);

# map the image collection
Map$addLayer(ee$Image(imageCollectionScaled$toList(imageCollectionScaled$size())$get(0)),vizParams, "TCC_1994")+
  Map$addLayer(ee$Image(imageCollectionScaled$toList(imageCollectionScaled$size())$get(1)),vizParams, "TCC_2003")+
  Map$addLayer(ee$Image(imageCollectionScaled$toList(imageCollectionScaled$size())$get(2)),vizParams, "TCC_2011")
  

# // extract green band
imageCollectionGreen = imageCollectionScaled$map(extractGreen);
ee_print(imageCollectionGreen)

# // Convert  green image collection to a single multitemporal image
imgMTGreen = imageCollectionGreen$toBands();

# // The renaming is a bit more advanced; GEE can be a pain
imgMTGreen = imgMTGreen$rename(years);

# // Add the multitemporal composite; 1994 shown in red, 2003 in green and 2011 in blue
Map$addLayer(imgMTGreen, list(bands= c('1994', '2003', '2011'), min= 0, max=1), 'multitemporal-green')

# Customize visualization using cptcity [make sure to install it]
library(cptcity)
find_cpt("radar")
green_VIZ= list(min = c(0), max = c(1),palette = cpt("mpl_inferno", rev = TRUE))

# map the image collection
Map$addLayer(imgMTGreen$select('1994'),green_VIZ,name ="green 1994")+
  Map$addLayer(imgMTGreen$select('2003'),green_VIZ,name ="green 2003")+
  Map$addLayer(imgMTGreen$select('2011'),green_VIZ,name ="green 2011")+
  Map$addLegend(visParams = green_VIZ,
                name = 'Green reflectance',position = c("bottomleft") ) 


# // Compute the NDSI:
ndsi <- function(img) {
  # Calculate the NDSI
  ndsi_values <- img$normalizedDifference(c("SR_B2","SR_B5"))$rename('NDSI')
  return(ndsi_values)
}

# // apply NDSI function to image collection
NDSI_imageColl= imageCollectionScaled$map(ndsi)
ee_print(NDSI_imageColl)

# // Convert  NDSI image collection to a single multitemporal image
ndsiComposite = NDSI_imageColl$toBands();
ee_print(ndsiComposite)

# // Rename the bands
ndsiComposite = ndsiComposite$rename(years);
ee_print(ndsiComposite)

# // add NDSI composite to map
Map$addLayer(ndsiComposite, list(bands= c('1994', '2003', '2011'), min= -1, max=1), 'multitemporal-NDSI')+
Map$addLayer(ndsiComposite, list(bands= c('1994', '2003', '2011'), min= 0, max=1), 'multitemporal-NDSI')

# add both composite to map
Map$addLayer(imgMTGreen, list(bands= c('1994', '2003', '2011'), min= 0, max=1), 'multitemporal-green')+
  Map$addLayer(ndsiComposite, list(bands= c('1994', '2003', '2011'), min= -1, max=1), 'multitemporal-NDSI_0_1')+
  Map$addLayer(ndsiComposite, list(bands= c('1994', '2003', '2011'), min= 0, max=1), 'multitemporal-NDSI_-1_1')


NDSI_VIZ= list(min = c(-1), max = c(1),palette = cpt("idv_temperature", rev = TRUE))

# map the image collection
Map$addLayer(ndsiComposite$select('1994'),NDSI_VIZ,name ="NDSI 1994")+
  Map$addLayer(ndsiComposite$select('2003'),NDSI_VIZ,name ="NDSI 2003")+
  Map$addLayer(ndsiComposite$select('2011'),NDSI_VIZ,name ="NDSI 2011")+
  Map$addLegend(visParams = NDSI_VIZ,
                name = 'NDSI Snow Cover',position = c("bottomleft") ) 

