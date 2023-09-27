rm(list=ls())

library(googledrive)
library(rgee)
library(mapedit)
library(tibble)
library(sf)
library(cptcity)
library(tidyverse)
library(sp)
library(leaflet.extras2)
library(magick)
# remotes::install_github("r-earthengine/rgeeExtra")
library(rgeeExtra)
library(raster)

ee_Initialize("athapa2@alaska.edu")

Nepal    <- getData('GADM', country='Nepal', level=2) %>%st_as_sf()  
Central     <- subset(Nepal, NAME_1  == "Central")
Bagmati         <- subset(Central , NAME_2  == "Bagmati")
plot(Bagmati)

# write_sf(Bagmati,"Bagmati.shp")

#mask <- st_read("SHP/Central.shp")%>%  sf_as_ee()  
mask <-Bagmati%>%  sf_as_ee()  
region <- mask$geometry()$bounds()

# Recupere el conjunto de datos MODIS Terra Vegetation Indices 16-Day Global 1km como ee.ImageCollectiony seleccione la banda NDVI.

col    <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')

# Agrupar imágenes por fecha compuesta

col         <- col$map(function(img) {
  doy         <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)})

distinctDOY <- col$filterDate('2020-01-01', '2021-01-01')


filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')

# Defina una combinación; convierta el FeatureCollection resultante en un ImageCollection.

join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))

# Aplicar la reducción media entre las colecciones DOY coincidentes.
comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})
# Defina los parámetros de visualización RGB.

visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)



rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>% 
    ee$Image$clip(mask)
})

# Defina los parámetros de visualización GIF.

gifParams <- list(
  region = region,
  dimensions = 900,
  crs = 'EPSG:3857',
  framesPerSecond = 5
)

dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .) # subset around month abbreviations

# Use las funciones ee_utils_gif_ * para renderizar la animación GIF y agregar algunos textos.
animation <-  rgeeExtra::ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")
print(animation)

animation %>% 
  
  rgeeExtra::ee_utils_gif_annotate(
    text = "AOI:Bagmati",
    size = 70, color = "white",
    location = "+20+650"
  ) %>% 
  
  rgeeExtra::ee_utils_gif_annotate(
    text = "DataViz: \nAmrit THAPA",
    size = 20, color = "white",
    location = "+750+680"
  ) %>% 
  
  ee_utils_gif_annotate(
    text = dates_modis_mabbr,
    size = 40,
    location = "+20+20",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  )%>%
  
  rgeeExtra::ee_utils_gif_annotate(
    text = "DATA:MODIS NDVI - 2020", 
    size = 25, 
    location = "+20+620",
    color = "white", 
    font = "arial",
    boxcolor = "#000000"
  ) -> animation_wtxt

print(animation_wtxt)

ee_utils_gif_save(animation_wtxt, path = "Bagmati_2020.gif")
