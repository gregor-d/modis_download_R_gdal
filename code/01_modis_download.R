#**********************************************************
# Filename: 01_modis_download.R (2019-07-02)
# Author(s): Gregor Didenko
#
# Download MODIS NDVI raster for a given extent and create map
#
#**********************************************************

#**********************************************************
# SET UP--------------
#**********************************************************

pacman::p_load("MODIS", "dplyr", "sf", "raster", "tmap", "tmaptools")

# Note: Download GDAL from http://www.gisinternals.com/release.php
# specify path to GDAL and paths
MODISoptions(
  # path to GDAL on your computer
  #gdalPath = "C:/Program Files/GDAL",
  gdalPath = "C:/OSGeo4W64/bin",
  # where to save downloaded MODIS data
  localArcPath = "data/modis",
  # where to save processed (projected, cropped and resampled) data
  outDirPath = "data/modis/processed",
  checkTools = TRUE,
  MODISserverOrder = c("LPDAAC", "LAADS"),
  dlmethod = "auto")
# save options for later on
opts = MODISoptions()

# create an Earthdata account:
# https://urs.earthdata.nasa.gov/home
# create file with Earthdata login credentials
EarthdataLogin("enso_earthdata", "asdfASDF1234")


#**********************************************************
# DOWNLOAD AND PREPROCESS MODIS--------------------------
#**********************************************************

# check products
prods = getProduct()
levels(prods$TOPIC)
filter(prods, TOPIC == "Vegetation Indices")
# MODIS/Terra Vegetation Indices 16-Day L3 Global 250m
product = "MOD13A3"
getCollection(product)

# define the extent of the studyarea in UTM 17 S
bb <- st_bbox(c(xmin = 400000 , xmax = 700000, ymax = 9350000, ymin = 9550000),
               crs = st_crs(32717))
# transfrom to WGS
sf_bb <- st_as_sfc(bb) %>% st_transform(crs = 4326)

# create raster-extent object
bbox = extent(st_bbox(sf_bb)[1],
              st_bbox(sf_bb)[3],
              st_bbox(sf_bb)[2],
              st_bbox(sf_bb)[4])
bbox

# download MODIS product for extent bbox with given begin- and end-dates
# NDVI december
runGdal(
  # the processed data will be stored in a folder named like job
  job = "VI_peru_dec",
  # choose the product to download, here MOD13A3
  product = "MOD13A3",
  # select only the bands you need, here NDVI, EVI
  SDSstring = "11",
  collection = "006",
  begin = as.POSIXct("01/12/2016", format = "%d/%m/%Y"),
  end = as.POSIXct("31/12/2016", format = "%d/%m/%Y"),
  # merge neighoring MODIS scenes, crop them to the extent of our study area
  extent = bbox,
  # reproject into UTM Zone 17 S
  outProj = "32717"
)

# NDVI march
runGdal(
  job = "VI_peru_mar",
  product = "MOD13A3",
  SDSstring = "11",
  collection = "006",
  begin = as.POSIXct("01/03/2017", format = "%d/%m/%Y"),
  end = as.POSIXct("31/03/2017", format = "%d/%m/%Y"),
  extent = bbox,
  outProj = "32717"
)

#**********************************************************
# Create NDVI raster stack ----
#**********************************************************

# load NDVI modis data from into R
# opts[["outDirPath"]]
file_path = file.path("data/modis/processed")
# get only ndvi files
ndvi_files = grep("NDVI", list.files(file_path, recursive = TRUE), value = TRUE)

# find out when the images were taken
dates = extractDate(ndvi_files, asDate = TRUE)$inputLayerDates
dates

# load ndvi data into raster stack, only december and march
ndvi_stack = stack(file.path(file_path, ndvi_files)[c(2,4)])
# rescale to put NDVI between -1 and 1
ndvi_stack = ndvi_stack * 0.0001
plot(ndvi_stack)
# set names 
names(ndvi_stack) = c("NDVI_December", "NDVI_March")

# world_sf = st_as_sf(map("world", fill = TRUE, plot = FALSE))

#**********************************************************
# Create and Save Maps ----
#**********************************************************

# NDVI december
ndvi_dec = tm_shape(ndvi_stack) +
  tm_raster("NDVI_December",
            title = "NDVI", 
            palette = rev(terrain.colors(20)),
            breaks = seq(0, 1, 0.1)) + 
  #tm_shape(world_sf) + tm_borders(alpha = 0.5) + 
  tm_grid(alpha = 0.2,
          # location and number of gridlines
          y = seq(9400000, 9500000, 50000),
          x = seq(450000,  650000,  50000),
          labels.inside.frame = FALSE,
          # remove number seperator and add unit "m"
          labels.format = list(fun = function(x){
                              paste0(formatC(x, digits = 0, format = "d"), "m")
                          })
          ) +
  # adds natural "water" color as bg.color
  tm_style("natural") + 
  # set margins, or UTM coordinates will lay outside of plot
  tm_layout(outer.margins = c(.05,.05,.05,.05),
            # change seperator from "a to b" to "a - b"
            legend.format = list(text.separator = "-"),
            # increase size
            legend.text.size = 1)
ndvi_dec
tmap_save(ndvi_dec, "figures/ndvi_december.png")

ndvi_mar = tm_shape(ndvi_stack) +
  tm_raster("NDVI_March",
            title = "NDVI", 
            palette = rev(terrain.colors(20)),
            breaks = seq(0, 1, 0.1),
            legend.show = FALSE) + 
  #tm_shape(wut) + tm_borders(alpha = 0.5) + 
  tm_grid(alpha = 0.2,
          y = seq(9400000, 9500000, 50000),
          x = seq(450000,  650000,  50000),
          labels.inside.frame = FALSE,
          labels.format = list(fun = function(x){
            paste0(formatC(x, digits = 0, format = "d"), "m")
          })) +
  tm_style("natural") + 
  tm_layout(outer.margins = c(.05,.05,.05,.05))
ndvi_mar
tmap_save(ndvi_mar, "figures/ndvi_march.png")




