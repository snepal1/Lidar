rm(list = ls())
# load spatial packages
library(raster)
library(rasterVis)
library(rgdal)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rgeos)
library(sf)

################### Read in data##############################################

NAip <- stack("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\NAIP\\Adam_2020_cropped.tif")
names(NAip) <- c("red_band", "green_band", "blue_band", "near_infrared_band")
mapview(NAip)

plot(NAip,
     col = gray(0:100 / 100),
     box = FALSE, axes = FALSE)

plotRGB(NAip, r = 1, g = 2, b = 3, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")


Ponderosa <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots", layer = "Plot_12m")
site_spdf <- spTransform(Ponderosa, crs(NAip))
mapview(site_spdf)+mapview(NAip)

# Loop through each feature in the shapefile
for (i in 1:length(site_spdf)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf@data[i, "LINK"])
  
  # Clip the raster by the current feature
  cr <- crop(NAip, site_spdf[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NAIP_Clip\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

################## clip image to 14 m #############################
Ponderosa <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14", layer = "Plot_14m")
site_spdf <- spTransform(Ponderosa, crs(NAip))
plot(site_spdf)

# Loop through each feature in the shapefile
for (i in 1:length(site_spdf)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf@data[i, "LINK"])
  
  # Clip the raster by the current feature
  cr <- crop(NAip, site_spdf[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\NAIP_Clip\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

################# Clip 16m plots###########################################
Ponderosa <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16", layer = "Plot_14m")
site_spdf <- spTransform(Ponderosa, crs(NAip))
plot(site_spdf)

# Loop through each feature in the shapefile
for (i in 1:length(site_spdf)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf@data[i, "LINK"])
  
  # Clip the raster by the current feature
  cr <- crop(NAip, site_spdf[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\NAIP_Clip\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

#################################################### NDVI using custom function######################
# calculate ndvi using the overlay function

diff_rasters <- function(b1, b2){
  # this function calculates the difference between two rasters of the same CRS and extent
  # input: 2 raster layers of the same extent, crs that can be subtracted
  # output: a single different raster of the same extent, crs of the input rasters
  normalized_diff <- (b2 - b1)/(b2+b1)
  return(normalized_diff)
}
# you will have to create the function on your own!
naip_ndvi_ov <- overlay(NAip[[1]],
                        NAip[[4]],
                        fun = diff_rasters)

plot(naip_ndvi_ov,
     main = "NAIP NDVI")

filename <- ("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\NAIP\\NDVI.tif")
writeRaster(naip_ndvi_ov, filename = filename, format = "GTiff")
############### Crop ###########################

############## NDVI


Ponderosa <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots", layer = "Plot_12m")
site_spdf1 <- spTransform(Ponderosa, crs(naip_ndvi_ov))
plot(site_spdf1)

# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "LINK"])
  
  # Clip the raster by the current feature
  cr <- crop(naip_ndvi_ov, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NDVI_raster\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}
