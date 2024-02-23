

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


NAip <- stack("S:\\Lassen_data\\NAIP\\mergedNAIP.tif")
names(NAip) <- c("red_band", "green_band", "blue_band", "near_infrared_band")

plot(NAip,
     col = gray(0:100 / 100),
     box = FALSE, axes = FALSE)

plotRGB(NAip, r = 1, g = 2, b = 3, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")

# convert data into rasterbrick for faster processing
#naip_br <- brick(NAip)
#################### Calculate NDVI ################
#naip_ndvi <- ((naip_br[[4]] - naip_br[[1]]) / (naip_br[[4]] + naip_br[[1]]))

#plot(naip_ndvi,
     #main = "NAIP NDVI calculated using the subtract function")
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

############### Plot using ggplot###########################

#Calculate the GNDVI


diff_rasters1 <- function(b1, b2){
  # this function calculates the difference between two rasters of the same CRS and extent
  # input: 2 raster layers of the same extent, crs that can be subtracted
  # output: a single different raster of the same extent, crs of the input rasters
  normalized_diff1 <- (b2 - b1)/(b2+b1)
  return(normalized_diff1)
}
# you will have to create the function on your own!
naip_gndvi <- overlay(NAip[[2]],
                        NAip[[4]],
                        fun = diff_rasters1)

plot(naip_gndvi,
     main = "NAIP GNDVI")


################### Near-infrared reflectance########
nir_reflectance<-(NAip[[4]]-minValue(NAip[[4]]))/(maxValue(NAip[[4]])-minValue(NAip[[4]]))
  plot(nir_reflectance)




  ############### calculate DEVI#################

ptime <- proc.time()
diff_rasters2 <- function(b1, b2, b3){
  # this function calculates the difference between two rasters of the same CRS and extent
  # input: 2 raster layers of the same extent, crs that can be subtracted
  # output: a single different raster of the same extent, crs of the input rasters
  DEVI <- (b3/3*b3)+(b1/3*b1)+(b3/3*b3)
  return(DEVI)
}
# you will have to create the function on your own!
naip_devi <- overlay(NAip[[1]],
                      NAip[[2]],
                      NAip[[3]],
                      fun = diff_rasters2)

ptime <- proc.time() - ptime
plot(naip_devi,
     main = "NAIP DEVI")


print(ptime)
################################# Green leaf index ########################

diff_rasters3 <- function(b1, b2, b3){
  # this function calculates the difference between two rasters of the same CRS and extent
  # input: 2 raster layers of the same extent, crs that can be subtracted
  # output: a single different raster of the same extent, crs of the input rasters
Green_leaf <- ((2*((b2)/(b1+b2+b3)))-((b1)/(b1+b2+b3))-((b3)/(b1+b2+b3)))/((2*((b2)/(b1+b2+b3)))+((b1)/(b1+b2+b3))+((b3)/(b1+b2+b3)))
  return(Green_leaf)
}
# you will have to create the function on your own!
naip_gli <- overlay(NAip[[1]],
                      NAip[[2]],
                      NAip[[3]],
                      fun = diff_rasters3)
plot(naip_gli,
     main = "GLI")
########################################################################################################

################### calculate the texture contrast #####################
red_band<-NAip[[4]]

contrast <- focal(red_band, w=matrix(0.6, nrow=3, ncol=3), fun=var, na.rm=TRUE)
plot(contrast)
#Raster cropping to the plot extent for each species

############### Clipping to the plot extent######################################
# Load the raster

# Load the shapefile
Ponderosa <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\AOI", layer = "Grid_150")
site_spdf1<-spTransform(Ponderosa,
                        crs(naip_gndvi))
plot(site_spdf1)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Clip the raster by the current feature
  cr <- crop(naip_gndvi, site_spdf1[i,])
  
  # Save the output raster
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\NDVI_100\\NDVI_", i, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

###################### GNVI for plots###################

site_spdf2<-spTransform(Ponderosa,
                        crs(naip_gndvi))
plot(site_spdf2)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf2)) {
  
  # Clip the raster by the current feature
  cr <- crop(naip_gndvi, site_spdf2[i,])
  
  # Save the output raster
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\GNDVI_100\\GNDVI_", i, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

###################################
###################### Reflectance for plots###################

site_spdf3<-spTransform(Ponderosa,
                        crs(nir_reflectance))
plot(site_spdf3)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf3)) {
  
  # Clip the raster by the current feature
  cr <- crop(nir_reflectance, site_spdf3[i,])
  
  # Save the output raster
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Reflectance_100\\Ref_", i, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

######################### GLI#####################
site_spdf4<-spTransform(Ponderosa,
                        crs(naip_gli))
plot(site_spdf4)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf4)) {
  
  # Clip the raster by the current feature
  cr <- crop(naip_gli, site_spdf4[i,])
  
  # Save the output raster
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\GLI_100\\GLI_", i, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

################### Extract DEVI
site_spdf5<-spTransform(Ponderosa,
                        crs(contrast))
plot(site_spdf5)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf5)) {
  
  # Clip the raster by the current feature
  cr <- crop(contrast, site_spdf5[i,])
  
  # Save the output raster
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Contrast_100\\Contrast_", i, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}


################################### Extract DEVI
################### Extract DEVI
site_spdf6<-spTransform(Ponderosa,
                        crs(naip_devi))
plot(site_spdf6)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf6)) {
  
  # Clip the raster by the current feature
  cr <- crop(naip_devi, site_spdf6[i,])
  
  # Save the output raster
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\DEVI_100\\DEVI_", i, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}
####################### 
# Export your raster
#writeRaster(x = naip_ndvi_ov,
            #filename="S:\\Lassen_data\\NAIP\\LAssen_ndvi.tif",
            #format = "GTiff", # save as a tif
            #datatype='INT1U') # save as a INTEGER rather than a float"
