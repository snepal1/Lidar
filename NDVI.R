
rm(list = ls())
# load spatial packages
library(raster)
library(rasterVis)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(rgeos)
library(sf)
library(mapview)

################### Read in data##############################################

#NAip <- brick("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\NDVI.tif")
NAip <- brick("D:\\Justin_coast\\Image\\Mosaic1.tif")
#NAip <- brick("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\NAIP\\Adam_2020_cropped.tif")
names(NAip) <- c("red_band", "green_band", "blue_band", "near_infrared_band")

plot(NAip,
     col = gray(0:100 / 100),
     box = FALSE, axes = FALSE)

plotRGB(NAip, r = 1, g = 2, b = 3, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")


mapview(NAip)

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

filename <- ("D:\\Justin_coast\\Image\\NDVI_clip.tif")
writeRaster(naip_ndvi_ov, filename = filename, format = "GTiff")
############### Plot using ggplot###########################

######################################### Clip NDVI to plots ####################

############### Clipping to the plot extent######################################
# Load the raster

# Load the shapefile
site_spdf1 <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam", layer = "50M-GRID")
mapview(site_spdf1)+mapview(naip_ndvi_ov)
plot(site_spdf1)
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "FID"])
  
  # Clip the raster by the current feature
  cr <- crop(naip_ndvi_ov, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NDVI_raster_pred\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}


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

#filename <- ("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\GNDVI_clip.tif")
#writeRaster(naip_gndvi, filename = filename, format = "GTiff")


############################### crop GNDVI_raster###########################


# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "FID"])
  
  # Clip the raster by the current feature
  cr <- crop(naip_gndvi, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GNDVI_raster_pred\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}



################### Near-infrared reflectance########
nir_reflectance<-(NAip[[4]]-minValue(NAip[[4]]))/(maxValue(NAip[[4]])-minValue(NAip[[4]]))
plot(nir_reflectance)

#filename <- ("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Ref_clip.tif")
#writeRaster(nir_reflectance, filename = filename, format = "GTiff")

################################## CLip NIR###################

# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "FID"])
  
  # Clip the raster by the current feature
  cr <- crop(nir_reflectance, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NIR_raster_pred\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}

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

#filename <- ("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\DEVI_clip.tif")
#writeRaster(naip_devi, filename = filename, format = "GTiff")

########################### Crop raster#########################
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "FID"])
  
  # Clip the raster by the current feature
  cr <- crop(naip_devi, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\DEVI_raster_pred\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}



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

#filename <- ("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\GLI_clip.tif")
#writeRaster(naip_gli, filename = filename, format = "GTiff")
########################################################################################################
############################# Crop GLI ######################################

########################### #########################
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "FID"])
  
  # Clip the raster by the current feature
  cr <- crop(naip_gli, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GLI_raster_pred\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}



################### calculate the texture contrast #####################
red_band<-NAip[[4]]

contrast <- focal(red_band, w=matrix(0.6, nrow=3, ncol=3), fun=var, na.rm=TRUE)
  plot(contrast)

#######################################################
############################# Crop contrast ######################################

########################### #########################
# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "FID"])
  
  # Clip the raster by the current feature
  cr <- crop(contrast, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Contrast_raster_pred\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}



#Raster cropping to the plot extent for each species
#filename <- ("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Contrast_clip.tif")
#writeRaster(contrast, filename = filename, format = "GTiff")


##################### zonal statistics to extract the NDVI mean value to the tree identified #######################
#################################### Parallel process ###########################
  library(foreach)
  library(doParallel)
  
  polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred", pattern = ".shp")
  raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NDVI_raster_pred", pattern = "\\.tif$")
  
  # Set the number of cores to use for parallel processing
  num_cores <- 8
  
  # Initialize a parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Perform the processing in parallel
  foreach(i = 1:length(polygon_files), .packages = c("raster", "rgdal")) %dopar% {
    # Read in the raster and polygon files for this iteration
    raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NDVI_raster_pred\\", raster_files[i]))
    site_spdf2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\", polygon_files[i]))
    
    # Extract the raster data using the polygon
    site_spdf2$GNDVI <- raster::extract(raster2, site_spdf2, fun = mean)
    site_spdf2$GNDVI_SD <- raster::extract(raster2, site_spdf2, fun = sd)
    
    # Get the base name of the shape file without the extension
    base_name <- tools::file_path_sans_ext(raster_files[i])
    
    # Write the extracted data as a CSV file
    write.csv(site_spdf2, file = paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NDVI_extracted_pred\\", base_name, ".csv"), row.names = FALSE)
  }
  
  # Stop the parallel backend
  stopCluster(cl)
  
################################ FOR GNDVI###############################
  
#################################### Parallel process ###########################

  
polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred", pattern = ".shp")
raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GNDVI_raster_pred", pattern = "\\.tif$")
  
  # Set the number of cores to use for parallel processing
num_cores <- 8
  
  # Initialize a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)
  
  # Perform the processing in parallel
  foreach(i = 1:length(polygon_files), .packages = c("raster", "rgdal")) %dopar% {
    # Read in the raster and polygon files for this iteration
    raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GNDVI_raster_pred\\", raster_files[i]))
    site_spdf2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\", polygon_files[i]))
    
    # Extract the raster data using the polygon
    site_spdf2$GNDVI <- raster::extract(raster2, site_spdf2, fun = mean)
    site_spdf2$GNDVI_SD <- raster::extract(raster2, site_spdf2, fun = sd)
    
    # Get the base name of the shape file without the extension
    base_name <- tools::file_path_sans_ext(raster_files[i])
    
    # Write the extracted data as a CSV file
    write.csv(site_spdf2, file = paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GNDVI_extracted_pred\\", base_name, ".csv"), row.names = FALSE)
  }

######################################################DEVI#############################
  # Stop the parallel backend
  stopCluster(cl)
############################# DEVI######################################
  
  polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred", pattern = ".shp")
  raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\DEVI_raster_pred", pattern = "\\.tif$")
  
  # Set the number of cores to use for parallel processing
  num_cores <- 8
  
  # Initialize a parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Perform the processing in parallel
  foreach(i = 1:length(polygon_files), .packages = c("raster", "rgdal")) %dopar% {
    # Read in the raster and polygon files for this iteration
    raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\DEVI_raster_pred\\", raster_files[i]))
    site_spdf2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\", polygon_files[i]))
    
    # Extract the raster data using the polygon
    site_spdf2$DEVI <- raster::extract(raster2, site_spdf2, fun = mean)
    site_spdf2$DEVI_SD <- raster::extract(raster2, site_spdf2, fun = sd)
    
    # Get the base name of the shape file without the extension
    base_name <- tools::file_path_sans_ext(raster_files[i])
    
    # Write the extracted data as a CSV file
    write.csv(site_spdf2, file = paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\DEVI_extracted_pred\\", base_name, ".csv"), row.names = FALSE)
  }
  
 
  # Stop the parallel backend
  stopCluster(cl)
  
  #################################################### GLI####################################
  
  ###################################################################
  
  polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred", pattern = ".shp")
  raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GLI_raster_pred", pattern = "\\.tif$")
  
  # Set the number of cores to use for parallel processing
  num_cores <- 8
  
  # Initialize a parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Perform the processing in parallel
  foreach(i = 1:length(polygon_files), .packages = c("raster", "rgdal")) %dopar% {
    # Read in the raster and polygon files for this iteration
    raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GLI_raster_pred\\", raster_files[i]))
    site_spdf2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\", polygon_files[i]))
    
    # Extract the raster data using the polygon
    site_spdf2$GLI <- raster::extract(raster2, site_spdf2, fun = mean)
    site_spdf2$GLI_SD <- raster::extract(raster2, site_spdf2, fun = sd)
    
    # Get the base name of the shape file without the extension
    base_name <- tools::file_path_sans_ext(raster_files[i])
    
    # Write the extracted data as a CSV file
    write.csv(site_spdf2, file = paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\GLI_extracted_pred\\", base_name, ".csv"), row.names = FALSE)
  }
  
  
  # Stop the parallel backend
  stopCluster(cl)
############################################ Contrast ##########################
  polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred", pattern = ".shp")
  raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Contrast_raster_pred", pattern = "\\.tif$")
  
  # Set the number of cores to use for parallel processing
  num_cores <- 8
  
  # Initialize a parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Perform the processing in parallel
  foreach(i = 1:length(polygon_files), .packages = c("raster", "rgdal")) %dopar% {
    # Read in the raster and polygon files for this iteration
    raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Contrast_raster_pred\\", raster_files[i]))
    site_spdf2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\", polygon_files[i]))
    
    # Extract the raster data using the polygon
    site_spdf2$Contarst <- raster::extract(raster2, site_spdf2, fun = mean)
    site_spdf2$Contarst_SD <- raster::extract(raster2, site_spdf2, fun = sd)
    
    # Get the base name of the shape file without the extension
    base_name <- tools::file_path_sans_ext(raster_files[i])
    
    # Write the extracted data as a CSV file
    write.csv(site_spdf2, file = paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Contarst_extracted_pred\\", base_name, ".csv"), row.names = FALSE)
  }
  
  
  # Stop the parallel backend
  stopCluster(cl)
     ############################################ REFlectance##########################
  ############################################ Contrast ##########################
  polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred", pattern = ".shp")
  raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NIR_raster_pred", pattern = "\\.tif$")
  
  # Set the number of cores to use for parallel processing
  num_cores <- 8
  
  # Initialize a parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Perform the processing in parallel
  foreach(i = 1:length(polygon_files), .packages = c("raster", "rgdal")) %dopar% {
    # Read in the raster and polygon files for this iteration
    raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NIR_raster_pred\\", raster_files[i]))
    site_spdf2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\", polygon_files[i]))
    
    # Extract the raster data using the polygon
    site_spdf2$NIR <- raster::extract(raster2, site_spdf2, fun = mean)
    site_spdf2$NIR_SD <- raster::extract(raster2, site_spdf2, fun = sd)
    
    # Get the base name of the shape file without the extension
    base_name <- tools::file_path_sans_ext(raster_files[i])
    
    # Write the extracted data as a CSV file
    write.csv(site_spdf2, file = paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NIR_extracted_pred\\", base_name, ".csv"), row.names = FALSE)
  }
  
  
  # Stop the parallel backend
  stopCluster(cl)
  #######################