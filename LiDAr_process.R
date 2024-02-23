

library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(lidR)
library(sf)
library(sp)
library(mapview)

################### Get both data###########################
ctg<-readALSLAScatalog("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\ground\\normal")
#ctg<-readALSLAScatalog("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI")
#ctg<-readALSLAScatalog("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Lidar")
summary(ctg)
########## get THE plots
las_check(ctg)

#plots<-read.csv("S:\\Lassen_data\\Lassen_Lidar\\Species Recognition Plots\\SINGLE_SPECIES.csv")
#str(plots)
#plots$DISTRICT
library(dplyr)
#site_spdf <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Ponerosa\\", layer = "Square_plots")
#site_spdf <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Combine", layer = "Merge")
site_spdf <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam", layer = "New")

# Transform site_spdf to match the CRS of ctg


mapview(as.spatial(ctg))+mapview(site_spdf)
###############################################################################################################################

# Loop over unique labels
for (label in unique(site_spdf$LINK)) {
  # Subset the shapefile and change projection to match LiDAR
  site_spdf2 <- subset(site_spdf, LINK == label)
  #site_spdf2 <- spTransform(site_spdf2, crs(ctg))
  
  # Set output file names for the clipped LiDAR data
  opt_output_files(ctg) <- gsub("Plot_", "", paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\AOI_12\\Plot_", label))
  
  # Clip the LiDAR data using the shapefile
  rois <- clip_roi(ctg, site_spdf2, radius=12)
}

# Loop over unique labels
for (label in unique(site_spdf$FID)) {
  # Subset the shapefile and change projection to match LiDAR
  site_spdf2 <- subset(site_spdf, FID == label)
  #site_spdf2 <- spTransform(site_spdf2, crs(ctg))
  
  # Set output file names for the clipped LiDAR data
  opt_output_files(ctg) <- gsub("Plot_", "", paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\Plot_", label))
  
  # Clip the LiDAR data using the shapefile
  rois <- clip_roi(ctg, site_spdf2)
}


# Set the folder path
folder_path <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area"

# Get the list of files in the folder
files <- list.files(folder_path, full.names = TRUE)

# Retrieve file information
file_info <- file.info(files)

# Filter files by size (less than 50 KB)
small_files <- files[file_info$size < 200 * 1024]

# Delete the small files
file.remove(small_files)


############################################ create the treelist in each plot and calculate the lidar metrics for each plot##########
# Load the required libraries

files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\AOI_normal1", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\AOI_normal1\\", las_file, sep = "")
  las1 <- readLAS(las_path)
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.12))
  ttops <- locate_trees(las1, lmf(ws = 3))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics)
  csv_file <- gsub(".las", "", basename(las_file))
  # Write CSV file with the same name as the LAS file
  csv_file <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\CHM\\", csv_file , ".csv")
  write.csv(crowns, csv_file, row.names = FALSE)
}


files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\AOI_normal1", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\AOI_normal1\\", las_file, sep = "")
  las1 <- readLAS(las_path, filter = "-keep_z 3 50")
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(las1, lmf(ws = 3))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics, geom="convex")
  crowns_df <- data.frame(crowns) # Convert to a data.frame
  crowns_sf <- st_as_sf(crowns, coords = c("xcoord", "ycoord"), 
                        crs = st_crs("+init=epsg:32618"))
  csv_file <- gsub(".las", "", basename(las_file))
  st_write(crowns_sf, paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots16\\CHM_shape\\",csv_file,".shp"))
 
}




######################## from 14 m plots###############




files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\AOI_normal", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\AOI_normal\\", las_file, sep = "")
  las1 <- readLAS(las_path, filter = "-keep_z 3 50")
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.2))
  ttops <- locate_trees(las1, lmf(ws = 1.8))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics)
  csv_file <- gsub(".las", "", basename(las_file))
  # Write CSV file with the same name as the LAS file
  csv_file <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\CHM\\", csv_file , ".csv")
  write.csv(crowns, csv_file, row.names = FALSE)
}



files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\AOI_normal", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\AOI_normal\\", las_file, sep = "")
  las1 <- readLAS(las_path, filter = "-keep_z 3 50")
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(las1, lmf(ws = 3))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics, geom="convex")
  crowns_df <- data.frame(crowns) # Convert to a data.frame
  crowns_sf <- st_as_sf(crowns, coords = c("xcoord", "ycoord"), 
                        crs = st_crs("+init=epsg:32618"))
  csv_file <- gsub(".las", "", basename(las_file))
  st_write(crowns_sf, paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots14\\CHM_shape\\",csv_file,".shp"))
  
}

######################## from 12 m plots###############


f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 5] <- 3
  y[x > 40] <- 11
  return(y)
}
heights <- seq(-5,30,0.5)
ws <- f(heights)
plot(heights, ws, type = "l",  ylim = c(0,15))

files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\", las_file, sep = "")
  las1 <- readLAS(las_path, filter = " -drop_z_below 4 -drop_z_above 50")
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.10))
  kernel <- matrix(1,3,3)
  smoothed <- terra::focal(chm , w = kernel, fun = median, na.rm = TRUE)
  ttops <- locate_trees(smoothed, lmf(ws=1.68))
  algo <- dalponte2016(smoothed, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics)
  csv_file <- gsub(".las", "", basename(las_file))
  # Write CSV file with the same name as the LAS file
  csv_file <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM1.65\\", csv_file , ".csv")
  write.csv(crowns, csv_file, row.names = FALSE)
  
  
  
}


files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\", las_file, sep = "")
  las1 <- readLAS(las_path)
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.10))
  ttops <- locate_trees(las1, lmf(ws = 1.68))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics, geom="convex")
  crowns_df <- data.frame(crowns) # Convert to a data.frame
  crowns_sf <- st_as_sf(crowns, coords = c("xcoord", "ycoord"), 
                        crs = st_crs("+init=epsg:32618"))
  csv_file <- gsub(".las", "", basename(las_file))
  st_write(crowns_sf, paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred\\",csv_file,".shp"))
  
}


########################################## FOr ground truthing plots#####################
######################## from 12 m plots###############



files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\AOI_12", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\AOI_12\\", las_file, sep = "")
  las1 <- readLAS(las_path, filter = " -drop_z_below 4 -drop_z_above 50")
  
  # Compute CHM, tree segmentation, and crown metrics
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.2))
  kernel <- matrix(1,3,3)
  smoothed <- terra::focal(chm , w = kernel, fun = median, na.rm = TRUE)
  ttops <- locate_trees(smoothed, lmf(ws=2.5))
  algo <- dalponte2016(smoothed, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics)
  csv_file <- gsub(".las", "", basename(las_file))
  # Write CSV file with the same name as the LAS file
  csv_file <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\AOI_12\\CHM1\\", csv_file , ".csv")
  write.csv(crowns, csv_file, row.names = FALSE)
  
  
  
}


#########################


  
files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area", pattern = ".las")
  
  for (i in 1:length(files)) {
    # Read LAS file and filter out points below 2m
    las_file <- files[i]
    las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\", las_file, sep = "")
    las1 <- readLAS(las_path)
    
    # Compute CHM, tree segmentation, and crown metrics
    chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.09))
    ttops <- locate_trees(las1, lmf(ws = 3))
    algo <- dalponte2016(chm, ttops)
    las <- segment_trees(las1, algo)
    crowns <- crown_metrics(las, func = .stdmetrics, geom="convex")
    crowns_df <- data.frame(crowns) # Convert to a data.frame
    crowns_sf <- st_as_sf(crowns, coords = c("xcoord", "ycoord"), 
                          crs = st_crs("+init=epsg:32618"))
    csv_file <- gsub(".las", "", basename(las_file))
    st_write(crowns_sf, paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape\\",csv_file,".shp"))
    
  }
  

################################################### Area-based: compute cloud metrics ##################
mkrf_plot_metrics <- data.frame()

files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area", pattern = ".las")

for (i in 1:length(files)) {
  # Read LAS file and filter out points below 2m
  las_file <- files[i]
  las_path <- paste("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\", las_file, sep = "")
  las1 <- readLAS(las_path, filter = "-keep_first -drop_z_below 5 -drop_z_above 50")
  
  # Compute CHM, tree segmentation, and crown metrics
  
  metrics <- cloud_metrics(las1, func = .stdmetrics)
  
  # Add the LAS filename (without extension) as a new column in metrics
  metrics$LAS_filename <- tools::file_path_sans_ext(las_file)
  
  mkrf_plot_metrics <- rbind(mkrf_plot_metrics, metrics)
  
  write.csv(mkrf_plot_metrics, "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Predicting_area\\CHM7\\Plot.csv")
}
