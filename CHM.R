rm(list=ls(all=TRUE))

######## Load the required library

library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(lidR)
library(RStoolbox)
library(sf)
library(sp)
library(terra)

#################### Bring in the data
#a<-readLAS("S:\\Lassen_data\\Lassen_Lidar\\White_fir\\Normal\\White_ 1 .las")

files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\Doug_fir", pattern = ".las")
for (i in 1:length (files)) {
  las1<- readLAS(paste("S:\\Lassen_data\\Lassen_Lidar\\Doug_fir\\Plot_", i, ".las", sep= ""))
  las <- classify_noise(las1, sor(100,1))
  las_denoise <- filter_poi(las, Classification != LASNOISE)
  las_ground <- classify_ground(las_denoise, algorithm = csf())
  dtm_tin <- rasterize_terrain(las_ground , res =0.6, algorithm = tin())
  normal<-las_denoise- dtm_tin 
  chm <- rasterize_canopy(normal, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(normal, lmf(ws = 5))
  algo <- dalponte2016(chm, ttops)
  las1 <- segment_trees(normal, algo)
 # x <-  las1@data$X
 # y <-  las1@data$Y
  #xy_coord <- data.frame(x, y)
  crowns <- crown_metrics(las1, func = .stdmetrics)
  #x<-crowns@geometry$X
  #y<-crowns@geometry$y
  #xy_coord <- data.frame(x, y)
  write.csv(crowns,paste0("S:\\Lassen_data\\Lassen_Lidar\\Doug_fir\\Co-ordinates\\Chm_",i,".csv"))
  
}


files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\White_fir\\Normal", pattern = ".las", full.names = TRUE)
for (i in 1:length(files)) {
  las <- readLAS("S:\\Lassen_data\\Lassen_Lidar\\White_fir\\Normal\\White_", i, ".las")
  chm <- rasterize_canopy(las, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(las, lmf(ws = 5))
  algo <- dalponte2016(chm, ttops)
  las1 <- segment_trees(las, algo)
  crowns <- crown_metrics(las1, func = .stdmetrics, geom = "convex")
  shape<-as_Spatial(crowns)########### convert to spatial polygon data so that extraction is easier later
  coord<-coordinates(shape)
  shape<-st_as_sf(crowns)
  st_write(shape,paste0("S:\\Lassen_data\\Lassen_Lidar\\White_fir\\CHM\\Chm_",i,".shp"))
  
}


############################ Do one at a time ################

las <- readLAS("S:\\Lassen_data\\Lassen_Lidar\\Ponerosa\\Normal\\Pondo_ 1 .las",filter = "-keep_first -drop_z_below 0 -drop_z_above 80")
plot(las, size = 3, bg = "white")

################## Create a cCHM using point to raster algorithem##################
chm <- rasterize_canopy(las, res = 0.6, algorithm = p2r(subcircle = 0.15,na.fill = tin()))
col <- height.colors(25)
plot(chm, col = col)
############# Identify trees #############
ttops <- locate_trees(las, lmf(ws = 5))
############## Plot chm over tree ########
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)
#######################################################
algo <- dalponte2016(chm, ttops)
las1 <- segment_trees(las, algo)
plot(las1, bg = "white", size = 4, color = "treeID") # visualize trees

crowns <- crown_metrics(las1, func = .stdmetrics, geom = "convex")
shape<-as_Spatial(crowns)########### convert to spatial polygon data so that extraction is easier later
plot(shape)
shape<-st_as_sf(crowns)
st_write(shape,"S:\\Lassen_data\\Lassen_Lidar\\Ponerosa_square\\Normal\\LIDAr", "Plot_2(a)", driver="ESRI Shapefile")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")
metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics


###################### CHM based tree segmenation

#algo1 <- dalponte2016(chm, ttops)
#crowns1 <- algo1()
#plot(crowns1, col = pastel.colors(200))



#writeRaster(metrics1$pzabove2, file="S:\\Lassen_data\\Lassen_Lidar\\Ponerosa_square\\Normal\\LIDAr\\PLOT1_LIDAR.tif", overwrite=TRUE)


##################### zonal statistics to extract the NDVI mean value to the tree identified #######################


############## NDVI
# Get a list of the raster and polygon files in each folder
#raster_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\AOI\\NDVI", pattern = ".tif")
polygon_files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3", pattern = ".shp")



for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  raster1 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\NDVI\\NDVI_", i, ".tif", sep= ""))
  polygon <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf1<-spTransform(polygon ,
                          crs(raster1))

  # Extract the raster data using the polygon
  site_spdf1@data$NDVI<-NA
  site_spdf1@data$NDVI_SD<-NA
  
  values <- c(extract(raster1, site_spdf1, fun=mean))
  values1 <- c(extract(raster1, site_spdf1, fun=sd))
  site_spdf1$NDVI<-values
  site_spdf1$NDVI_SD<-values1
  site_df <- as.data.frame(site_spdf1)
  write.csv(site_df, file= paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Exracted_NDVI\\NDVI_",i, ".csv"))
}

##############################GNDVI
for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  raster2 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\GNDVI\\GNDVI_", i, ".tif", sep= ""))
  polygon2 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf2<-spTransform(polygon2 ,
                          crs(raster2))
  
  # Extract the raster data using the polygon
  site_spdf2@data$GNDVI<-NA
  site_spdf2@data$GNDVI_SD<-NA
  
  values2 <- c(extract(raster2, site_spdf2, fun=mean))
  values3 <- c(extract(raster2, site_spdf2, fun=sd))
  site_spdf2$GNDVI<-values2
  site_spdf2$GNDVI_SD<-values3
  site_dfa <- as.data.frame(site_spdf2)
  write.csv(site_dfa, file= paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Extracted_GNDVI\\GNDVI_",i, ".csv"))
}

#################### GLI

for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  raster3 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\GLI\\GLI_", i, ".tif", sep= ""))
  polygon3 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf3<-spTransform(polygon3 ,
                          crs(raster3))
  
  # Extract the raster data using the polygon
  site_spdf3@data$GLI<-NA
  site_spdf3@data$GLI_SD<-NA
  
  values4 <- c(extract(raster3, site_spdf3, fun=mean))
  values5 <- c(extract(raster3, site_spdf3, fun=sd))
  site_spdf3$GLI<-values4
  site_spdf3$GLI_SD<-values5
  site_dfb <- as.data.frame(site_spdf3)
  write.csv(site_dfb, file= paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Extracted_GLI\\GLI_",i, ".csv"))
}

   ####### DEVI 

for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  raster4 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\DEVI\\DEVI_", i, ".tif", sep= ""))
  polygon4 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf4<-spTransform(polygon4 ,
                          crs(raster4))
 
  # Extract the raster data using the polygon
  site_spdf4@data$DEVI<-NA
  site_spdf4@data$DEVI_SD<-NA

  values6 <- c(extract(raster4, site_spdf4, fun=mean))
  values7 <- c(extract(raster4, site_spdf4, fun=sd))
  site_spdf4$DEVI<-values6
  site_spdf4$DEVI_SD<-values7
  site_dfc <- as.data.frame(site_spdf4)
  write.csv(site_dfc, file= paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Extracted_DEVI\\DEVI_",i, ".csv"))
}

############# Reflectance

for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  raster5 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Reflectance\\Ref_", i, ".tif", sep= ""))
  polygon5 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf5<-spTransform(polygon5 ,
                          crs(raster5))
  
  # Extract the raster data using the polygon
  site_spdf5@data$Ref<-NA
  site_spdf5@data$Ref_SD<-NA
  
  values8 <- c(extract(raster5, site_spdf5, fun=mean))
  values9 <- c(extract(raster5, site_spdf5, fun=sd))
  site_spdf5$Ref<-values8
  site_spdf5$Ref_SD<-values9
  site_dfd <- as.data.frame(site_spdf5)
  write.csv(site_dfd, file= paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Extracted_Ref\\Ref_",i, ".csv"))
}

################## contrast
for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  raster6 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Contrast\\Contrast_", i, ".tif", sep= ""))
  polygon6 <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf6<-spTransform(polygon6 ,
                          crs(raster6))
  
  # Extract the raster data using the polygon
  site_spdf6@data$Con<-NA
  site_spdf6@data$Con_SD<-NA
  
  values10 <- c(extract(raster6, site_spdf6, fun=mean))
  values11<- c(extract(raster6, site_spdf6, fun=sd))
  site_spdf6$Con<-values10
  site_spdf6$Con_SD<-values11
  site_dfe <- as.data.frame(site_spdf6)
  write.csv(site_dfe, file= paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Extracted_con\\Con_",i, ".csv"))
}


########################## Individual #######################

#site_spdf <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Ponerosa_square\\Normal\\LIDAr\\", layer = "Plot_2(a)")

raster<-raster("S:\\Lassen_data\\Lassen_Lidar\\Doug_fir\\Contrast\\Doug_con_7.tif")

#plot(raster)
shape<- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Doug_fir\\CHM", layer = "Chm_7")
#plot(shape)
site_spdf1<-spTransform(shape,
                        crs(raster))

###################################################create a new field in the spatial data frame ###########################################################################################

site_spdf1@data$Con<-NA
site_spdf1@data$Con_SD<-NA

values <- c(extract(raster, site_spdf1, fun=mean))
values1 <- c(extract(raster, site_spdf1, fun=sd))
site_spdf1$Con<-values


site_spdf1$Con_SD<-values1

write.csv(site_spdf1,"S:\\Lassen_data\\Lassen_Lidar\\Doug_fir\\Extracted_con\\Doug_7_con.csv")

##########################################################################################################################################

########## Merge multip;e files together##############

# get list of all csv files in directory
csv_files <- list.files(path = "S:\\Lassen_data\\Lassen_Lidar\\White_fir\\Extracted_con", pattern = "*.csv", full.names = TRUE)

# load each csv file into a list of data frames
df_list <- lapply(csv_files, read.csv)

# combine the list of data frames into one data frame
final_df <- do.call(rbind, df_list)
write.csv(final_df, "S:\\Lassen_data\\Lassen_Lidar\\White_fir\\Extracted_con\\White_COn.csv")



############################# Pixel based calculations####################

metrics1 <- pixel_metrics(las, .stdmetrics, 0.6) # calculate standard metrics
#plot(metrics1$pzabove2, col = height.colors(50))
nbands<-nlyr(metrics1)
# Loop through each feature in the shapefile
for (i in seq(1, nbands)) {
  #for (i in 1:length(nbands))
  
  # Extract values for band
  values <- extract(metrics1, i)
  # Create new raster object
  ras_band <- raster(values, xmn=xmin(metrics1), xmx=xmax(metrics1), ymn=ymin(metrics1),ymx=ymax(metrics1))
  # Save raster to file
  writeRaster(ras_band, filename=paste0("S:\\Lassen_data\\Lassen_Lidar\\Ponerosa_square\\Normal\\LIDAr\\band_", i, ".tif"), format="GTiff", overwrite=TRUE)
}


############### looping thorugh polygon and writing a shapefiel code######################
for (i in 1:length(polygon_files)) {
  
  # Read in the raster and polygon files for this iteration
  #raster1 <- raster(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\NDVI\\NDVI_", i, ".tif", sep= ""))
  polygon <- readOGR(paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM_3\\CHM_", i, ".shp", sep= ""))
  
  site_spdf1<-spTransform(polygon ,
                          crs(raster1))
  
  # Extract the raster data using the polygon
  site_spdf1@data$NDVI<-NA
  site_spdf1@data$NDVI_SD<-NA
  
  values <- c(extract(raster1, site_spdf1, fun=mean))
  values1 <- c(extract(raster1, site_spdf1, fun=sd))
  site_spdf1$NDVI<-values
  site_spdf1$NDVI_SD<-values1
  #site_df <- as.data.frame(site_spdf1)
  writeOGR(site_spdf1, dsn= "S:\\Lassen_data\\Lassen_Lidar\\AOI\\Exracted_NDVI", 
           paste0("NDVI_",i), driver="ESRI Shapefile")
}