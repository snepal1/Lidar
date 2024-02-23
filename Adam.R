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
library(mapview)

#################### Bring in the data
ctg<-readALSLAScatalog("S:\\Lassen_data\\Lassen_Lidar\\All_Lidar")
summary(ctg)


ROI<-readOGR(dsn ="S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest",layer = "Export_Output")

site_spdf1<-spTransform(ROI,crs(ctg))
rois <- clip_roi(ctg,site_spdf1)
las <- classify_noise(rois, sor(100,1))
las_denoise <- filter_poi(las, Classification != LASNOISE)
las_ground <- classify_ground(las_denoise, algorithm = csf())
dtm_tin <- rasterize_terrain(las_ground , res =0.5, algorithm = tin())
normal<-las_denoise- dtm_tin 

writeLAS(normal, "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\normal1.las")
chm <- rasterize_canopy(normal, res = 0.5, algorithm = p2r(subcircle = 0.15))
ttops <- locate_trees(normal, lmf(ws = 2))


plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

algo <- dalponte2016(chm, ttops)
las1 <- segment_trees(normal, algo)
# x <-  las1@data$X
# y <-  las1@data$Y
#xy_coord <- data.frame(x, y)
crowns <- crown_metrics(las1, func = .stdmetrics)
write.csv(crowns,paste0("S:\\Lassen_data\\Lassen_Lidar\\Doug_fir\\Co-ordinates\\Chm_",i,".csv"))

########################## Write them as the shape file #######################################
crowns <- crown_metrics(las1, func = .stdmetrics, geom = "convex")
shape<-as_Spatial(crowns)########### convert to spatial polygon data so that extraction is easier later
shape<-st_as_sf(crowns)
st_write(shape,"S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\tree.shp")
############################################################