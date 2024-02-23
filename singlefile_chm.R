

rm(list=ls(all=TRUE))
library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(lidR)
library(RStoolbox)
library(sf)
library(sp)
library(mapview)


  las1<- readLAS("S:\\Lassen_data\\Lassen_Lidar\\AOI\\AOI_Lidar\\Plot_45.las")
  las <- classify_noise(las1, sor(100,1))
  las_denoise <- filter_poi(las, Classification != LASNOISE)
  las_ground <- classify_ground(las_denoise, algorithm = csf())
  dtm_tin <- rasterize_terrain(las_ground , res =0.6, algorithm = tin())
  normal<-las_denoise- dtm_tin 
  chm <- rasterize_canopy(normal, res = 0.6, algorithm = p2r(subcircle = 0.25))
  ttops <- locate_trees(normal, lmf(ws = 3.5))
  algo <- dalponte2016(chm, ttops)
  las1 <- segment_trees(normal, algo)
  crowns <- crown_metrics(las1, func = .stdmetrics, geom = "convex")
########### convert to spatial polygon data so that extraction is easier later
  shape<-st_as_sf(crowns)
  st_write(shape,("S:\\Lassen_data\\Lassen_Lidar\\AOI\\CHM\\Chm_45.shp"))
  