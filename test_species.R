

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

files <- list.files("S:\\Lassen_data\\Lassen_Lidar\\AOI", pattern = ".laz")
for (i in 1:length (files)) {
  las1<- readLAS(paste("S:\\Lassen_data\\Lassen_Lidar\\AOI\\AOI", ".laz", sep= ""))
  las <- classify_noise(las1, sor(100,1))
  las_denoise <- filter_poi(las, Classification != LASNOISE)
  las_ground <- classify_ground(las_denoise, algorithm = csf())
  dtm_tin <- rasterize_terrain(las_ground , res =0.6, algorithm = tin())
  normal<-las_denoise- dtm_tin 
  chm <- rasterize_canopy(normal, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(normal, lmf(ws =3))
  algo <- dalponte2016(chm, ttops)
  las1 <- segment_trees(normal, algo)
  crowns <- crown_metrics(las1, func = .stdmetrics, geom = "convex")
  shape<-as_Spatial(crowns)########### convert to spatial polygon data so that extraction is easier later
  shape<-st_as_sf(crowns)
  st_write(shape,paste0("S:\\Lassen_data\\Lassen_Lidar\\AOI\\Chm_",i,".shp"))
  
}
