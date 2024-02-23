library(lidR)
library(sf)
library(sp)

files <- list.files("D:\\LiDAr_viewer\\Alex_data\\ground\\normal", pattern = ".laz")
for (i in 1:length(files)) {
  las1 <- readLAS(paste("D:\\LiDAr_viewer\\Alex_data\\ground\\normal\\Normal_", i, ".laz", sep = ""))
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(las1, lmf(ws = 5))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics, geom = "convex")
  crowns_df <- data.frame(crowns) # Convert to a data.frame
  crowns_sf <- st_as_sf(crowns, coords = c("xcoord", "ycoord"), 
                        crs = st_crs("+init=epsg:32618"))
  st_write(crowns_sf, paste0("D:\\LiDAr_viewer\\Alex_data\\ground\\normal2\\Chm_",i,".shp"))
  #write.csv(crowns, paste0("D:\\LiDAr_viewer\\Alex_data\\ground\\normal1\\Chm_", i, ".csv"), row.names = FALSE)
}
