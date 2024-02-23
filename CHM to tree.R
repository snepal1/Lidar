# Load the required libraries
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
  crowns <- crown_metrics(las, func = .stdmetrics)
  #st_write(shape,paste0("D:\\LiDAr_viewer\\Alex_data\\ground\\normal2\\Chm_",i,".shp"))
  write.csv(crowns, paste0("D:\\LiDAr_viewer\\Alex_data\\ground\\normal1\\Chm_", i, ".csv"), row.names = FALSE)
}


#######################
files <- list.files("D:\\LiDAr_viewer\\Alex_data\\ground\\normal", pattern = ".laz")
for (i in 1:length(files)) {
  las1 <- readLAS(paste("D:\\LiDAr_viewer\\Alex_data\\ground\\normal\\Normal_", i, ".laz", sep = ""))
  chm <- rasterize_canopy(las1, res = 0.6, algorithm = p2r(subcircle = 0.15))
  ttops <- locate_trees(las1, lmf(ws = 5))
  algo <- dalponte2016(chm, ttops)
  las <- segment_trees(las1, algo)
  crowns <- crown_metrics(las, func = .stdmetrics)
  crowns <- crowns[, colSums(is.na(crowns)) == 0] # remove columns with all NA values
  crowns <- crowns[, !grepl("^\\s*$", colnames(crowns))] # remove unnamed columns
  crowns <- crowns[, !grepl("^geometry$", colnames(crowns))] # remove geometry column
  write.csv(crowns, paste0("D:\\LiDAr_viewer\\Alex_data\\ground\\normal3\\Chm_", i, ".csv"), row.names = FALSE)
}

