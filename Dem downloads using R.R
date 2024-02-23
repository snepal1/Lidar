
#Step1: Read in the csv file with the url links

data <- read.csv("S:\\Web_scrapping\\data _dem.csv", header=T)

# create a vector or list of urls to look throug

v1<- c(data$Url)
leng_f<-length(v1)

#str(leng_f)

# set the output directory path where you want to save the downloaded file

output_dir <- "S:\\Web_scrapping\\Erik"

# loop through entire URL links and download them one by one to the folder

for (i in 1:leng_f) {
  url <- v1[i]
  output_file <- paste0(output_dir, "\\data_", i, ".tif")
  download.file(url, destfile = output_file)
}

###################### End of code #################################################################
library(raster)
library(terra)

# Directory containing raster files
raster_dir <- "S:\\Web_scrapping\\Erik"

# Get a list of all raster files with .tif extension in the directory
raster_files <- list.files("S:\\Web_scrapping\\Erik\\", pattern = ".tif$", all.files = T, full.names = FALSE)

# Open all raster files
src_files_to_mosaic <- lapply(raster_files,raster)

# Mosaic the raster files
mosaic_raster <- do.call(mosaic, src_files_to_mosaic)

# Create the output mosaic file
output_mosaic_path <- "S:/Lassen_data/SPI_CHM/LiDAR_process/CHM/Merge_mosaic.tif"
writeRaster(mosaic_raster, filename = output_mosaic_path, format = "GTiff", overwrite = TRUE)

# Print information about the mosaic raster
print(mosaic_raster)
