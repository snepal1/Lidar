library(raster)
library(sf)


NAip <- stack("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\NAIP\\Adam_2020_cropped.tif")
names(NAip) <- c("red_band", "green_band", "blue_band", "near_infrared_band")
mapview(NAip)

site_spdf1 <- readOGR(dsn = "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam", layer = "24m_squares")
mapview(NAip)+mapview(Ponderosa)




# Loop through each feature in the shapefile
for (i in 1:length(site_spdf1)) {
  
  # Get the LINK value for the current feature
  link <- as.character(site_spdf1@data[i, "LINK"])
  
  # Clip the raster by the current feature
  cr <- crop(NAip, site_spdf1[i,])
  
  # Save the output raster using the LINK value as the filename
  filename <- paste0("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NAIP_clip\\", link, ".tif")
  writeRaster(cr, filename = filename, format = "GTiff")
}


################## loop through each croped file and write the extent as polygon###################




# Set the directory path where NAIP imagery files are located
naip_dir <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NAIP_clip"

# Set the output directory path to save the extent polygons
output_dir <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Extent"

# Get a list of NAIP imagery files in the directory
naip_files <- list.files(naip_dir, pattern = ".tif$", full.names = TRUE)

# Loop through each NAIP imagery file
for (naip_file in naip_files) {
  # Read the NAIP image
  naip <- brick(naip_file)
  
  # Get the extent of the NAIP image
  naip_extent <- extent(naip)
  
  # Convert the extent to a polygon
  naip_polygon <- as(naip_extent, "SpatialPolygons")
  
  # Convert the polygon to an sf object
  naip_sf <- st_as_sf(naip_polygon)
  
  # Assign the CRS of the NAIP imagery to the extent sf object
  st_crs(naip_sf) <- st_crs(naip)
  
  # Get the file name without extension
  naip_name <- tools::file_path_sans_ext(basename(naip_file))
  
  # Set the output file path
  output_file <- file.path(output_dir, paste0(naip_name, ".shp"))
  
  # Save the extent polygon as a shapefile
  st_write(naip_sf, output_file)
}


###################################### combine shapefile################


# Set the directory path where the shapefiles are located
shapefile_dir <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Extent"

# Get a list of shapefile files in the directory
shapefile_files <- list.files(shapefile_dir, pattern = ".shp$", full.names = TRUE)

# Initialize an empty vector to store the shapefile names
shapefile_names <- vector("character", length(shapefile_files))

# Read the shapefiles and store their names
for (i in 1:length(shapefile_files)) {
  shapefile <- st_read(shapefile_files[i])
  shapefile_names[i] <- tools::file_path_sans_ext(basename(shapefile_files[i]))
}

# Combine the shapefiles into a single shapefile
combined_sf <- do.call(rbind, lapply(shapefile_files, st_read))

# Add a new field to the combined shapefile for shapefile names
combined_sf$Shapefile_Name <- shapefile_names

# Set the output file path for the combined shapefile
output_file <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\Combine\\Merge.shp"

# Write the combined shapefile
st_write(combined_sf, output_file, driver = "ESRI Shapefile")
