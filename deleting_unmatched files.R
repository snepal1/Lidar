
tif_folder <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\NIR_raster_pred"
shp_folder <- "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\AOI_plots\\CHM_shape_pred"

# Get the list of files in each folder
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE)
shp_files <- list.files(shp_folder, pattern = "\\.shp$", full.names = TRUE)

# Extract the filenames (without extension) from shp_files
shp_filenames <- tools::file_path_sans_ext(basename(shp_files))

# Keep only the matching .tif files
matching_tif_files <- file.path(tif_folder, paste0(shp_filenames, ".tif"))
files_to_remove <- setdiff(tif_files, matching_tif_files)
file.remove(files_to_remove)
