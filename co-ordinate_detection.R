library(sf)
shp <- st_read("S:\\Lassen_data\\Lassen_Lidar\\Ponerosa\\CHM\\Chm_1.shp")
centroids <- st_centroid(shp)
centroid_coords <- st_coordinates(centroids)
df <- data.frame(X = centroid_coords[, 1], Y = centroid_coords[, 2])
points_sf <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(shp))
st_write(points_sf, "S:\\Lassen_data\\Lassen_Lidar\\Ponerosa\\co-ordinates_test.shp")

########################## using the lidar data directly ############################
Path: 
S:/Lassen_data/Lassen_Lidar/co-ordinate_detection.R