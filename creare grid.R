library(sf)
library(sp)

# Load the shapefile
shp <- st_read("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam\\ANALYSIS_AREA_M_MC_200.shp")
plot(shp)
# Determine the extent of the shapefile
shp_bbox <- st_bbox(shp)
plot(shp_bbox)
# Create a grid with a resolution of 20 x 20 m
grid <- st_make_grid(shp_bbox, cellsize = c(27, 27))

# Convert the grid to a spatial object
grid_sf <- st_as_sf(grid)

# Intersect the grid with the shapefile
intersection <- st_intersection(grid_sf, shp)

# Save the intersection as a new shapefile
st_write(intersection, "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam\\Grid_24.shp")
