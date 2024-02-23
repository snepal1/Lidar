library(sf)

# Read the point shapefile
points <- st_read("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam\\New.shp")

# Create a square polygon around each point
squares <- st_buffer(points, dist = 24)  # Adjust the distance to control the size of the square

# Transform the CRS of the square polygons to EPSG:3489
squares_transformed <- st_transform(squares, crs = "+init=epsg:3489")

# Write the transformed square polygons to a new shapefile in the same CRS
st_write(squares_transformed, "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam\\24m_squares.shp")

#############################

library(sf)

# Read the shapefile
shape <- st_read("S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam\\AOI.shp")

# Create a square grid over the extent of the shape
grid <- st_make_grid(shape, cellsize = c(24,24))  # Adjust the cellsize as desired

# Clip the grid to the extent of the shape
grid_clipped <- st_intersection(grid, shape)

# Write the clipped grid to a new shapefile
st_write(grid_clipped, "S:\\Lassen_data\\Lassen_Lidar\\Unit_of Interest\\Adam\\24M-GRID.shp")
