rm(list=ls(all=TRUE))
library(sf)
library(raster)

site_spdf <- readOGR(dsn = "S:\\Lassen_data\\DATA_SHAPES", layer = "unit_of_interest")
plot(site_spdf)
site_spdf@proj4string
# Create an empty raster.
grid <- raster(extent(site_spdf))

# Choose its resolution. I will use 2 degrees of latitude and longitude.
res(grid) <- 300

# Make the grid have the same coordinate reference system (CRS) as the shapefile.
proj4string(grid)<-proj4string(site_spdf)

# Transform this raster into a polygon and you will have a grid, but without Brazil (or your own shapefile).
gridpolygon <- rasterToPolygons(grid)

# Intersect our grid with Brazil's shape (or your shapefile). R will need a considerable time to do that (~15 minutes in our example). Just let it work while you do other things, or drink a coffee, or whatever. Note that the time needed to finish this depends on the size (area) of your shape, and on your grid resolution. So, don't expect to intersect a 0.5 lat/long grid with a world shapefile in 5 minutes (=]).
dry.grid <- intersect(site_spdf, gridpolygon)

# Plot the intersected shape to see if everything is fine.
plot(dry.grid)

writeOGR(dry.grid, dsn = "S:\\Lassen_data\\Lassen_Lidar\\AOI", layer = "250mgrid", driver = "ESRI Shapefile")
