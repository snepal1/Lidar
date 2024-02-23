
################# Load all the required libraries ################
library(rstac)
library(magrittr)
library(terra)
library(lidR)

################# Stack API URL from MIcrosoft planetary computer API##################################
s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")


################## Give a bounding box for area of interest in your region as min and maximum latitiude and longitude#############

it_obj <- s_obj %>%
  stac_search(collections = "3dep-lidar-copc",
              bbox = c(-121.755981, 40.623334,-121.699677,40.647304)) %>% #(west=minlong, south=minlat,east=maxlong, north=maxlat)
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())

leng_f<-length(it_obj$features)
#str(leng_f)

################### Set the output directory path to wherver you want in your computer ####################
output_dir <- tempdir()
#url <- paste0("/vsicurl/", it_obj$features[[i]]$assets$data$href)
#str(it_obj)


################# From the gather list of number of files in the uRL selcte how mny you want to download , I have 5 files to download , you can change the number####
for (i in 1:5) {
    url <- it_obj$features[[i]]$assets$data$href
  output_file <- paste0(output_dir, "data_", i, ".laz")
  download.file(url, destfile = output_file , mode='wb')
   }
#################### Read the data ############

