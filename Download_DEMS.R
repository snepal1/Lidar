library(rstac)
library(magrittr)
library(terra)
library(lidR)
s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")



it_obj <- s_obj %>%
  stac_search(collections = "3dep-seamless",
              bbox = c(-121.755981, 40.623334,-121.699677,40.647304)) %>% #(west=minlong, south=minlat,east=maxlong, north=maxlat)
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())

leng_f<-length(it_obj$features)

#str(leng_f)

output_dir <- "D:\\All_app\\LiDAR_data\\"

#url <- paste0("/vsicurl/", it_obj$features[[i]]$assets$data$href)
#str(it_obj)


for (i in 1:leng_f) {
  url <- it_obj$features[[i]]$assets$data$href
  output_file <- paste0(output_dir, "data_", i, ".tif")
  download.file(url, destfile = output_file)
}
