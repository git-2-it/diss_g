# diss01
# Run after Clean_Practices
# # Assigns SOA to practices


library(dplyr)
library(PostcodesioR)

# --------------------------------------------------
# Get practice data
# --------------------------------------------------

main_path <- "~/courses/dissertation/data"

datapath <- "portal_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

docs_in <- read.csv(paste(datapath, "cleaned_practice_data.csv", sep = "/"))

head(docs_in)

# Examine structure of the file
str(docs_in)

# Find closest clusters?
# What about duplicates? - 309 Practices, 251 Postcodes
# Verify the volume of duplicates, then consider action

# create postcode finder for lat/long
# lat_long_chk <- function(long_in, lat_in)
postcode_api_lookup <- function(x)
{
  # str(x)
  
  find_code <- x["Postcode" ]
  
  # The codes should already be cleaned up
  ret_code <- "NOTFOUND"
  
  tryCatch(
    {
      foundcode <- postcode_lookup(find_code)
      ret_code <- foundcode [c("longitude", "latitude", "postcode", "eastings", "northings")]
    } , 
    error=function(cond) {
      ret_code <- "NOTFOUND"
    },
    warning=function(cond) {
      ret_code <- "NOTFOUND"
    }
  )
  
  return(ret_code)
  
} # end function

lat_long_data <- apply(docs_in, 1, postcode_api_lookup)
lat_long_data


lat_long_data <- do.call(rbind.data.frame, lat_long_data)
lat_long_data$postcode <-   gsub(' ', '',lat_long_data$postcode)

colnames(lat_long_data) <- c("longitude", "latitude", "Postcode", "eastings", "northings")
lat_long_data <- subset(lat_long_data, !duplicated(lat_long_data$Postcode))
lat_long_data

# docs_in <- cbind(docs_in, lat_long_data)
# docs_in
docs_in$Postcode <-   gsub(' ', '',docs_in$Postcode)
docs_in <- merge(x = docs_in, y = lat_long_data, by.x = "Postcode", by.y = "Postcode")

 
# closest <- nn2(doc_lat_long[,1:2], query = data2, k = 2, searchtype = "radius", radius = 1.001)
# closest
# closest <- sapply(closest, cbind) %>% as_tibble
# closest
# 
# closest2 <- nn2(doc_lat_long[,1:2], query = data2, k = 2, searchtype = "radius", radius = 1.001)
# closest2

library(sf)
library(sp)

main_path <- "~/courses/dissertation/data"

datapath <- "portal_data"
datapath <- paste(main_path, datapath, sep = "/")

shape_file <- "SOA2011_Esri_Shapefile_0/SOA2011.shp"
shape_file <- paste(datapath, shape_file, sep = "/")
shape_file

aoi_boundary_HARV <- st_read(shape_file)

class(aoi_boundary_HARV)

st_geometry_type(aoi_boundary_HARV)

crs_in <- st_crs(aoi_boundary_HARV)
crs_in

st_bbox(aoi_boundary_HARV)

aoi_boundary_HARV


pts <- docs_in[c("eastings", "northings", "PracNo")]
colnames(pts) <- c("x", "y", "Practice")
pts

coordinates(pts) <- ~ x + y

class(pts)

pts <- st_as_sf(pts)

plot(pts)

st_crs(pts)

pts <- st_set_crs(pts, crs_in)

class(aoi_boundary_HARV)
class(pts)

spdf <- as_Spatial(aoi_boundary_HARV)
class(spdf)

# convert points()
pts_sp <- as_Spatial(pts)
class(pts_sp)

over_results <- over(pts_sp, spdf)
over_results

docs_in <- cbind(docs_in, over_results)
docs_in <- droplevels(docs_in)

# Check all practices have an SOA assigned
colSums(is.na(docs_in))

datapath <- "portal_data"

datapath <- paste(main_path, datapath, sep = "/")
datapath

geo_cleaned_practice_data_file <- "geo_cleaned_practice_data.csv"
geo_cleaned_practice_data_file <- paste(datapath, geo_cleaned_practice_data_file, sep = "/")

# write.csv(file=geo_cleaned_practice_data_file, x=docs_in, quote=TRUE, row.names = FALSE)

names(docs_in)[names(docs_in) == 'eastings'] <- 'x'
names(docs_in)[names(docs_in) == 'northings'] <- 'y'


pts <- docs_in[c("x", "y", "PracNo", "C_Group")]
#colnames(pts) <- c("x", "y", "Practice")

coordinates(pts) <- ~ x + y
plot(pts)

axis(1); axis(2)


