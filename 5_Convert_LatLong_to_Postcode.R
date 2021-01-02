# ca4_ni

# Creates summarised data per practice, per month
# Total items
# For each file, crate equivalent summarised version
# Unzips, processe and cleans-up the extracts

# Init required libraries
library("readr")
library("tidyr")
library("dplyr")
library(PostcodesioR)
library(sf)


datapath <- "../../../data"
datapath <- "~/courses/dissertation/data"
atlas_path <- paste(datapath, "atlas", sep = "/")
atlas_path
atlas_data <- paste(atlas_path, "atlas_data.censored.csv", sep = "/")
#atlas_data <- paste(atlas_path, "atlas_data_lat_long.csv", sep = "/")

outdir <- atlas_path
output_data  <- paste(outdir, "atd_lat_long.csv", sep = "/")

atlas_data

data_in <- read.csv(atlas_data )

str(data_in)

# Check for NA values
colSums(is.na(data_in))

# Convert lat/long to UK post code
# Deal with non UK possibilities?

# rev_geo 

# create postcode finder for lat/long
# lat_long_chk <- function(long_in, lat_in)
lat_long_chk <- function(x)
  {
  long_in = x["X" ]
  lat_in = x["Geog"]
  
  # rev_geo <- reverse_geocoding(long_in, lat_in )
  
  str(long_in)
  str(lat_in)
  
  rev_geo <- reverse_geocoding(long_in, lat_in , 1, 10, TRUE)
  
  str(rev_geo)
  
  postcode <- rev_geo[[1]]$postcode[1]
  str(postcode)
  
  return(postcode)
  
} # end function


str(data_in)

data_sub <- data_in[data_in$G_INCL == "1", ]


data_sub$postcode <- apply(data_sub, 1, lat_long_chk )

str(data_sub$postcode)

# write.csv(file= output_data, x=data_sub, quote=TRUE, row.names = TRUE)

############################# postcode for atlas data to link to SOA

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


# Get the specific atlas data

main_path <- "~/courses/dissertation/data"

datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

atlas_data <- paste(datapath, "atd_lat_long.csv", sep = "/")

clusters_in <- read.csv(atlas_data)

names(clusters_in)[names(clusters_in) == 'postcode'] <- 'Postcode'
head(clusters_in)
clusters_in$Postcode <- as.character(clusters_in$Postcode)

# lat_long_data <- do.call(rbind.data.frame, lat_long_data)
clusters_in$Postcode <- gsub(' ', '',clusters_in$Postcode)

lat_long_data <- apply(clusters_in, 1, postcode_api_lookup)
lat_long_data

lat_long_data <- do.call(rbind.data.frame, lat_long_data)
lat_long_data$postcode <-   gsub(' ', '',lat_long_data$postcode)
lat_long_data

colnames(lat_long_data) <- c("longitude", "latitude", "Postcode", "eastings", "northings")
lat_long_data <- subset(lat_long_data, !duplicated(lat_long_data$Postcode))
lat_long_data

# docs_in <- cbind(docs_in, lat_long_data)
# docs_in
clusters_in$Postcode <-   gsub(' ', '', clusters_in$Postcode)
clusters_in <- merge(x = clusters_in, y = lat_long_data, by.x = "Postcode", by.y = "Postcode")

# import the SOA polygon shape file


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


class(aoi_boundary_HARV)
st_bbox(aoi_boundary_HARV)
aoi_boundary_HARV


pts <- clusters_in[c("eastings", "northings", "PID")]
colnames(pts) <- c("x", "y", "PID")

pts

coordinates(pts) <- ~ x + y

class(pts)

pts <- st_as_sf(pts)

plot(pts)

st_crs(pts)
pts <- st_set_crs(pts, crs_in)

spdf <- as_Spatial(aoi_boundary_HARV)

# convert points()
pts_sp <- as_Spatial(pts)
class(pts_sp)

over_results <- over(pts_sp, spdf)
over_results

clusters_in <- cbind(clusters_in, over_results)
clusters_in <- droplevels(clusters_in)

# Check all practices have an SOA assigned
colSums(is.na(clusters_in))

datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

clusters_soa_file <- "clusters_SOA.csv"
clusters_soa_file <- paste(datapath, clusters_soa_file, sep = "/")

# write.csv(file=clusters_soa_file, x=clusters_in, quote=TRUE, row.names = FALSE)


clusters_in$SOA_CODE <- as.character(clusters_in$SOA_CODE)

clusters_in$Cluster_Grp <- "2"
clusters_in$Cluster_Grp[clusters_in$Structure == "NI_III"] <- "1"
clusters_in$Cluster_Grp[clusters_in$Structure == "NI_II"] <- "1"

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

grouped_clusters_file <- "grouped_clusters.csv"
grouped_clusters_file <- paste(datapath, grouped_clusters_file, sep = "/")
grouped_clusters_file

# write.csv(file=grouped_clusters_file, x=clusters_in, quote=TRUE, row.names = FALSE)

##################
# SA data 

main_path <- "~/courses/dissertation/data"

datapath <- "portal_data"
datapath <- paste(main_path, datapath, sep = "/")

shape_file <- "SA2011_Esri_Shapefile_0/SA2011.shp"
shape_file <- paste(datapath, shape_file, sep = "/")
shape_file

aoi_sa_data <- st_read(shape_file)

class(aoi_sa_data)

st_geometry_type(aoi_sa_data)

crs_in <- st_crs(aoi_sa_data)
crs_in

st_bbox(aoi_sa_data)

aoi_sa_data

# Need the cluster points data

datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

clusters_soa_file <- "clusters_SOA.csv"
clusters_soa_file <- paste(datapath, clusters_soa_file, sep = "/")

clusters_in <- read.csv(clusters_soa_file)

pts <- clusters_in[c("eastings", "northings", "PID")]
colnames(pts) <- c("x", "y", "PID")

pts

coordinates(pts) <- ~ x + y

class(pts)

pts <- st_as_sf(pts)

plot(pts)

st_crs(pts)
pts <- st_set_crs(pts, crs_in)

spdf <- as_Spatial(aoi_sa_data)

# convert points()
pts_sp <- as_Spatial(pts)
class(pts_sp)

over_results <- over(pts_sp, spdf)
over_results

clusters_in <- cbind(clusters_in, over_results)
clusters_in <- droplevels(clusters_in)

# Check all practices have an SOA assigned
colSums(is.na(clusters_in))

# write out clusters with SA
datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

clusters_sa_file <- "clusters_SA.csv"
clusters_sa_file <- paste(datapath, clusters_sa_file, sep = "/")

# write.csv(file=clusters_sa_file, x=clusters_in, quote=TRUE, row.names = FALSE)





################################### EEE


