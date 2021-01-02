# install.packages("sf")

library(sf)

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


#library(ggplot2)

# ggplot() + 
#   geom_sf(data = aoi_boundary_HARV, size = 3, color = "black", fill = "cyan1") + 
#   ggtitle("AOI Boundary Plot") + 
#   coord_sf()

# junk <- aoi_boundary_HARV[1]
# junk
  
#st_crs(spdf)


library(sp)

# x_list <- runif(10, min=188538.2, max=366410.7)
# y_list <- runif(10, min=309895.8, max=453200.8)

#coordinates(pts) <- ~ x_list + y_list

set.seed(357)
#pts <- data.frame(x = rnorm(100), y = rnorm(100), var1 = runif(100), var2 = sample(letters, 100, replace = TRUE))

pts <- data.frame(x = runif(10, min=188538.2, max=366410.7), 
                  y = runif(10, min=309895.8, max=453200.8), 
                  var1 = runif(10, min=188538.2, max=366410.7),
                  var2 = sample(letters, 10, replace = TRUE))
pts

coordinates(pts) <- ~ x + y

class(pts)

pts <- st_as_sf(pts)

plot(pts)
axis(1); axis(2)

st_crs(pts)

#irl_grid <- "EPSG:29900"
# Proj4js.defs["EPSG:29900"] = "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs";
# proj4_string <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs"


#s.sf <- st_set_crs(s.sf, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
# pts <- st_set_crs(pts, proj4_string)
pts <- st_set_crs(pts, crs_in)

# ply <- matrix(c(-1,-1, 1,-1, 1,1, -1,1, -1,-1), ncol = 2, byrow = TRUE)
# ply <- SpatialPolygons(list(Polygons(list(Polygon(ply)), ID = 1)))
# ply <- SpatialPolygonsDataFrame(Sr = ply, data = data.frame(polyvar = 357))
# plot(ply, add = TRUE, border = "red")

# over(pts, ply)
# 
# over(pts,as(ply,"SpatialPolygons"))

# over(pts, aoi_boundary_HARV)
# over(pts,as(aoi_boundary_HARV,"SpatialPolygons"))
# over(as(pts,"SpatialPoints"),as(aoi_boundary_HARV,"SpatialPolygons"))
# over(as(pts,"SpatialPoints"),as(aoi_boundary_HARV,"SpatialPolygonsDataFrame"))


class(aoi_boundary_HARV)
class(pts)

#pts_old <- pts

#spTransform(pts, SpatialPolygons )

#spTransform(pts, CRS("+proj=utm +zone=32 +datum=WGS84"))

#pts <- spTransform(pts, CRS(proj4_string))
#pts <- spTransform(pts, CRS(crs_in))

#install.packages("rgdal")

#over(pts,junk)



spdf <- as_Spatial(aoi_boundary_HARV)
class(spdf)
# identical to
spdf <- as(aoi_boundary_HARV, "Spatial")

# convert to SpatialPolygons
#junk <- as(st_geometry(aoi_boundary_HARV), "Spatial")
#junk

# convert points()
pts_sp <- as_Spatial(pts)
class(pts_sp)

over_results <- over(pts_sp, spdf)
#over(pts,as(aoi_boundary_HARV,"SpatialPolygons"))
#over(as(pts,"SpatialPoints"),as(aoi_boundary_HARV,"SpatialPolygons"))
#over(as(pts,"SpatialPoints"),as(aoi_boundary_HARV,"SpatialPolygonsDataFrame"))

over_results

#irl_grid <- "EPSG:29900"

# Proj4js.defs["EPSG:29900"] = "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs";
#proj4_string <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs"


#s.sf <- st_set_crs(s.sf, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
#junk <- st_set_crs(junk, proj4_string)


library(PostcodesioR)
#foundcode <- 

chk <- postcode_lookup("BT414BS")

# "BT308RD", "BT294LN", "BT414BS", 
# "BT388TP","BT399HL", "BT670LQ", "BT670DD", "BT670QA"


library("factoextra")
data("decathlon2")
df 

decathlon.active <- decathlon2[1:23, 1:10]
res.pca <- prcomp(decathlon.active, scale = TRUE)
fviz_pca_biplot(res.pca)


library(dplyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(factoextra)
library(ggfortify)
library(cluster) 
library(corrplot)

### How to sample the geo area?
# Grab the SOA atlas data, join with the practices info, seperate and process



##############

no_outliers_file <- "no_outliers.csv"
no_outliers_file <- paste(datapath, no_outliers_file, sep = "/")
no_outliers_file

# doctor/practise data
# docs_in <- read.csv(paste(datapath, "cleaned_practice_data.csv", sep = "/"))
pr_main_in <- read.csv(no_outliers_file)

head(pr_main_in)

# Examine structure of the file
str(pr_main_in)


# --------------------------------------------------
# Re-Read practice info including the geo additions
# --------------------------------------------------

datapath <- "portal_data"

datapath <- paste(main_path, datapath, sep = "/")
datapath

# doctor/practise data
docs_in <- read.csv(paste(datapath, "geo_cleaned_practice_data.csv", sep = "/"))

head(docs_in)

# Examine structure of the file
str(docs_in)
colnames(docs_in)[colnames(docs_in) == "PracNo"] <-"Practice"
docs_in <- select(docs_in, c(Practice, PracticeName, 
                             Town, County, 
                             longitude, latitude, 
                             eastings, northings,
                             SOA_CODE,
                             SOA_LABEL))

str(docs_in)

pr_main_in <- select(pr_main_in, !c(Town, County, Postcode))
pr_main_in

# pr_main_in$Practice <- as.integer((levels(pr_main_in$Practice))[pr_main_in$Practice])

# Left join to get practice info for the prescription data
pr_main  <- left_join(pr_main_in, docs_in, 
                      by = c("Practice" = "Practice")
)

str(pr_main)

# Check on NA
colSums(is.na(pr_main))

pr_main <- droplevels(pr_main)


###########

main_path <- "~/courses/dissertation/data"

datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

clusters_soa_file <- "clusters_SOA.csv"
clusters_soa_file <- paste(datapath, clusters_soa_file, sep = "/")

clusters_in <- read.csv(clusters_soa_file)

clusters_soa <- select(clusters_in, c(Postcode, PID, Structure, 
                                      SOA_CODE,
                                      eastings, northings,
                                      latitude, longitude
))

str(clusters_soa)
str(pr_main)

pr_main$SOA_CODE <- as.character(pr_main$SOA_CODE)
clusters_soa$SOA_CODE <- as.character(clusters_soa$SOA_CODE)

# merged_1 direct SOA link between clusters practices
merged_1 <- merge(clusters_soa, pr_main, by.x = "SOA_CODE", by.y = "SOA_CODE"  )
merged_1

# This to basically review the practices main dataset "grouping"
ggline(merged_1, x = "Structure", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "Structure ")  + geom_point(aes(color=Structure ))

t.test(items_avg_patient_ratio ~ Structure ,
       data = merged_1,
       alternative = "two.sided",
       paired = FALSE
)
# significance -> p = 0.1939

ms_anova_one_way <- aov(items_avg_patient_ratio ~ Structure, data = merged_1)
summary(ms_anova_one_way)

cluster_pts <- clusters_soa[c("eastings", "northings", "PID")]
colnames(cluster_pts) <- c("x", "y", "PID")
cluster_pts
coordinates(cluster_pts) <- ~ x + y
class(cluster_pts)
cluster_pts <- st_as_sf(cluster_pts)

docs_pts <- docs_in[c("eastings", "northings", "Practice")]
colnames(docs_pts) <- c("x", "y", "Practice")
docs_pts
coordinates(docs_pts) <- ~ x + y
class(docs_pts)
docs_pts <- st_as_sf(docs_pts)
docs_pts <- st_set_crs(docs_pts, crs_in)
class(docs_pts)

# read shape file and find crs
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

#### crs found

plot(cluster_pts)
st_crs(cluster_pts)

cluster_pts <- st_set_crs(cluster_pts, crs_in)

##############

# # install.packages("nngeo")
library(nngeo)

# junk <- st_connect(cluster_pts, docs_pts)
nearest_docs <- st_nn(cluster_pts, docs_pts, k = 1) # k = number to find
nearest_docs

nearest_docs <- lapply(nearest_docs, function(x) docs_pts$Practice[x])
nearest_docs

nearest_docs <- do.call(rbind.data.frame, nearest_docs)
nearest_docs

colnames(nearest_docs) <- c("Practice")

nearest_docs <- cbind(nearest_docs, clusters_soa)

# fred2 <- docs_in[docs_in$Practice %in% fred$Practice, ]
# fred2


nearest_docs_pts <- nearest_docs[c("eastings", "northings", "Practice")]
colnames(nearest_docs_pts) <- c("x", "y", "Practice")
nearest_docs_pts
coordinates(nearest_docs_pts) <- ~ x + y
class(nearest_docs_pts)
nearest_docs_pts <- st_as_sf(nearest_docs_pts)
nearest_docs_pts <- st_set_crs(nearest_docs_pts, crs_in)

plot(nearest_docs_pts, col = 10)
plot(nearest_docs_pts, reset = FALSE, col = 100)
plot(cluster_pts, add = TRUE, col = 30)


nearest_docs <- merge(nearest_docs, pr_main, by = "Practice")
nearest_docs

# This to basically review the practices main dataset "grouping"
ggline(nearest_docs, x = "Structure", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "Structure ")  + geom_point(aes(color=Structure ))

ms_anova_one_way <- aov(items_avg_patient_ratio ~ Structure, data = nearest_docs)
summary(ms_anova_one_way)

# t.test(items_avg_patient_ratio ~ Structure ,
#        data = merged_1,
#        alternative = "two.sided",
#        paired = FALSE
# )

# Remove connacht, Dublin, merge NI_II and NI_III
junk <- subset(nearest_docs, Structure != "Connacht" &  Structure != "Dublin")

junk$Structure[junk$Structure == "NI_III" ] <- "NI_II"

ggline(junk, x = "Structure", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "Structure ")  + geom_point(aes(color=C_Group ))

ms_anova_one_way <- aov(items_avg_patient_ratio ~ Structure, data = junk)
summary(ms_anova_one_way)



# over_results <- over(pts_sp, spdf)
# over_results

# library(RANN)
# 
# data2 <-docs_in[, 5:6]
# 
# closest <- nn2(clusters_soa[, 7:8], query = data2, k = 10, searchtype = "radius", radius = 10.001)
# closest
# 
# closest <- sapply(closest, cbind) %>% as_tibble
# closest
# 
# closest2 <- nn2(doc_lat_long[,1:2], query = docs_in, k = 2, searchtype = "radius", radius = 1.001)
# closest2



str(junk)

# library(tidyverse)
# library(ggpubr)
# library(rstatix)
# 
# ggplot(pr_sorted, aes( y=chapter_avg_patient_ratio)) + 
#   geom_boxplot(notch=TRUE,
#                outlier.colour="red",
#                outlier.size=2,
#                fill='#A4A4A4', color="black"
#   )  + labs(title="Avg Items / Patient", y = "Avg Items per Patient") +  
#   facet_wrap(~ Chapter_lab, scale="free")

# boxplot(x = pr_sorted$chapter_avg_patient_ratio,
#         main = "chapter_avg_patient_ratio",
#         sub = paste("outlier rows: ",
#                     boxplot.stats(x = pr_sorted$chapter_avg_patient_ratio)$out),
#         col = "steelblue",
#         border = "black"
# )


# friedman.test(pr_sorted,
#               non_wide$Practice, non_wide$chapter_avg_patient_ratio, non_wide$BNF_Chapter)



library(lattice)

histogram(~ ratio | Practice,
          data=pr_sorted,
          # layout=c(1,5)      #  columns and rows of individual plots
)

junk <- data.matrix(junk)

friedman.test(junk)
class(junk)
class(RoundingTimes)


RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))
friedman.test(RoundingTimes)


# install.packages("scmamp")
# library(scmamp)
# library("ggplot2")
# library("graphviz")
# 
# friedmanAlignedRanksTest(pr_sorted)



# sorted_data_file = "sorted_data_file.csv"
# write.csv(file=sorted_data_file, x=pr_sorted, quote=TRUE, row.names = FALSE)


##################
# SA data 

library(sf)

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

pr_main <- pr_main[!pr_main$Practice %in% remove_practices$Practice, ]
pr_main <- droplevels(pr_main)



#### boxplots

forbox <- merge(clusters, chapters_in)


str(forbox)

forbox <- select(forbox, c(Practice, PID , Structure, Cluster_Grp, Chapter_lab) )
forbox


# Cluster_Grp.f ,Total_Items_P
# P_Month_Avg,BNF_Chapter
# Total_Items_PC ,PC_Month_Avg
# Chapter_lab   

cluster_1 <- subset(clusters, clusters$Cluster_Grp == 1)
cluster_2 <- subset(clusters, clusters$Cluster_Grp == 2)

ggplot(forbox, aes( y=PC_Month_Avg  ,   x = Cluster_Grp )) + 
  #  labs(title="# items_avg_patient_ratio Patients", x = "C_Group",  y = "items_avg_patient_ratio") +
  geom_boxplot(notch=FALSE,
               outlier.colour="red",
               outlier.size=2 ,
  ) +  facet_wrap(~Chapter_lab, scale="free")


ggplot(chapters_in, aes( y=PC_Month_Avg  ,  fill = Chapter_lab )) + 
  #  labs(title="# items_avg_patient_ratio Patients", x = "C_Group",  y = "items_avg_patient_ratio") +
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2 ,
  ) +  facet_wrap(~Chapter_lab, scale="free")

ggplot(chk, aes( y=PC_Month_Avg  ,  fill = BNF_Chapter , x = C_Group)) + 
  #  labs(title="# items_avg_patient_ratio Patients", x = "C_Group",  y = "items_avg_patient_ratio") +
  geom_violin(notch=TRUE,
              outlier.colour="red",
              outlier.size=2 ,
  ) +  facet_wrap(~BNF_Chapter, scale="free")


# end PCA code on the 2 clusters together
############################################


# Regrab the 24 practices
# want to label the original 24 vs the 42 expanded group

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

nearest_docs_file <- "nearest_docs_24.csv"
nearest_docs_file <- paste(datapath, nearest_docs_file, sep = "/")
nearest_docs_file

nearest_docs <- read.csv(nearest_docs_file)

clusters_24 <- select(nearest_docs, c(Practice, PID, Structure))

clusters <- merge(clusters, clusters_24, by = "Practice", all = TRUE)
clusters$src <- ifelse(rowSums(is.na(clusters)) == dim(clusters)[2] - 1, "NO", 
                       ifelse(rowSums(!is.na(clusters)) == dim(clusters)[2], paste(clusters$Cluster_Grp, "org", sep = ""), 
                              paste(clusters$Cluster_Grp, "xp", sep = "")                         ))

clusters <- select(clusters, ! c(PID.y, Structure.y) )

colnames(clusters) <- c("Practice", "PID", "Structure", "Cluster_Grp", "Cluster_Grp.f", "src")

clusters$src.f <- as.factor(clusters$src)

clst  <- chk[chk$Practice %in% clusters$Practice, ]
clst = merge(clst, clusters)
clst <- droplevels(clst)

pca_chk42 <- prcomp(clst[ , 2:21], scale = TRUE)



biplot <- fviz_pca_biplot(pca_chk42, 
                          axes = c(1,2),
                          addEllipses = TRUE,
                          label = "var",
                          habillage = clusters$Cluster_Grp.f ,
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "Cluster comparison, 42 practices",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
) # + geom_text(aes(label=clusters$src.f),hjust=0, vjust=0) 


# ggpar(biplot, 
#       title = "Chapter PCA Analysis",
#       subtitle = "Cluster comparison, 42 practices",
#       xlab = "PC 1",
#       ylab = "PC 2",
#       legend.title = "Cluster",
#       legend.position = "top",
#       ggtheme = theme_light(),
#       palette = "jco"
# ) + geom_point(aes(color =clusters$src.f )) # , shape = , color=src.f, size=src.f))












