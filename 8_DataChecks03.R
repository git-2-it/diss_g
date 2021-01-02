# install.packages("sf")

library(sf)
library(sp)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(factoextra)
library(ggfortify)
library(cluster) 
library(corrplot)
library(nngeo)

main_path <- "~/courses/dissertation/data"

# Read shapefile and set CRS
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

#### crs found



### How to sample the geo area?
# Grab the SOA atlas data, join with the practices info, seperate and process


##############

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

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
# Read the clusters and SOA data

main_path <- "~/courses/dissertation/data"

datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

clusters_soa_file <- "clusters_SOA.csv"


datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath
clusters_soa_file <- "grouped_clusters.csv"
clusters_soa_file <- paste(datapath, clusters_soa_file, sep = "/")

clusters_in <- read.csv(clusters_soa_file)

# clusters_soa <- select(clusters_in, c(Postcode, PID, Structure, 
#                                       SOA_CODE,
#                                       eastings, northings,
#                                       latitude, longitude
# ))

clusters_soa <- select(clusters_in, c(PID, Structure,
                                      SOA_CODE,
                                      eastings, northings,
                                      latitude, longitude,
                                      Cluster_Grp
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

# Pretty meaningless anyway due to small sample size


# Create cluster points
cluster_pts <- clusters_soa[c("eastings", "northings", "PID")]
colnames(cluster_pts) <- c("x", "y", "PID")
cluster_pts
coordinates(cluster_pts) <- ~ x + y
class(cluster_pts)
cluster_pts <- st_as_sf(cluster_pts)

# Create practice points
docs_pts <- docs_in[c("eastings", "northings", "Practice")]
colnames(docs_pts) <- c("x", "y", "Practice")
docs_pts
coordinates(docs_pts) <- ~ x + y
class(docs_pts)
docs_pts <- st_as_sf(docs_pts)
docs_pts <- st_set_crs(docs_pts, crs_in)
class(docs_pts)


# plot(cluster_pts)
st_crs(cluster_pts)

cluster_pts <- st_set_crs(cluster_pts, crs_in)

##############
# nearest_docs_24

# Run based on finding single nearest practice to cluster
nearest_docs <- st_nn(cluster_pts, docs_pts, k = 1) # k = number to find
nearest_docs

nearest_docs <- lapply(nearest_docs, function(x) docs_pts$Practice[x])
nearest_docs

nearest_docs <- do.call(rbind.data.frame, nearest_docs)
nearest_docs

colnames(nearest_docs) <- c("Practice")

nearest_docs <- cbind(nearest_docs, clusters_soa)
nearest_docs <- nearest_docs[c("PID", "Structure", "Practice")]

nearest_docs  <- merge(nearest_docs, pr_main, by = "Practice")
nearest_docs


datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

nearest_docs_file <- "nearest_docs_24.csv"
nearest_docs_file <- paste(datapath, nearest_docs_file, sep = "/")
nearest_docs_file

# Needs tidy before save
# master_list <- unique(select(pr_main, c("Practice")))
# master_list
write.csv(file=nearest_docs_file, x=nearest_docs, quote=TRUE, row.names = FALSE)


nearest_docs_pts <- nearest_docs[c("eastings", "northings", "Practice")]
colnames(nearest_docs_pts) <- c("x", "y", "Practice")
nearest_docs_pts
coordinates(nearest_docs_pts) <- ~ x + y
class(nearest_docs_pts)
nearest_docs_pts <- st_as_sf(nearest_docs_pts)
nearest_docs_pts <- st_set_crs(nearest_docs_pts, crs_in)

plot(nearest_docs_pts, col = 10, lab = "fred")
plot(nearest_docs_pts, reset = FALSE, col = 100)
plot(cluster_pts, add = TRUE, col = 30,
     )


# nearest_docs <- merge(nearest_docs, pr_main, by = "Practice")
# nearest_docs

# This to basically review the practices main dataset "grouping"
ggline(nearest_docs, x = "Structure", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "Structure ")  + geom_point(aes(color=Structure ))

ms_anova_one_way <- aov(items_avg_patient_ratio ~ Structure, data = nearest_docs)
summary(ms_anova_one_way)
# significant with these groupings -> p-value 0.0319


# t.test(items_avg_patient_ratio ~ Structure ,
#        data = merged_1,
#        alternative = "two.sided",
#        paired = FALSE
# )

# Remove Connacht, Dublin, merge NI_II and NI_III
junk <- nearest_docs
junk <- subset(nearest_docs, Structure != "Connacht" & Structure != "Dublin")
junk <- subset(nearest_docs, Structure != "Connacht" & Structure != "Dublin" & Structure != "NI_III")

junk$Structure[junk$Structure == "NI_III" ] <- "NI_II"

junk$Structure[junk$Structure == "Connacht" ] <- "Ulster"
junk$Structure[junk$Structure == "Dublin" ] <- "Ulster"

ggline(junk, x = "Structure", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "Structure ")  + geom_point(aes(color=C_Group ))

ms_anova_one_way <- aov(items_avg_patient_ratio ~ Structure, data = junk)
summary(ms_anova_one_way)
# significance drops with removal of connacht, dublin, and the NI_III shift

#########################################################
# Run based on findingnearest practices to cluster
# nearest_docs_42

nearest_docs <- st_nn(cluster_pts, docs_pts, k = 2) # k = number to find
nearest_docs

nearest_docs <- lapply(nearest_docs, function(x) docs_pts$Practice[x])
nearest_docs

nearest_docs <- do.call(rbind.data.frame, nearest_docs)
nearest_docs

colnames(nearest_docs) <- c("Practice1" ,"Practice2")

nearest_docs <- cbind(nearest_docs, clusters_soa)
nearest_docs <- pivot_longer(nearest_docs, cols=Practice1:Practice2, names_to = "Level", values_to = "Practice")

nearest_docs <- nearest_docs[c("PID", "Structure", "Practice", "Level")]

nearest_docs  <- merge(nearest_docs, pr_main, by = "Practice")
nearest_docs

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


# nearest_docs <- merge(nearest_docs, pr_main, by = "Practice")
# nearest_docs

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

# Remove Connacht, Dublin, merge NI_II and NI_III
junk <- unique(nearest_docs[c("Practice", "Structure")])
junk 
str(junk)
str(nearest_docs)

#setkey(nearest_docs)

# Get Unique lines in the data table
# should dups be removed or kept or possibly weighted?
nrow(nearest_docs)
nearest_docs <- nearest_docs[!duplicated(nearest_docs[c("Practice", "Structure")]),]
nrow(nearest_docs)

junk <- nearest_docs

# junk <- subset(nearest_docs, Structure != "Connacht" &  Structure != "Dublin")
junk <- subset(junk, Structure != "Connacht" &  Structure != "Dublin")
junk$Structure[junk$Structure == "NI_III" ] <- "NI_II"

junk$Structure[junk$Structure == "Connacht" ] <- "Ulster"
junk$Structure[junk$Structure == "Dublin" ] <- "Ulster"

ggline(junk, x = "Structure", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "Structure ") #  + geom_point(aes(color=C_Group ))

ms_anova_one_way <- aov(items_avg_patient_ratio ~ Structure, data = junk)
summary(ms_anova_one_way)

t.test(items_avg_patient_ratio ~ Structure ,
        data = junk,
        alternative = "two.sided",
        paired = FALSE
)

# Either way its still inconclusive WRT prescription items

# Chapter data
# Save this list to file then process with datacheck4 (more or less copy of datacheck2)

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

nearest_docs_file <- "nearest_docs.csv"
nearest_docs_file <- paste(datapath, nearest_docs_file, sep = "/")
nearest_docs_file

# Needs tidy before save
# master_list <- unique(select(pr_main, c("Practice")))
# master_list
# write.csv(file=nearest_docs_file, x=nearest_docs, quote=TRUE, row.names = FALSE)



