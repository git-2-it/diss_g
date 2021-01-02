# diss01
# SA/SOA religious affiliation

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

# aoi_list_soa <- aoi_boundary_HARV %>% select(SOA_CODE, SOA_LABEL) 

#### crs found


# Grabbed the SOA atlas data, join with the clusters info


########### 
# Read the clusters and SOA data

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

clusters_soa$SOA_CODE <- as.character(clusters_soa$SOA_CODE)

clusters_soa$Cluster_Grp <- "2"
clusters_soa$Cluster_Grp[clusters_soa$Structure == "NI_III"] <- "1"
clusters_soa$Cluster_Grp[clusters_soa$Structure == "NI_II"] <- "1"

# # Create cluster points
# cluster_pts <- clusters_soa[c("eastings", "northings", "PID")]
# colnames(cluster_pts) <- c("x", "y", "PID")
# cluster_pts$PID <- as.character(cluster_pts$PID)
# cluster_pts
# 
# coordinates(cluster_pts) <- ~ x + y
# class(cluster_pts)
# cluster_pts <- st_as_sf(cluster_pts)
# 
# plot(cluster_pts)
# st_crs(cluster_pts)
# 
# cluster_pts <- st_set_crs(cluster_pts, crs_in)

# Read in SOA mapping data
# Super Object Area - a reference goeography for NI
datapath <- "general"
datapath <- paste(main_path, datapath, sep = "/")
datapath

SOA_mapping_file <- "Geographic Data (statistical geographies)_SOA.csv"
SOA_mapping_file <- paste(datapath, SOA_mapping_file, sep = "/")
SOA_mapping_file

soa_mapping <- read.csv(SOA_mapping_file)

colnames(soa_mapping)[colnames(soa_mapping) == "ï..SOA.Code"] <-"SOA_Code"
str(soa_mapping)

# # Create new column to get "normalised SOA"
# soa_mapping$SOA_CODE_sub <- substring(soa_mapping[,1], 1, 6) 


# Small Area - a reference goeography for NI
datapath <- "general"
datapath <- paste(main_path, datapath, sep = "/")
datapath

SA_mapping_file <- "Geographic Data (statistical geographies)_SA.csv"
SA_mapping_file <- paste(datapath, SA_mapping_file, sep = "/")
SA_mapping_file

sa_mapping <- read.csv(SA_mapping_file)

colnames(sa_mapping)[colnames(sa_mapping) == "ï..SA"] <-"SA"
str(sa_mapping)


# read in the religon data
# airo data looks wrong!
# Use NI data


datapath <- "ninis2"
datapath <- paste(main_path, datapath, sep = "/")
datapath

rel_ai_file <- "ninis2_KS212NI (s).csv"
rel_ai_file <- paste(datapath, rel_ai_file, sep = "/")
rel_ai_file

rel_ai <- read.csv(rel_ai_file)
str(rel_ai)

colnames(rel_ai) <- c("SA Label",
                      "SA",
                      "Residents",
                      "Catholic",
                      "Protestant/OC",
                      "Other",
                      "None"
)
rel_ai <- rel_ai[,1:7]

str(rel_ai)

head(rel_ai)

# remove factors
rel_ai$SA <- as.character(rel_ai$SA)
sa_mapping$SA <- as.character(sa_mapping$SA)

colSums(is.na(rel_ai))
colSums(is.na(sa_mapping))

rel_ai <- merge(rel_ai, sa_mapping )

rel_ai <- rel_ai[,1:8]

# Select only areas as per the clusters list
# Mapping needed for small area to SOA (Super Output Area)


examine_rel <- rel_ai[rel_ai$SOA %in% clusters_soa$SOA_CODE , ]

examine_rel$Catholic <- as.numeric(examine_rel$Catholic)
cath <- aggregate(examine_rel$Catholic ,
                  by=list(
                    examine_rel$SOA
                  ),
                  FUN=sum)
colnames(cath) <- c("SOA", "cath") 

examine_rel$OTH_C <- as.numeric(examine_rel$`Protestant/OC`)
oth_c <- aggregate(examine_rel$OTH_C,
                   by=list(
                     examine_rel$SOA
                   ),
                   FUN=sum)
colnames(oth_c) <- c("SOA", "oth_c") 

nc_oth <- aggregate(examine_rel$Other,
                    by=list(
                      examine_rel$SOA
                    ),
                    FUN=sum)
colnames(nc_oth) <- c("SOA", "nc_oth") 


examine_rel$None <- as.numeric(examine_rel$None)
no_rel <- aggregate(examine_rel$None,
                    by=list(
                      examine_rel$SOA
                    ),
                    FUN=sum)
colnames(no_rel) <- c("SOA", "no_rel") 


collected <- merge(cath, oth_c, by = "SOA")
collected <- merge(collected, nc_oth , by = "SOA" )
collected <- merge(collected, no_rel , by = "SOA")

colSums(is.na(collected))


collected$total <- rowSums(collected[2:5])

collected$pc_ro_cath <- (collected$cath / collected$total) * 100
collected$pc_oth_c <- (collected$oth_c / collected$total) * 100
collected$pc_nc_oth <- (collected$nc_oth / collected$total) * 100
collected$pc_no_rel <- (collected$no_rel / collected$total) * 100


collected$maj[collected$pc_ro_cath > collected$pc_oth_c] <- "C"
collected$maj[collected$pc_ro_cath <= collected$pc_oth_c] <- "P"

collected$SOA <- as.character(collected$SOA)

colnames(collected)[colnames(collected) == "SOA"] <-"SOA_CODE"


merged_df <- merge(collected, clusters_soa, by.x = 'SOA_CODE', by.y = 'SOA_CODE' )


observed1 <- table( merged_df$maj )
observed1

observed2 <- table( merged_df$Cluster_Grp )
observed2


library(lsr)

merged_df$maj.f <- as.factor(merged_df$maj)
merged_df$Cluster_Grp.f <- as.factor(merged_df$Cluster_Grp)

associationTest( formula = ~maj.f+Cluster_Grp.f, data = merged_df )


# Alternative is Fishers test for smaller sample size
# set up (need matrix of counts)
chk <- merged_df[, c(20,21)]
chk

# Leads to matrix
#   1	2
# c	7	7
# p	4	6

data <- data.frame(c(7, 7),
                   c(4,6)
)
fisher.test(data)

chisq.test(data)$expected
chisq.test(data)


# run test on the percentages
# One-way ANOVA

anova_one_way <- aov(pc_ro_cath ~ Cluster_Grp, data = merged_df)
summary(anova_one_way)

anova_one_way <- aov(pc_oth_c ~ Cluster_Grp, data = merged_df)
summary(anova_one_way)

t.test(pc_ro_cath ~ Cluster_Grp ,
       data = merged_df,
       alternative = "two.sided",
       paired = FALSE
)

# No significant correllations

# Possible to get cluster data at SA level?

# Read the clusters and SOA data

datapath <- "atlas"
datapath <- paste(main_path, datapath, sep = "/")
datapath

clusters_sa_file <- "clusters_SA.csv"
clusters_sa_file <- paste(datapath, clusters_sa_file, sep = "/")

clusters_in <- read.csv(clusters_sa_file)

clusters_sa <- select(clusters_in, c(Postcode, PID, Structure, 
                                     SA2011,
                                     eastings, northings,
                                     latitude, longitude
))

str(clusters_sa)

clusters_sa$SA2011 <- as.character(clusters_sa$SA2011)

clusters_sa$Cluster_Grp <- "2"
clusters_sa$Cluster_Grp[clusters_sa$Structure == "NI_III"] <- "1"
clusters_sa$Cluster_Grp[clusters_sa$Structure == "NI_II"] <- "1"

# Reread the religon data
datapath <- "ninis2"
datapath <- paste(main_path, datapath, sep = "/")
datapath

rel_ai_file <- "ninis2_KS212NI (s).csv"
rel_ai_file <- paste(datapath, rel_ai_file, sep = "/")
rel_ai_file

rel_ai <- read.csv(rel_ai_file)
str(rel_ai)

colnames(rel_ai) <- c("SA Label",
                      "SA",
                      "Residents",
                      "Catholic",
                      "Protestant/OC",
                      "Other",
                      "None",
                      "PC_Catholic",
                      "PC_Protestant_OC",
                      "PC_Other",
                      "PC_None"
                      
)
# rel_ai <- rel_ai[,1:7]


# remove factors
rel_ai$SA <- as.character(rel_ai$SA)
sa_mapping$SA <- as.character(sa_mapping$SA)

examine_rel <- rel_ai[rel_ai$SA %in% clusters_sa$SA2011 , ]

colSums(is.na(rel_ai))
colSums(is.na(sa_mapping))

examine_rel$maj[examine_rel$PC_Catholic > examine_rel$PC_Protestant_OC] <- "C"
examine_rel$maj[examine_rel$PC_Catholic <= examine_rel$PC_Protestant_OC] <- "P"

sa_chk <- merge(examine_rel, clusters_sa, by.x = 'SA', by.y = 'SA2011' )

observed1 <- table( sa_chk$maj )
observed1

observed2 <- table( sa_chk$Cluster_Grp )
observed2

str(sa_chk)

to_test <- subset(sa_chk, 
                  select = c("SA", "Cluster_Grp", "Cluster_Grp.f", "maj", "maj.f")
)

sa_chk$maj.f <- as.factor(sa_chk$maj)
sa_chk$Cluster_Grp.f <- as.factor(sa_chk$Cluster_Grp)

associationTest( formula = ~maj.f+Cluster_Grp.f, data = sa_chk )

summary(to_test)
str(to_test)
head(to_test)

associationTest( formula = ~maj.f+Cluster_Grp.f, data = to_test )

chisq.test(to_test)


# This p-value is low, p-value = 0.014

# Alternative is Fishers test for smaller sample size
# set up (need matrix of counts)
chk <- sa_chk[, c(21,22)]
chk

# Leads to new matrix
#   1	  2
#c	4	  12
#p	7	  1


matrix_data <- data.frame(c(4, 12),
                          c(7,1)
)
fisher.test(matrix_data)


# # # # # #

matrix_data <- data.frame(c(1, 9),
                          c(11,3)
)
fisher.test(matrix_data)


# # # # # 

# p = 0.0078

# install.packages("Barnard")
library(Barnard)

barnard.test(4,12,7,1)

chisq.test(data)$expected
chisq.test(data)


# run test on the percentages?
# One-way ANOVA
# Kruskal-Wallis test (for 3+ groups)

# anova_one_way <- aov(pc_ro_cath ~ Cluster_Grp, data = merged_df)
# summary(anova_one_way)

anova_one_way <- aov(PC_Catholic ~ Cluster_Grp, data = sa_chk)
summary(anova_one_way)
# Also high significant
# p = 0.00711


t.test(PC_Catholic ~ Cluster_Grp ,
       data = sa_chk,
       alternative = "two.sided",
       paired = FALSE
)
# Also significant, p = 0.001242


