# diss01
# run 4
# Several datasets required apart from the prescription data itself
# Practice data
# Post code data (for practice geo-location)
# This to check the dataset for outliers etc prior to any analysis.

library(dplyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(factoextra)
library(ggfortify)
library(cluster) 
library(corrplot)
library(clustMixType)
library(FactoMineR)


# --------------------------------------------------
# Get practice data
# --------------------------------------------------

main_path <- "~/courses/dissertation/data"

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

if (! exists("opar") ) {
  opar <- par()
  }

# Need to better split/refine the geo analysis

# Read in SOA mapping data
# Small Object Area - a reference goeography for NI
datapath <- "general"
datapath <- paste(main_path, datapath, sep = "/")
datapath

SOA_mapping_file <- "Geographic Data (statistical geographies)_SOA.csv"
SOA_mapping_file <- paste(datapath, SOA_mapping_file, sep = "/")
SOA_mapping_file

soa_mapping <- read.csv(SOA_mapping_file)

colnames(soa_mapping)[colnames(soa_mapping) == "ï..SOA.Code"] <-"SOA_Code"
str(soa_mapping)

pr_main <- left_join(pr_main, soa_mapping, 
                      by = c("SOA_CODE" = "SOA_Code")
)

pr_main <- droplevels((pr_main))

# Now have practice data with its associated SOA and other SOA based geographies
colSums(is.na(pr_main))

# remove blank/mainly blank columns
pr_main$WARD1992.2.[pr_main$WARD1992.2. == ""] <- NA

is.na(pr_main) <- pr_main==''
droplevels(pr_main)
pr_main <- select(pr_main , !c(WARD1992.2. ))


# # library(fpc)
# # library(dbscan)
# # library(clValid)
# 
# # Compute clValid
# clmethods <- c("hierarchical","kmeans","pam")
# 
# 
# intern <- clValid(pr_nums, nClust = 2:6,
#                   clMethods = clmethods, validation = "internal")
# # Summary
# summary(intern)
# 
# junk <- scale(pr_nums)
# 
# intern <- clValid(junk, nClust = 2:6,
#                   clMethods = clmethods, validation = "internal")
# # Summary
# summary(intern)
#  
# plot(intern)
# 
# 
# 
# Try another clustering number search
# gap_stat <- clusGap(pr_nums, FUN = kmeans, nstart = 25,
#                    K.max = 10, B = 50)
# Print the result
#print(gap_stat, method = "firstmax")

# To see optinal number of clusters
# fviz_gap_stat(gap_stat)

# indicates 2

# set.seed(25020)
# junk <- kmeans(pr_nums, 4, nstart = 25)
# junk
# 
# km.res <- eclust(pr_nums, "kmeans", k = 4, nstart = 25, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())

# km.res <- eclust(pr_nums, "kmeans", k = 2, nstart = 25, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())
# 
# km.res <- eclust(pr_nums, "kmeans", k = 9, nstart = 25, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())

# stats::prcomp(pr_nums)


# Results look same as the other cluster estimate mechanism
# Is there any point to this considering geo nature of the co-ords


# The master list of practrices to work with based on previously selected outlier removal
datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

master_list_file <- "master_list.csv"

master_list_file <- paste(datapath, master_list_file, sep = "/")
master_list_file
master_list <- read.csv(master_list_file)

# Now to look at chapters in more details
# pull in the data, merge with practice info (if required)
# create chapter specific metric as per the value created for practices

# Read in the practice data including chapter data
chapters_in <- read.csv(paste(datapath, "practice_summary_info.csv", sep = "/"))

# drop data not in master_list
chapters_in <- chapters_in[chapters_in$Practice %in% master_list$Practice, ]
chapters_in <- droplevels(chapters_in)

# Drop chapter 18
chapters_in <- subset(chapters_in, BNF_Chapter != "18")
chapters_in
chapters_in <- droplevels(chapters_in)


# Reconfigure values in the chapters field
chapters_in <- chapters_in %>% mutate(Chapter_lab=recode(BNF_Chapter,
                                               "1" = "Gastro",
                                               "2" = "Cardio", 
                                               "3" = "Resp", 
                                               "4" = "CNS", 
                                               "5" = "Infect",
                                               "6" = "Endo",
                                               "7" = "OGU",
                                               "8" = "MalD_ImmSup",
                                               "9" = "Blod_Nutrt",
                                               "10" = "Musco_Skel",
                                               "11" = "Eye",
                                               "12" = "ENT",
                                               "13" = "Skin",
                                               "14" = "Imm_Vacc",
                                               "15" = "Anaest",
                                               "18" = "Prep_Diag",
                                               "19" = "Other",
                                               "20" = "Dressings",
                                               "21" = "Appliances",
                                               "22" = "Incont",
                                               "23" = "Stoma"
))


ggplot(chapters_in, aes( y=PC_Month_Avg)) + 
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2,
               fill='#A4A4A4', color="black"
  )  + labs(title="Avg Items / Patient", y = "Avg Items per Patient") +  
  facet_wrap(~ Chapter_lab, scale="free")

# 20 different chapters across multiple value ranges (should these be scaled later?)
# Note absence of some chapter codes - 16, 17,18 - leaving 20 chapters 
#
# Leave outliers in place, the point to use that info

# Join chapters data with practices info.
chk <- merge(chapters_in, pr_main_in, by = "Practice")
str(chk)

colnames(chk)

# Need to calculate the chapter items/patient ratio

chk <- select(pr_main, c(Practice, Registered_Patients) )
chk <- merge(chapters_in, chk, by = "Practice")
chk$chapter_avg_patient_ratio <- chk$PC_Month_Avg / chk$Registered_Patients
chk


ggplot(chk, aes( y=chapter_avg_patient_ratio)) + 
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2,
               fill='#A4A4A4', color="black"
  )  + labs(title="Avg Items / Chapter / Patient", y = "Avg Chapter Items per Patient") +  
  facet_wrap(~ Chapter_lab, scale="free")


str(chk)
# chk <- chk[, c(1,4,8)  ]
chk <- chk[, c(1,7,9)  ]
chk

# rename columns
names(chk)[names(chk) == 'Chapter_lab'] <- 'BNF_Chapter'

chk <- pivot_wider(chk, names_from = BNF_Chapter, values_from = chapter_avg_patient_ratio)
chk

# Now have chk being rows of practices with columns of chapter data by chapter item avg value

colSums(is.na(chk))
# Chapter 19 - Other - set non existing to zero
chk[is.na(chk)] <- 0
str(chk)


####### start PCA coding
##  for all practices


pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
pca_chk
#pca_chk <- prcomp(chk, scale = TRUE)

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scre plot all practices")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
      )

# Requires 10 components to cover 90% of the data?


###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
pca <- pca_chk

fviz_pca_var(pca, col.var="contrib",
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19



# View what contributes to the PCA results

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

summary(pca)

# Still low contribution to conponents, though gastro the biggest differentiator

# whats this look like

library(FactoMineR)
pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
res.pca <- PCA(chk[ , 2:20], graph = FALSE)

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1


# Use these figures as the basis for comparision

################################## start colour plots
# possible view componets colouring by groups etc

##################################

# colour put by groups
fviz_pca_ind(pca, axes = c(1,2), colvar = "contrib",
             geom.ind = "point", col.ind = pr_main$C_Group , #  LGD1992NAME  ,
#             palette = c("red", "green"),
             addEllipses = TRUE, 
             legend.totle = "Contrib")


biplot <- fviz_pca_biplot(pca, 
                          # col.ind =  pr_main$C_Group ,
                          #addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

# What this is showing?

biplot <- fviz_pca_ind(pca,
                       geom = "point",
                       col.ind =pr_main$C_Group  ,
                       addEllipses = TRUE,
                       label = "var",
                       col.var = "black",
                       repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)


biplot <- fviz_pca_ind(pca,
                       geom = "point",
                       col.ind = pr_main$LCG  ,
                       addEllipses = TRUE,
                       label = "var",
                       col.var = "black",
                       repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

biplot <- fviz_pca_biplot(pca, 
                          axes = c(1,2),
                          col.ind = pr_main$C_Group,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

biplot <- fviz_pca_biplot(pca, 
                          axes = c(1,2),
                          col.ind = pr_main$LCG,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)


biplot <- fviz_pca_biplot(pca, 
                          axes = c(1,3),
                          col.ind = pr_main$C_Group,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

biplot <- fviz_pca_biplot(pca, 
                          axes = c(3,4),
                          col.ind = pr_main$C_Group,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

# If Gastro and blood/nutrition are main elements of PCA components, then what?

################################## end colour plots
## end PCA coding for all practices
##################################


####### start PCA coding for clusters

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

grouped_clusters_file <- "grouped_clusters.csv"
grouped_clusters_file <- paste(datapath, grouped_clusters_file, sep = "/")
grouped_clusters_file

grp_clusters <- read.csv(grouped_clusters_file)

####### start PCA coding for clusters

## # start of the 24 size sample

# First pick up clusters

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

nearest_docs_file <- "nearest_docs_24.csv"
nearest_docs_file <- paste(datapath, nearest_docs_file, sep = "/")
nearest_docs_file

nearest_docs <- read.csv(nearest_docs_file)

clusters <- select(nearest_docs, c(Practice, PID, Structure))

clusters$Cluster_Grp <- "2"
clusters$Cluster_Grp[clusters$Structure == "NI_III"] <- "1"
clusters$Cluster_Grp[clusters$Structure == "NI_II"] <- "1"


#######################################
# run PCA code on the the 2 clusters only

clst  <- chk[chk$Practice %in% clusters$Practice, ]
clst = merge(clst, clusters)
clst <- droplevels(clst)

pca_chk <- prcomp(clst[ , 2:21], scale = TRUE)
pca_chk

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scre plot group1")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# Requires 8-9 components to cover 90% of the data?

###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
pca <- pca_chk

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19



# View what contributes to the PCA results

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

summary(pca)


clusters$Cluster_Grp.f <- as.factor(clusters$Cluster_Grp)
## visualise PCA direction vectors 
biplot <- fviz_pca_biplot(pca_chk, 
                          axes = c(1,2),
#                          col.ind = clusters$Cluster_Grp.f,
#                          col.ind = "blue", # clusters$Cluster_Grp.f,
                          addEllipses = TRUE,
                          label = "var",
                          habillage = clusters$Cluster_Grp.f ,
#                          col.var = "black",
                          repel = TRUE
, palette = "Dark2"
)

data(iris)
res.pca <- prcomp(iris[, -5],  scale = TRUE)

clst2 = merge(clst, clusters)

res.pca <- prcomp(clst2[ , 2:21], scale = TRUE)

fviz_pca_ind(res.pca, label="none", habillage= clst2$Cluster_Grp.f ,
             addEllipses=TRUE, ellipse.level=0.95,
             palette = c("#999999", "#E69F00", "#56B4E9"))

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)


# end PCA code on the clusters only
############################################

# Split into clusters 1 and 2

cluster_1 <- subset(clusters, clusters$Cluster_Grp == 1)
cluster_2 <- subset(clusters, clusters$Cluster_Grp == 2)


####### start PCA coding for cluster 01


clst  <- chk[chk$Practice %in% cluster_1$Practice, ]
clst <- droplevels(clst)


pca_chk <- prcomp(clst[ , 2:20], scale = TRUE)
pca_chk

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scre plot group1")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# Requires 10 components to cover 90% of the data?

###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
pca <- pca_chk

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19



# View what contributes to the PCA results

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

summary(pca)

# Still low contribution to conponents, though gastro the biggest differentiator

# whats this look like

# # library(FactoMineR)
# pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
# res.pca <- PCA(chk[ , 2:20], graph = FALSE)
# 
# res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# # Description of dimension 1
# res.desc$Dim.1

################################## end PCA cluster 01


####### start PCA coding for cluster 02


clst  <- chk[chk$Practice %in% cluster_2$Practice, ]
clst <- droplevels(clst)


pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
pca_chk

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scre plot group1")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# Requires 10 components to cover 90% of the data?

###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
pca <- pca_chk

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19



# View what contributes to the PCA results

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

summary(pca)

# Still low contribution to conponents, though gastro the biggest differentiator

# whats this look like

library(FactoMineR)
pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
res.pca <- PCA(chk[ , 2:20], graph = FALSE)

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1

################################## end PCA cluster 02


## # end of the 24 size sample


## # start of the 42 size sample

# First pick up clusters

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

nearest_docs_file <- "nearest_docs_42.csv"
nearest_docs_file <- paste(datapath, nearest_docs_file, sep = "/")
nearest_docs_file

nearest_docs <- read.csv(nearest_docs_file)

clusters <- select(nearest_docs, c(Practice, PID, Structure))

clusters$Cluster_Grp <- "2"
clusters$Cluster_Grp[clusters$Structure == "NI_III"] <- "1"
clusters$Cluster_Grp[clusters$Structure == "NI_II"] <- "1"


# Split into clusters 1 and 2

cluster_1 <- subset(clusters, clusters$Cluster_Grp == 1)
cluster_2 <- subset(clusters, clusters$Cluster_Grp == 2)


####### start PCA coding for cluster 01

clst  <- chk[chk$Practice %in% cluster_1$Practice, ]
clst <- droplevels(clst)


pca_chk <- prcomp(clst[ , 2:21], scale = TRUE)
pca_chk

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scre plot group1")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

#  Requires 6-7 components to cover 90% of the data?

###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
pca <- pca_chk

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19



# View what contributes to the PCA results

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

summary(pca)

# Still low contribution to conponents, though gastro the biggest differentiator

# whats this look like


################################## end PCA cluster 01


####### start PCA coding for cluster 02


clst  <- chk[chk$Practice %in% cluster_2$Practice, ]
clst <- droplevels(clst)

pca_chk <- prcomp(clst[ , 2:20], scale = TRUE)
pca_chk

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scre plot group1")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# Requires 10 components to cover 90% of the data?

###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
fviz_pca_var(pca_chk, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca_chk, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca_chk, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19


# View what contributes to the PCA results

fviz_contrib(pca_chk, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_chk, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_chk, choice = "var", axes = 3, top = 10)
fviz_contrib(pca_chk, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca_chk, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

summary(pca_chk)

# Still low contribution to components, though gastro remains the biggest differentiator

################################## end PCA cluster 02


# Gastro,Cardio, Resp, CNS, Endo, OGU, Blod Nutrt, Musco Skel, Skin, Dressings

chk_to_chk <- subset(chk, select = c(Gastro,Cardio, Resp, CNS, Endo, OGU, Skin, Dressings, Blod_Nutrt, Musco_Skel ))
str(chk_to_chk)

pca_chk <- prcomp(chk_to_chk[ , 1:10], scale = TRUE)
pca_chk
#pca_chk <- prcomp(chk, scale = TRUE)

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 60), title = "Scree plot all practices", ncp = 20)
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# Shifts to 80% from 3


###########################################


pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Conveinience copy ...
pca <- pca_chk

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

head(pca_forvariables$cos2, 20)

head(pca_forvariables$contrib, 5)
corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19



# View what contributes to the PCA results

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
# fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# whats point in 3 dimensions?
# red line the average expected contribution

summary(pca)

# Still low contribution to conponents, though gastro the biggest differentiator

# whats this look like

pca_chk <- prcomp(chk_to_chk[ , 1:10], scale = TRUE)
res.pca <- PCA(chk_to_chk[ , 1:10], graph = FALSE)


res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1


# Use these figures as the basis for comparision

################################## start colour plots
# possible view componets colouring by groups etc

##################################

# colour put by groups
fviz_pca_ind(pca, axes = c(1,2), colvar = "contrib",
             geom.ind = "point", col.ind =  pr_main$C_Group , #  LGD1992NAME  ,
#                          palette = c("red", "green"),
             addEllipses = TRUE, 
             legend.title = "Contrib")


biplot <- fviz_pca_biplot(pca, 
                          # col.ind =  pr_main$C_Group ,
                          #addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

# What this is showing?

biplot <- fviz_pca_ind(pca,
                       geom = "point",
                       col.ind =pr_main$C_Group  ,
                       addEllipses = TRUE,
                       label = "var",
                       col.var = "black",
                       repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)


biplot <- fviz_pca_ind(pca,
                       geom = "point",
                       col.ind = pr_main$LCG  ,
                       addEllipses = TRUE,
                       label = "var",
                       col.var = "black",
                       repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

biplot <- fviz_pca_biplot(pca, 
                          axes = c(1,2),
                          col.ind = pr_main$C_Group,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

biplot <- fviz_pca_biplot(pca, 
                          axes = c(1,2),
                          col.ind = pr_main$LCG,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)


biplot <- fviz_pca_biplot(pca, 
                          axes = c(1,3),
                          col.ind = pr_main$C_Group,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

biplot <- fviz_pca_biplot(pca, 
                          axes = c(3,4),
                          col.ind = pr_main$C_Group,
                          addEllipses = TRUE,
                          label = "var",
                          col.var = "black",
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "BNF Chapter Avg Items prescribed data",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)

# If Gastro and blood/nutrition are main elements of PCA components, then what?






############################## EOF

