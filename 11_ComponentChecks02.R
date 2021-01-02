# diss01
# run PCA sections

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

pca_chk <- prcomp(chk[ , 2:21], scale = TRUE)
pca_chk

summary(pca_chk)

pca_chk$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50), title = "Scree plot all practices")
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# Requires 11 components to cover 90% of the data?

# what are they variables?
pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

fviz_contrib(pca_chk, choice = "var", axes = 1, top = 10)
# Values cross check with teh heatmap above
fviz_contrib(pca_chk, choice = "var", axes = 1, top = 20)

corrplot(pca_forvariables$contrib, is.corr = FALSE)
# Hmmm, though gastro a strong contributor to dim1, its a stronger contributor to dim19 and 18
# pattern spread across many variables


fviz_contrib(pca_chk, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_chk, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_chk, choice = "var", axes = 3, top = 10)
fviz_contrib(pca_chk, choice = "var", axes = 1:2, top = 20)
# fviz_contrib(pca_chk, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

fviz_contrib(pca_chk, choice = "var", axes = 1:2, top = 10,
              title = "Top 10 contributors to all practices")

summary(pca_chk)


fviz_contrib(pca_chk, choice = "var", axes = 1, top = 5)
fviz_contrib(pca_chk, choice = "var", axes = 2, top = 5)

### kmeans for all 
# no point in kmeans for all, the data is too disjointed
# 
# comp_all <- data.frame(pca_chk$x[,1:3])
# # Plot
# plot(comp_all, pch=16, col=rgb(0,0,0,0.5))
# 
# k_val <- fviz_nbclust(comp_all, FUNcluster = kmeans)
# k_val <- fviz_nbclust(comp_all, kmeans, method = "silhouette")
# k_val
# # k_val <- fviz_nbclust(comp, kmeans, method = "wss")
# # k_val
# # k_val <- fviz_nbclust(comp, kmeans, method = "gap_stat")
# # k_val
# 
# # k <- kmeans(comp, 4, nstart=25, iter.max=1000)
# # k9 <- kmeans(comp, 9, nstart=25, iter.max=1000)
# 
# k_all <- kmeans(comp_all, 2, nstart=25, iter.max=1000)
# 
# 
# plot(comp, col=k_all$cluster, pch=16) #  + geom_text(aes(label=clusters$Practice),hjust=0, vjust=0)
# 
# plot(comp, col=clusters$Cluster_Grp, pch=16) #  + geom_text(aes(label=clusters$Practice),hjust=0, vjust=0)
# 
# 
# shapes = c(16, 17, 18, 19, 20, 1, 2, 3, 4) 
# shapes <- shapes[as.numeric(k_all$clust)]
# shapes.f <- as.factor(shapes)
# 
# shapes <- k_all$clust
# 
# # Plot
# plot(comp_all, col=clusters$Cluster_Grp, pch = shapes ) 
# 
# legend("topright", legend = levels(clusters$Cluster_Grp.f),
#        col =  c("#00AFBB", "#E7B800", "#FC4E07"),
#        pch = c(16, 17, 18) )
# 
# 
# ## end kmeans for all

################################## 
## end PCA coding for all practices
##################################

####### start PCA coding for clusters

# First pick up clusters

datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

grouped_clusters_file <- "grouped_clusters.csv"
# grouped_clusters_file <- "new_grouped_clusters.csv"
grouped_clusters_file <- paste(datapath, grouped_clusters_file, sep = "/")
grouped_clusters_file

grp_clusters <- read.csv(grouped_clusters_file)

####### start PCA coding for clusters

## # start of the 24 size sample

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
# run PCA code on the 2 clusters together

clst  <- chk[chk$Practice %in% clusters$Practice, ]
clst = merge(clst, clusters)
clst <- droplevels(clst)

pca_chk24 <- prcomp(clst[ , 2:21], scale = TRUE)
pca_chk24

summary(pca_chk24)

pca_chk24$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk24, addlabels = TRUE, ylim = c(0, 70), title = "Scree plot clusters")
plot1

# Different pattern over the clusters
# 1-2 covers 65% of variance
# 1-3 covers 75% of variance

pr_var = ( pca_chk24$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# 6-7 variables to cover 90% of variance

# What are they?

pca_forvariables <- get_pca_var(pca_chk24)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

fviz_contrib(pca_chk24, choice = "var", axes = 1, top = 10)
# Values cross check with the heatmap above
fviz_contrib(pca_chk24, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_chk24, choice = "var", axes = 3, top = 10)
fviz_contrib(pca_chk24, choice = "var", axes = 1:2, top = 20)
# fviz_contrib(pca, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

clusters$Cluster_Grp.f <- as.factor(clusters$Cluster_Grp)
## visualise PCA direction vectors 
biplot <- fviz_pca_biplot(pca_chk24, 
                          axes = c(1,2),
                          addEllipses = TRUE,
                          label = "var",
                          habillage = clusters$Cluster_Grp.f ,
                          repel = TRUE
)

ggpar(biplot, 
      title = "Chapter PCA Analysis",
      subtitle = "Cluster comparison, 24 practices",
      xlab = "PC 1",
      ylab = "PC 2",
      legend.title = "Cluster",
      legend.position = "top",
      ggtheme = theme_light(),
      palette = "jco"
)  
# + geom_text(aes(label=clusters$Practice),hjust=0, vjust=0)
# + geom_point(size = 1.1, stroke = 0, shape = 16)

# end PCA code on the 2 clusters together for the 24 size sample
############################################

# Can see there are 2 overlapped groups?
# Note the outlier practices in cluster grp2

# Increase the number of practices to gather more data into analysis
## # start of the 42 size sample

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


#######################################
# run PCA code on the 2 clusters together, 42 practices

clst  <- chk[chk$Practice %in% clusters$Practice, ]
clst = merge(clst, clusters)
clst <- droplevels(clst)

pca_chk42 <- prcomp(clst[ , 2:21], scale = TRUE)
pca_chk42

summary(pca_chk42)

pca_chk42$x[1:5,1:3]

plot1 <- fviz_eig(pca_chk42, addlabels = TRUE, ylim = c(0, 60), title = "Scree plot clusters", ncp = 20)
plot1

summary(pca_chk42)
# Different pattern over the clusters
# 1 dim = 43%
# 1-3 covers < 65% of variance

pr_var = ( pca_chk42$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# 10 variables to cover 90% of variance

pca_forvariables <- get_pca_var(pca_chk42)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)


fviz_contrib(pca_chk42, choice = "var", axes = 1, top = 10)
# Values cross check with the heatmap above
fviz_contrib(pca_chk42, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_chk42, choice = "var", axes = 3, top = 10)
fviz_contrib(pca_chk42, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca_chk42, choice = "var", axes = 1:2, top = 10, 
             title = "Top 10 contributors to PC1 & PC2")
# fviz_contrib(pca_chk42, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution

clusters$Cluster_Grp.f <- as.factor(clusters$Cluster_Grp)
## visualise PCA direction vectors 
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
)  
# + geom_text(aes(label=clusters$Practice),hjust=0, vjust=0)


biplot <- fviz_pca_ind(pca_chk42,
                       geom = "point",
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
)  

fviz_contrib(pca_chk42, choice = "var", axes = 1:2, top = 20, 
             title = "Top 10 contributors to PC1 & PC2")

fviz_contrib(pca_chk42, choice = "var", axes = 1:2, top = 10, 
             title = "Top 10 contributors to PC1 & PC2")

fviz_contrib(pca_chk24, choice = "var", axes = 1:2, top = 10, 
             title = "Top 10 contributors to PC1 & PC2")

# par(mfrow = c(2, 2))
  
fviz_contrib(pca_chk42, choice = "var", axes = 1, top = 5)
# Values cross check with the heatmap above
fviz_contrib(pca_chk42, choice = "var", axes = 2, top = 5)

### end 42


####################################### 
# kmeans etc

library(RColorBrewer)
library(scales)
library(lsr)
library(psych)


## 42

### which clustering to use?

library(clValid)

# Compute clValid
clmethods <- c("kmeans","pam")

comp <- data.frame(pca_chk42$x[,1:3])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

intern <- clValid(comp, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

optimalScores(intern)
# plot(intern)

## pam

k_val <- fviz_nbclust(comp, FUNcluster = kmeans, method = "silhouette")
k_val
k_val <- fviz_nbclust(comp, FUNcluster = pam, method = "silhouette")
k_val

library("cluster")

### kmeans, k = 2, eclust

# k2 <- kmeans(comp, 2, nstart=25, iter.max=1000)

km.res <- eclust(comp, "kmeans", k = 2, nstart = 25, iter.max=1000, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())

km_c2 <- km.res$cluster
km_c2.df<- data.frame(km_c2)


km_k2 <- cbind(km_c2.df, clusters)
km_k2

km_k2 <- subset(km_k2, 
                  select = c("km_c2", "Cluster_Grp")
)

km_k2$Cluster_Grp.f <- as.factor(km_k2$Cluster_Grp)
km_k2$km_c2.f <- as.factor(km_k2$km_c2)

associationTest( formula = ~km_c2.f+Cluster_Grp.f, data = km_k2 )

pair_variables <- c("Cluster_Grp",
                    "km_c2"
)

pairs.panels(km_k2[pair_variables], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of patient numbers and item prescription figures"
)



## end kmeans, k = 2, eclust

# PAM, k = 6


pam.res <- pam(comp, 6)
# Visualize
fviz_cluster(pam.res)

pam.res

pam_6 <- cbind(comp, cluster = pam.res$cluster)

pam_6 <- cbind(pam_6, clusters)

head(pam_6, n = 3)

dd$cluster.f <- as.factor(pam_6$cluster)

associationTest( formula = ~cluster.f+Cluster_Grp.f, data = pam_6 )

pair_variables <- c("Cluster_Grp",
                    "cluster"
)

pairs.panels(pam_6[pair_variables], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of patient numbers and item prescription figures"
)


# cluster.f 1 2
# 1 2 5
# 2 4 8
# 3 5 1
# 4 5 7
# 5 2 2
# 6 0 1

data <- data.frame(
  c(2, 5),
  c(4, 8),
  c(5, 1),
  c(5, 7),
  c(2, 2),
  c(0, 1)
  )

fisher.test(data)

chisq.test(data)$expected
chisq.test(data)

## pam, k = 6

## pam, k = 2

pam.res <- pam(comp, 2)
# Visualize
fviz_cluster(pam.res)

pam.res

pam_2 <- cbind(comp, cluster = pam.res$cluster)

pam_2 <- cbind(dd, clusters)

head(pam_2, n = 3)

pam_2$cluster.f <- as.factor(dd$cluster)

associationTest( formula = ~cluster.f+Cluster_Grp.f, data = pam_2 )

pair_variables <- c("Cluster_Grp",
                    "cluster"
)

pairs.panels(pam_2[pair_variables], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of patient numbers and item prescription figures"
)



# Alternative is Fishers test for smaller sample size
# set up (need matrix of counts)
# Leads to matrix
#   1	2
# c	7	11
# p	11 13

data <- data.frame(c(7, 11),
                   c(11,13)
)

fisher.test(data)

chisq.test(data)$expected
chisq.test(data)

## pam, k = 2

## end pam




# running with longer RoH

## tidied


# long_roh <- subset(dd, dd$PID == "ATLAS106" | dd$PID == "ATLAS054" | dd$PID == "ATLAS102"
#                    | dd$PID ==  "ATLAS162" | dd$PID ==  "ATLAS070")

#######################################
# run PCA code on the 2 clusters together, 42 practices

clst_roh <- subset(clst, clst$PID == "ATLAS106" | clst$PID == "ATLAS054" | clst$PID == "ATLAS102"
                   | clst$PID ==  "ATLAS162" | clst$PID ==  "ATLAS070" 
                   )


clusters_roh <- subset(clusters, clusters$PID == "ATLAS106" | clusters$PID == "ATLAS054" | 
                         clusters$PID == "ATLAS102"
                       | clusters$PID ==  "ATLAS162" | clusters$PID ==  "ATLAS070"
)

pca_chkroh <- prcomp(clst_roh[ , 2:21], scale = TRUE)
pca_chkroh

summary(pca_chkroh)

pca_chkroh$x[1:5,1:3]

plot1 <- fviz_eig(pca_chkroh, addlabels = TRUE, ylim = c(0, 70), title = "Scree plot clusters" ) 
# , ncp = 20) - no need as dimensions tail of at 9.
plot1

# Different pattern over the clusters
# 1 dim = 43%
# 1-3 covers < 65% of variance

pr_var = ( pca_chkroh$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" 
)

# 4-5 variables to cover 90% of variance

pca_forvariables <- get_pca_var(pca_chkroh)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)


fviz_contrib(pca_chkroh, choice = "var", axes = 1, top = 10)
# Values cross check with the heatmap above
fviz_contrib(pca_chkroh, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_chkroh, choice = "var", axes = 3, top = 10)
fviz_contrib(pca_chkroh, choice = "var", axes = 1:2, top = 20)
fviz_contrib(pca_chkroh, choice = "var", axes = 1:2, top = 10, 
             title = "Top 10 contributors to PC1 & PC2")
# fviz_contrib(pca_chk42, choice = "var", axes = 1:3, top = 20, addlabels = TRUE)
# red line the average expected contribution
fviz_contrib(pca_chkroh, choice = "var", axes = 1:4, top = 10, 
             title = "Top 10 contributors to RoH PC")

## roh cluster

### kmeans, k = 2, eclust

# k2 <- kmeans(comp, 2, nstart=25, iter.max=1000)

comp_roh <- data.frame(pca_chkroh$x[,1:4])  ## should be 4 - 5?
# Plot
plot(comp_roh, pch=16, col=rgb(0,0,0,0.5))

km.roh <- eclust(comp_roh, "kmeans", k = 2, nstart = 25, iter.max=1000, graph = FALSE)

km_c2_roh <- km.roh$cluster
km_c2_roh.df <- data.frame(km_c2_roh)

km_c2_roh <- cbind(km_c2_roh.df, clusters_roh)
km_c2_roh

km_k2_roh <- subset(km_c2_roh, 
                    select = c("km_c2_roh", "Cluster_Grp")
)


km_k2_roh$Cluster_Grp.f <- as.factor(km_k2_roh$Cluster_Grp)
km_k2_roh$km_c2.f <- as.factor(km_k2_roh$km_c2)

associationTest( formula = ~km_c2.f+Cluster_Grp.f, data = km_k2_roh )


# Observed contingency table: -- pca - 1-4
#   Cluster_Grp.f
# km_c2.f 1 2
# 1 2 0
# 2 1 6

pair_variables <- c("Cluster_Grp",
                    "km_c2_roh"
)


pairs.panels(km_k2_roh[pair_variables], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of patient numbers and item prescription figures"
)

roh_data <- data.frame(c(2, 0),
                       c(1,6)
)

fisher.test(roh_data)
fisher.test(roh_data2)


# install.packages("Barnard")
library(Barnard)

barnard.test(2,0,1,6)


# bt<-barnard.test(40,14,10,30)
# bts<-bt$statistic.table
# plot(bts[,1],bts[,2],
#      col=hsv(bts[,4]/4,1,1),
#      t="p",xlab="n1",ylab="n2")


### 

# Observed contingency table: -- pca - 1-3
#   Cluster_Grp.f
# km_c2.f 1 2
# 1 2 1
# 2 1 5
# 
# pairs.panels(km_k2_roh[pair_variables], 
#              method = "pearson", # correlation method
#              hist.col = "#00AFBB",
#              density = TRUE,  # show density plots
#              ellipses = TRUE # show correlation ellipses
#              , main = "Correlation check of patient numbers and item prescription figures"
# )
# 
# roh_data <- data.frame(c(2, 1),
#                        c(1,5)
# )
# 
# fisher.test(roh_data)
# 

## end roh cluster

## end RoH run



### end which type of clustering




pca_chk42

comp <- data.frame(pca_chk42$x[,1:3])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

k_val <- fviz_nbclust(comp, FUNcluster = kmeans)
k_val <- fviz_nbclust(comp, kmeans, method = "silhouette")
k_val
# k_val <- fviz_nbclust(comp, kmeans, method = "wss")
# k_val
# k_val <- fviz_nbclust(comp, kmeans, method = "gap_stat")
# k_val

# k <- kmeans(comp, 4, nstart=25, iter.max=1000)
# k9 <- kmeans(comp, 9, nstart=25, iter.max=1000)

k2 <- kmeans(comp, 2, nstart=25, iter.max=1000)
k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)

k <- k2
k <- k6

# palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16) #  + geom_text(aes(label=clusters$Practice),hjust=0, vjust=0)

plot(comp, col=clusters$Cluster_Grp, pch=16) #  + geom_text(aes(label=clusters$Practice),hjust=0, vjust=0)


shapes = c(16, 17, 18, 19, 20, 1, 2, 3, 4) 
shapes <- shapes[as.numeric(k$clust)]
shapes.f <- as.factor(shapes)

shapes <- k$clust

# Plot
plot(comp, col=clusters$Cluster_Grp, pch = shapes ) 

legend("topright", legend = levels(clusters$Cluster_Grp.f),
       col =  c("#00AFBB", "#E7B800", "#FC4E07"),
       pch = c(16, 17, 18) )



biplot <- fviz_pca_ind(pca_chk42,
                       geom = "point",
                       addEllipses = TRUE,
                       label = "var",
                       # habillage = clusters$Cluster_Grp.f ,
                       # habillage =k$clust ,
                       repel = TRUE,
                       # col.ind = clusters$Cluster_Grp.f 
                       # , pch = shapes
                       col.ind = shapes.f
#                        , pch = clusters$Cluster_Grp.f 
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
)  


c2 <- k$cluster
c2.df<- data.frame(c2)


sa_junk <- cbind(c2.df, clusters)
sa_junk


sa_junk <- subset(sa_junk, 
                  select = c("c2", "Cluster_Grp")
)

sa_junk$Cluster_Grp.f <- as.factor(sa_junk$Cluster_Grp)
sa_junk$c2.f <- as.factor(sa_junk$c2)

associationTest( formula = ~c2.f+Cluster_Grp.f, data = sa_junk )

pair_variables <- c("Cluster_Grp",
                    "c2"
)

pairs.panels(sa_junk[pair_variables], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of patient numbers and item prescription figures"
)


## 42


#######################################

####### EOF