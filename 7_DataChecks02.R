# diss01
# Exploring the data in different ways, 
# Writes the master data to file, ie list of practies to work from

library(dplyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(factoextra)
library(ggfortify)
library(cluster) 
library(corrplot)


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

par(mfrow = c(1, 1))

# This to basically review the practices main dataset "grouping"
ggline(pr_main, x = "LCG", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "LCG")  + geom_point(aes(color=C_Group))

ggline(pr_main, x = "C_Group", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "C_Group") + geom_point(aes(color=LCG))


t.test(items_avg_patient_ratio ~ C_Group,
       data = pr_main,
       alternative = "two.sided",
       paired = FALSE
)
# significant -> p = 0.03841

ms_anova_one_way <- aov(items_avg_patient_ratio ~ C_Group, data = pr_main)
summary(ms_anova_one_way)

ms_anova_one_way <- aov(items_avg_patient_ratio ~ LCG, data = pr_main)
summary(ms_anova_one_way)

# LCG appears to be hte more significant grouping of the avg items per patient

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
# Example Local Govt District
ggline(pr_main, x = "LGD1992NAME", y = "items_avg_patient_ratio", 
       add = c("mean_se") , # , "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "LGD") + geom_point(aes(color=LCG)) + rotate_x_text()

ms_anova_one_way <- aov(items_avg_patient_ratio ~ LGD1992NAME, data = pr_main)
summary(ms_anova_one_way)
# Significant - p value = 0.000284

ms_anova_one_way <- aov(items_avg_patient_ratio ~ SOA_CODE, data = pr_main)
summary(ms_anova_one_way)
# Significant - p = 0.000317
# Should be significant as SOAs map to LGD - interesting not identical

# How to use SOA and the genetics info?

# # futtering with data - can show how picking the area can deliver "results" (not too surprising)
# pr_main$temp_Group <- "g0"
# pr_main$temp_Group[# pr_main$LGD1992NAME == "FERMANAGH"
#                    # | 
#                      pr_main$LGD1992NAME == "OMAGH"
#                    | pr_main$LGD1992NAME == "DUNGANNON"
#                    | pr_main$LGD1992NAME == "NEWRY & MOURNE"
#                   # | pr_main$LGD1992NAME == "MAGHERAFELT"
#                    ] <- "g1"
# pr_main$temp_Group[pr_main$LGD1992NAME == "COLERAINE"
#                    | pr_main$LGD1992NAME == "BALLYMONEY"
#                    | pr_main$LGD1992NAME == "LIMAVADY"
#                    # | pr_main$LGD1992NAME == "BALLYMENA"
#                    | pr_main$LGD1992NAME == "LARNE"
#                    ] <- "g2"
#
# ggline(pr_main, x = "temp_Group", y = "items_avg_patient_ratio", 
#        add = c("mean_se") , # , "jitter"), 
#        ylab = "items_avg_patient_ratio", xlab = "C_Group") + geom_point(aes(color=LCG)) + rotate_x_text()
# 
# ms_anova_one_way <- aov(items_avg_patient_ratio ~ temp_Group, data = pr_main)
# summary(ms_anova_one_way)
# 
# junk <- subset(pr_main, pr_main$temp_Group == "g1" | pr_main$temp_Group == "g2" )
#   
# ggline(junk, x = "temp_Group", y = "items_avg_patient_ratio", 
#        add = c("mean_se") , # , "jitter"), 
#        ylab = "items_avg_patient_ratio", xlab = "C_Group") + geom_point(aes(color=LCG)) + rotate_x_text()
# 
# ms_anova_one_way <- aov(items_avg_patient_ratio ~ temp_Group, data = junk)
# summary(ms_anova_one_way)
# 
# t.test(items_avg_patient_ratio ~ temp_Group,
#        data = junk,
#        alternative = "two.sided",
#        paired = FALSE
# )

# ggline(junk, x = "LGD1992NAME", y = "items_avg_patient_ratio", 
#        add = c("mean_se") , # , "jitter"), 
#        ylab = "items_avg_patient_ratio", xlab = "C_Group") + geom_point(aes(color=temp_Group)) + rotate_x_text()

############ end futter

# Next level, to check on differences in Chapter patterns across areas

colSums(is.na(pr_main))
# remove blank/mainly blank columns
pr_main$WARD1992.2.[pr_main$WARD1992.2. == ""] <- NA

is.na(pr_main) <- pr_main==''
droplevels(pr_main)
pr_main <- select(pr_main , !c(WARD1992.2. ))


# Add location data to the mix
#pr_nums <- pr_main[numeric_variable_list]
pr_nums <- pr_main[c("items_avg_patient_ratio", "eastings", "northings" )]
pr_nums

km.res <- eclust(pr_nums, "kmeans", k = 2, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())


k_val <- fviz_nbclust(pr_nums, FUNcluster = kmeans)
k_val

k_val <- fviz_nbclust(pr_nums, kmeans, method = "silhouette")
k_val
# indicates 4 possible clusters for k-means with easting/northing
# 2 and 9 also look interesting?

set.seed(25020)
junk <- kmeans(pr_nums, 2, nstart = 25)
junk

junk <- kmeans(pr_nums, 4, nstart = 25)
junk

kmeans(pr_nums, 8, nstart = 25)

# Run through a few k sizes to see what clusters look like
km.res <- eclust(pr_nums, "kmeans", k = 9, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm") # , ggtheme = theme_minimal())
# 5 and 8 look interesting

km.res <- eclust(pr_nums, "kmeans", k = 2, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm") # , ggtheme = theme_minimal())

km.res <- eclust(pr_nums, "kmeans", k = 4, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm") # , ggtheme = theme_minimal())


# fviz_cluster(object = km.res, data = pr_nums, geom = "point",
#              choose.vars = c("items_avg_patient_ratio", "Practice"), stand = FALSE, 
#              ellipse.type = "norm") + theme_bw()

dd <- cbind(pr_main, cluster = km.res$cluster)
head(dd)

# check correlation between original and calculated clusters
dd$C_Group_num <- as.numeric((dd$C_Group))
cor(dd$C_Group_num,dd$cluster)


dd$SOA_CODE_num <- as.numeric((dd$SOA_CODE))
cor(dd$SOA_CODE_num,dd$cluster)

# Not much to gain from correllation?



####### other clustering, hierarchy, dbscan?

pr_nums

res.dist <- get_dist(pr_nums, stand = TRUE, method = "pearson")

# install.packages("NbClust")

library("NbClust")

pr_nums <- pr_main[c("items_avg_patient_ratio" )]
pr_nums


# This is showing that for pr_nums of 3 variables clusting could be #clusters = 10


library(fpc)

# install.packages("clValid")
library(clValid)

# Compute clValid
clmethods <- c("kmeans","pam")


intern <- clValid(pr_nums, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)


junk <- scale(pr_nums)

intern <- clValid(junk, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
 
plot(intern)


# The master list of practrices to work with based on previously selected outlier removal
datapath <- "processed_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

master_list_file <- "master_list.csv"
master_list_file <- paste(datapath, master_list_file, sep = "/")
master_list_file

# master_list <- unique(select(pr_main, c("Practice")))
# master_list
# write.csv(file=master_list_file, x=master_list, quote=TRUE, row.names = FALSE)

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
                                               "8" = "MalD ImmSup",
                                               "9" = "Blod Nutrt",
                                               "10" = "Musco Skel",
                                               "11" = "Eye",
                                               "12" = "ENT",
                                               "13" = "Skin",
                                               "14" = "Imm Vacc",
                                               "15" = "Anaest",
                                               "18" = "Prep Diag",
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

# Need to calculate the chapter items/patient ratio

chk <- select(pr_main, c(Practice, Registered_Patients) )
chk <- merge(chapters_in, chk, by = "Practice")
chk$chapter_avg_patient_ratio <- chk$PC_Month_Avg / chk$Registered_Patients
chk


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

pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
pca_chk

summary(pca_chk)

pca_res <- pca_chk

pca_res$x[1:5,1:3]

# var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
# 
# pca_res$x %>% 
#   as.data.frame %>%
#   ggplot(aes(x=PC1,y=PC2)) + geom_point(size=4) +
#   theme_bw(base_size=32) + 
#   labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
#        y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
#   theme(legend.position="top") 
# # +   geom_point(aes(color=C_Group_num),size=4)

plot1 <- fviz_eig(pca_chk, addlabels = TRUE, ylim = c(0, 50))
plot1

pr_var = ( pca_chk$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot to see how component choice covers the cumulatively
plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" )

# 10 components to cover 90% of the data?


###########################################
###########################################
###########################################


##################################

# colour put by groups
fviz_pca_ind(pca, axes = c(1,2), colvar = "contrib",
             geom.ind = "point", col.ind = pr_main$C_Group , #  LGD1992NAME  ,
#             palette = c("red", "green"),
             addEllipses = TRUE, 
             legend.totle = "Vote")


biplot <- fviz_pca_biplot(pca, 
                          col.ind =  pr_main$C_Group ,
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


##################################

### How to sample the geo area?
# Grab the SOA atlas data, join with the practices info, seperate and process

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

############################## EOF


