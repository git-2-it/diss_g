# diss01
# mostly a copy of datacheck02 except with the nearest_docs datasets

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

# This code to replace the previously outliers pickup
nearest_docs_file <- "nearest_docs*.csv"
nearest_docs_file <- paste(datapath, nearest_docs_file, sep = "/")
nearest_docs_file

# doctor/practise data
# docs_in <- read.csv(paste(datapath, "cleaned_practice_data.csv", sep = "/"))
pr_main_in <- read.csv(nearest_docs_file)

head(pr_main_in)

# Examine structure of the file
str(pr_main_in)

pr_main  <- pr_main_in


# --------------------------------------------------


ms_anova_one_way <- aov(items_avg_patient_ratio ~ SOA_CODE, data = pr_main)
summary(ms_anova_one_way)
# Not significant - p = 0.141
# Next level, to check on differences in Chapter patterns across areas

colSums(is.na(pr_main))
# remove blank/mainly blank columns
# pr_main$WARD1992.2.[pr_main$WARD1992.2. == ""] <- NA

is.na(pr_main) <- pr_main==''
droplevels(pr_main)
# pr_main <- select(pr_main , !c(WARD1992.2. ))

# numeric_variable_list <- sapply(pr_main, is.numeric)
# numeric_variable_list
# str(numeric_variable_list)
# assign("numeric_variable_list", NULL, envir = .GlobalEnv)
# 
# # Create a subset of the data
# numerical_data <- pr_main[numeric_variable_list]
# colnames(numerical_data)
# 
# # Remove other unwanted fields 
# numeric_variable_list["latitude"] <- FALSE
# numeric_variable_list["longitude"] <- FALSE
# numeric_variable_list["eastings"] <- FALSE
# numeric_variable_list["northings"] <- FALSE
# 
# numeric_variable_list["Registered_Patients"] <- FALSE
# numeric_variable_list["year"] <- FALSE
# numeric_variable_list["Total_Items_P"] <- FALSE
# numeric_variable_list["Number.SAs"] <- FALSE
# numeric_variable_list["SQ.Km"] <- FALSE
# 
# numeric_variable_list["items_avg_patient_ratio"] <- TRUE
# 
# str(numeric_variable_list)
# numeric_variable_list

# How to cluster based on geo co-ords and a 3rd variable?
# Add location data to the mix
#pr_nums <- pr_main[numeric_variable_list]
pr_nums <- pr_main[c("items_avg_patient_ratio", "Practice", "eastings", "northings" )]
pr_nums

pr_nums <- pr_main[c("items_avg_patient_ratio", "eastings", "northings" )]
pr_nums


# km.res <- eclust(pr_nums, "kmeans", k = 2, nstart = 25, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())


k_val <- fviz_nbclust(pr_nums, FUNcluster = kmeans)
k_val <- fviz_nbclust(pr_nums, kmeans, method = "silhouette")
k_val
# indicates 3 possible clusters for k-means
# 9 and 4 also look interesting?

set.seed(25020)
junk <- kmeans(pr_nums, 3, nstart = 25)
junk

junk <- kmeans(pr_nums, 4, nstart = 25)
junk

kmeans(pr_nums, 8, nstart = 25)

# Run through a few k sizes to see what clusters look like
km.res <- eclust(pr_nums, "kmeans", k = 9, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm") # , ggtheme = theme_minimal())
# 5 and 8 look interesting

km.res <- eclust(pr_nums, "kmeans", k = 4, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm") # , ggtheme = theme_minimal())

km.res <- eclust(pr_nums, "kmeans", k = 3, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm") # , ggtheme = theme_minimal())


dd <- cbind(pr_main, cluster = km.res$cluster)
head(dd)

# check correlation between original and calculated clusters
dd$C_Group_num <- as.numeric((dd$C_Group))
cor(dd$C_Group_num,dd$cluster)


dd$SOA_CODE_num <- as.numeric((dd$SOA_CODE))
cor(dd$SOA_CODE_num,dd$cluster)

# Not much to gain from correllation?

# Try another clustering number search
gap_stat <- clusGap(pr_nums, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

# To see optinal number of clusters
fviz_gap_stat(gap_stat)

# again indicates 3

set.seed(25020)
junk <- kmeans(pr_nums, 4, nstart = 25)
junk

# km.res <- eclust(pr_nums, "kmeans", k = 4, nstart = 25, graph = FALSE)
# 
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())
# 
# km.res <- eclust(pr_nums, "kmeans", k = 2, nstart = 25, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())
# 
# km.res <- eclust(pr_nums, "kmeans", k = 9, nstart = 25, graph = FALSE)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm" , ggtheme = theme_minimal())
# 

stats::prcomp(pr_nums)

# Results look same as the other cluster estimate mechanism
# Is there any point to this considering geo nature of the co-ords


# The master list of practrices to work with based on previously selected outlier removal
# Master list now is this list of practices chosen for cluster proximity

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

# chapters_in <- subset(chapters_in, BNF_Chapter == "18")

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



# ggplot(chapters_in, aes(x = as.factor(BNF_Chapter), y = PC_Month_Avg, fill = BNF_Chapter) ) + 
#   geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
#   ggtitle("Items dispensed") + 
#   labs(title="Avg items dispensed per Chapter",
#        x ="BNF Chapter", y = "# Items")   + theme(legend.position="none")


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

ggplot(chk, aes( y=chapter_avg_patient_ratio)) + 
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2,
               fill='#A4A4A4', color="black"
  )  + labs(title="Avg Items / Patient / Chapter", y = "Avg Items per Patient per Chapter") +  
  facet_wrap(~ Chapter_lab, scale="free")



str(chk)
# chk <- chk[, c(1,4,8)  ]
chk <- chk[, c(1,7,9)  ]
# chk <- chk[, c(1,4,9)  ]
chk

# rename columns
names(chk)[names(chk) == 'Chapter_lab'] <- 'BNF_Chapter'

chk <- chk[!duplicated(chk[c("Practice", "BNF_Chapter", "chapter_avg_patient_ratio")]),]

chk <- pivot_wider(chk, names_from = BNF_Chapter, values_from = chapter_avg_patient_ratio)
chk

# Now have chk being rows of practices with columns of chapter data by chapter item avg value

colSums(is.na(chk))
# Chapter 19 - Other - set non existing to zero
chk[is.na(chk)] <- 0
str(chk)


pca_chk <- prcomp(chk[ , 2:20], scale = TRUE)
pca_chk
#pca_chk <- prcomp(chk, scale = TRUE)

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

pca_forvariables <- get_pca_var(pca_chk)
pca_forvariables

# Correlation heatmap
corrplot(pca_forvariables$cos2, is.corr = FALSE)

# Cheating conveinience copy ...
pca <-pca_chk

fviz_pca_var(pca, col.var="contrib",
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 20)
# Values cross check with the heatmap above

head(pca_forvariables$cos2, 20)

# Still low contribution to conponents
# Gastro remains the biggest differentiator
# Cardio
# CNS
# Musculo skel
# Blood, Nutrition


##################################

pr_main$C_Group <- as.character(pr_main$C_Group)

# colour put by groups
fviz_pca_ind(pca, axes = c(1,2), colvar = "contrib",
             geom.ind = "point", # col.ind = pr_main$C_Group , #  LGD1992NAME  ,
#             palette = c("red", "green"),
             addEllipses = TRUE
)
# ,             legend.title = "Vote")


biplot <- fviz_pca_biplot(pca, 
#                          col.ind =  pr_main$C_Group ,
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

# How to read what this is showing?

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
                       , habillage = "none"
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


###########
# Ranking

junk <- junk[
  with(chk, order (Practice) )
       ]
junk




# Remove Connacht, Dublin, merge NI_II and NI_III
pr_main$Cluster_Grp <- "2"
pr_main$Cluster_Grp[pr_main$Structure == "NI_III"] <- "1"
pr_main$Cluster_Grp[pr_main$Structure == "NI_II"] <- "1"

t.test(items_avg_patient_ratio ~ Cluster_Grp ,
       data = pr_main,
       alternative = "two.sided",
       paired = FALSE
)

# # Split into 2 coherts
# 
# pr_main1 <- pr_main$Cluster_Grp[pr_main$Cluster_Grp == "1"]
# pr_main1
# pr_main2 <- pr_main$Cluster_Grp[pr_main$Cluster_Grp == "2"]
# pr_main2

# Wilcox tst better for smaller sample sizes
wil_res <- wilcox.test(items_avg_patient_ratio ~ Cluster_Grp, data = pr_main)
wil_res

# Re move 526 - and check on how it got there, dual nationality? example of how the 
# data available at this level isn't accurate enough.
pr_main <- subset(pr_main, Practice != 526)

wil_res <- wilcox.test(items_avg_patient_ratio ~ Cluster_Grp, data = pr_main)
wil_res


# Get non-wide data again
# Join chapters data with practices info
non_wide <- merge(chapters_in, pr_main_in, by = "Practice")
str(non_wide)

# Need to calculate the chapter items/patient ratio

non_wide <- data.frame(pr_main)
non_wide <- merge(chapters_in, non_wide, by = "Practice")
non_wide$chapter_avg_patient_ratio <- non_wide$PC_Month_Avg / non_wide$Registered_Patients
non_wide

# Sort by practice and chapter items value
pr_sorted <- non_wide[order(non_wide$Cluster_Grp,
                            non_wide$Practice, 
                            non_wide$chapter_avg_patient_ratio, 
                            non_wide$BNF_Chapter
                          ), ]
pr_sorted

pr_sorted <- select(pr_sorted, c(chapter_avg_patient_ratio, BNF_Chapter, Practice, Cluster_Grp))
# pr_sorted <- select(pr_sorted, c(chapter_avg_patient_ratio, BNF_Chapter, Practice))
pr_sorted$Practice.f = factor(pr_sorted$Practice,
                              levels=unique(pr_sorted$Practice))
pr_sorted$ratio.f = factor(pr_sorted$chapter_avg_patient_ratio,
                           ordered=TRUE)
pr_sorted$BNF_Chapter.f <- factor(pr_sorted$BNF_Chapter)
pr_sorted$Cluster_Grp.f <- factor(pr_sorted$Cluster_Grp)

# friedman.test(chapter_avg_patient_ratio ~ BNF_Chapter | Practice, data = pr_sorted)

friedman.test(chapter_avg_patient_ratio ~ Practice.f | BNF_Chapter.f  , data = pr_sorted)

colSums(is.na(pr_sorted))

tally(pr_sorted)

junk <- pr_sorted %>% group_by(Practice) %>% count(Practice)
junk

# 578 and 583 are missing values
# Need to insert as 0
junk <-select(pr_sorted, )

junk <-subset(pr_sorted , pr_sorted$Practice == 578) 
junk
junk <-subset(pr_sorted , pr_sorted$Practice == 583) 
junk

# chapter 19 missing in both cases
# Insert 0 value in both cases
junk <- subset(pr_sorted , pr_sorted$Practice == 583 & BNF_Chapter == 1) 
junk$BNF_Chapter <- 19
junk$chapter_avg_patient_ratio <- 0
junk

pr_sorted <- rbind(pr_sorted, junk)

junk <-subset(pr_sorted , pr_sorted$Practice == 578 & BNF_Chapter == 1) 
junk$BNF_Chapter <- 19
junk$chapter_avg_patient_ratio <- 0
junk

pr_sorted <- rbind(pr_sorted, junk)

# Make sure to do refactorisation of dataframe
pr_sorted$Practice.f = factor(pr_sorted$Practice,
                              levels=unique(pr_sorted$Practice))
pr_sorted$ratio.f = factor(pr_sorted$chapter_avg_patient_ratio,
                           ordered=TRUE)
pr_sorted$BNF_Chapter.f <- factor(pr_sorted$BNF_Chapter)
pr_sorted$Cluster_Grp.f <- factor(pr_sorted$Cluster_Grp)

pr_sorted$Cluster_Grp.f = factor(pr_sorted$Cluster_Grp,
                              levels=unique(pr_sorted$Cluster_Grp))


# Recheck tally, should be 800, 40 x 20
tally(pr_sorted)

junk <- pr_sorted %>% group_by(Practice) %>% count(Practice)
junk

# re-run friedman test

junk <- friedman.test(chapter_avg_patient_ratio ~ Practice.f | BNF_Chapter.f  , data = pr_sorted)
junk

junk <- friedman.test(chapter_avg_patient_ratio ~ BNF_Chapter.f | Practice.f  , data = pr_sorted)
junk

friedman_effsize(chapter_avg_patient_ratio ~ BNF_Chapter.f | Practice.f  , data = pr_sorted)
friedman_effsize(chapter_avg_patient_ratio ~ Practice.f | BNF_Chapter.f  , data = pr_sorted)


junk <- friedman.test(chapter_avg_patient_ratio ~ Practice.f | Cluster_Grp.f  , 
                      data = pr_sorted)
junk


junk <- table(pr_sorted$Practice.f, pr_sorted$Cluster_Grp)
summary(junk)

junk <- friedman.test(chapter_avg_patient_ratio ~ Cluster_Grp.f | BNF_Chapter.f  , data = pr_sorted)
junk

junk <- friedman.test(chapter_avg_patient_ratio ~ Cluster_Grp.f | Practice.f  , data = pr_sorted)
junk


summary(junk)

junk <- pr_sorted %>% group_by(Practice) %>% count(Cluster_Grp)
junk

# #############################
# # Get more data, Register of Prevalence - removed to check06
# Looks unuseable all the same


# A number of practices missing - 517, 578, 583 - most likely closed, check

chk  <- subset(docs_in, docs_in$PracNo == 517)
chk
# 517 - closed 2018
# Practice closed and majority of patients dispersed to Practice 495

chk  <- subset(docs_in, docs_in$PracNo == 578)
chk
# 578 - closed 2016
# Merged with 616 and 617 to become 616 April 2016

chk  <- subset(docs_in, docs_in$PracNo == 583)
chk
# 583 - closed 2018
# Merged with 581 and 585 on 1st April 2018 to become 585

chk  <- subset(docs_in, docs_in$PracNo == 495)
chk

chk  <- subset(docs_in, docs_in$PracNo == 616)
chk

chk  <- subset(docs_in, docs_in$PracNo == 585)
chk

# Ignore?
# Complicated to bring in WRT the time based nature of the data
# plus 616 is one of the "super-practices" of 15k patients

ggplot(pr_sorted,  aes(x=Cluster_Grp,y=chapter_avg_patient_ratio, fill=factor(Cluster_Grp))) +
  geom_boxplot() +
  labs(fill = "Cluster_Grp") +
  theme_bw(base_size = 16) + facet_wrap(~BNF_Chapter) +   geom_point(position=position_jitterdodge(),alpha=0.3) 


# Chapters 1,2, 3, 4, 
# 1- 10

chk <- subset(pr_sorted, pr_sorted$BNF_Chapter <= 10)
chk <- subset(chk, chk$BNF_Chapter != 7 & chk$BNF_Chapter != 8)


ggplot(chk,  aes(x=Cluster_Grp,y=chapter_avg_patient_ratio, fill=factor(Cluster_Grp))) +
  geom_boxplot() +
  labs(fill = "Cluster_Grp") +
  theme_bw(base_size = 16) + facet_wrap(~BNF_Chapter) +   geom_point(position=position_jitterdodge(),alpha=0.3) 

chk <- subset(chk, chk$BNF_Chapter != 5 & chk$BNF_Chapter != 10)

ggplot(chk,  aes(x=Cluster_Grp,y=chapter_avg_patient_ratio, fill=factor(Cluster_Grp))) +
  geom_boxplot() +
  labs(fill = "Cluster_Grp") +
  theme_bw(base_size = 16) + facet_wrap(~BNF_Chapter) +   geom_point(position=position_jitterdodge(),alpha=0.3) 

# t-test by Chapter

indiv <- subset(chk, chk$BNF_Chapter == 1)
t.test(chapter_avg_patient_ratio ~ Cluster_Grp ,
       data = indiv,
       alternative = "two.sided",
       paired = FALSE
)
# p is 0.2162 - not significant
wilcox.test(chapter_avg_patient_ratio ~ Cluster_Grp, data = indiv)
# p = 0.3563

indiv <- subset(chk, chk$BNF_Chapter == 2)
t.test(chapter_avg_patient_ratio ~ Cluster_Grp ,
       data = indiv,
       alternative = "two.sided",
       paired = FALSE
)
# not significant -> p = 0.6502
wilcox.test(chapter_avg_patient_ratio ~ Cluster_Grp, data = indiv)
# p = 0.4812

indiv <- subset(chk, chk$BNF_Chapter == 3)
t.test(chapter_avg_patient_ratio ~ Cluster_Grp ,
       data = indiv,
       alternative = "two.sided",
       paired = FALSE
)
# not significant, p= 0.8971
wilcox.test(chapter_avg_patient_ratio ~ Cluster_Grp, data = indiv)
# p 0.5882

indiv <- subset(chk, chk$BNF_Chapter == 4)
t.test(chapter_avg_patient_ratio ~ Cluster_Grp ,
       data = indiv,
       alternative = "two.sided",
       paired = FALSE
)
# p = 0.2592, not significant
wilcox.test(chapter_avg_patient_ratio ~ Cluster_Grp, data = indiv)
# p = 0.2772

indiv <- subset(chk, chk$BNF_Chapter == 6)
t.test(chapter_avg_patient_ratio ~ Cluster_Grp ,
       data = indiv,
       alternative = "two.sided",
       paired = FALSE
)
# p = 0.9615, not significant
wilcox.test(chapter_avg_patient_ratio ~ Cluster_Grp, data = indiv)
# p = 0.9535

################### run against others

indiv <- subset(pr_sorted, pr_sorted$BNF_Chapter == 23)
t.test(chapter_avg_patient_ratio ~ Cluster_Grp ,
       data = indiv,
       alternative = "two.sided",
       paired = FALSE
)
wilcox.test(chapter_avg_patient_ratio ~ Cluster_Grp, data = indiv)

# 12 - p = 0.1694 and 0.2108
# 21 - p = 0.0255 and 0.03918
# 23 - p = 0.07213 and 0.07066


# 21 is Appliances - covers a wide range of products


# chapters_in <- chapters_in %>% mutate(Chapter_lab=recode(BNF_Chapter, 
#                                                          "1" = "Gastro",
#                                                          "2" = "Cardio", 
#                                                          "3" = "Resp", 
#                                                          "4" = "CNS", 
#                                                          "5" = "Infect",
#                                                          "6" = "Endo",
#                                                          "7" = "OGU",
#                                                          "8" = "MalD ImmSup",
#                                                          "9" = "Blod Nutrt",
#                                                          "10" = "Musco Skel",
#                                                          "11" = "Eye",
#                                                          "12" = "ENT",
#                                                          "13" = "Skin",
#                                                          "14" = "Imm Vacc",
#                                                          "15" = "Anaest",
#                                                          "18" = "Prep Diag",
#                                                          "19" = "Other",
#                                                          "20" = "Dressings",
#                                                          "21" = "Appliances",
#                                                          "22" = "Incont",
#                                                          "23" = "Stoma"
# ))


############## EOF
