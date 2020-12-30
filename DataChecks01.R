# diss01
# run 4
# The practice and prescription data are joined using practice id 
# This to check the dataset for outliers etc prior to any analyusis.

library(dplyr)

# --------------------------------------------------
# Get practice data
# --------------------------------------------------

main_path <- "~/courses/dissertation/data"

datapath <- "portal_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

# doctor/practise data
docs_in <- read.csv(paste(datapath, "geo_cleaned_practice_data.csv", sep = "/"))

head(docs_in)

# Examine structure of the file
str(docs_in)


# --------------------------------------------------
# Read prescription items data
# --------------------------------------------------

# Open data file to review contents
p_data <- read.csv(paste(datapath,"grep " , sep = "/"))
# p_data <- read.csv("data/summary_info.csv")

head(p_data)

# look at the structure of the file
str(p_data)

summary(p_data)
head(p_data)

# ------------------------------------------------------
# Check the practice list for missing data values in either dataset


p_data$Practice <- as.factor(p_data$Practice)
docs_in$PracNo <- as.factor(docs_in$PracNo)

str(p_data)
str(docs_in)

colSums(is.na(p_data))


# Join practice and items data
# pr_main is main practice data
pr_main <- subset(docs_in, 
                  select = c(PracNo, Postcode, LCG, Registered_Patients, Town, County, C_Group, year))

str(pr_main)
colnames(pr_main)[colnames(pr_main) == "PracNo"] <-"Practice"
colnames(pr_main)

# Need to clear up the commas in spome reg patients figures
pr_main$Registered_Patients <- as.numeric(gsub(",", "", 
                                 as.character(levels(pr_main$Registered_Patients))[pr_main$Registered_Patients], 
                                 fixed = TRUE))
str(pr_main)

pr_main2 <- subset(p_data, select = c("Practice", "Total_Items_P", "P_Month_Avg") )
pr_main2 <-unique(pr_main2)

# Left join to get practice info for the prescription data
pr_main  <- left_join(pr_main, pr_main2, 
                      by = c("Practice" = "Practice")
)

str(pr_main)

# Check on NA
colSums(is.na(pr_main))

# Plot some visuals to help understand the shape of the data set

library("viridis")
# hold the param settings
# Store default plot settings
opar <- par()

par(mfrow = c(1, 2))

# Plot items prescribed vs patients
scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$Total_Items_P,
               main = "Practice Total Prescribed Items vs Patients",
               xlab = "# Patients",
               ylab = "# Items",
)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Practice Avg Prescribed Items vs Patients",
               xlab = "# Patients",
               ylab = "#Items Avg",
)

# Check on correlation between patient numbers and total items and monthly average
# Would expect strong link 
cor(pr_main$Registered_Patients, pr_main$Total_Items_P)
cor(pr_main$Registered_Patients, pr_main$P_Month_Avg)

# Plot items prescribed vs patients
scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$Total_Items_P,
               main = "Patient vs Total Prescribed Items",
               xlab = "Patients",
               ylab = "# Items",
)


text(pr_main$Registered_Patients, pr_main$Total_Items_P,
     labels= pr_main$Practice,
     cex=0.9, font=2)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Patient vs Avg Prescribed Item numbers",
               xlab = "Patients",
               ylab = "# Items",
)
text(pr_main$Registered_Patients, pr_main$P_Month_Avg,
     labels= pr_main$Practice,
     cex=0.9, font=2)


# Prac no 157
chk  <- subset(docs_in, docs_in$PracNo == 157)
chk

# Uni Health centre - remove
pr_main <- subset(pr_main, Practice != 157)

par(mfrow = c(1, 1))

# Replot, colour coding
scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Patient vs Prescribed Item numbers",
               sub = "By LCG",
               xlab = "Patients",
               ylab = "# Items",
               col = pr_main$LCG,
)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Patient vs Prescription numbers",
               sub = "By Town",
               xlab = "Patients",
               ylab = "# Prescriptions",
               col = pr_main$Town
)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Patient vs Prescription numbers",
               sub = "By Year",
               xlab = "Patients",
               ylab = "# Prescriptions",
               col = pr_main$year
)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Patient vs Prescription numbers",
               sub = "By C_Group",
               xlab = "Patients",
               ylab = "# Prescriptions",
               col = pr_main$C_Group
)

# The scatter by year
scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$P_Month_Avg,
               main = "Patient vs Prescription numbers",
               sub = "By C_Group",
               xlab = "Patients",
               ylab = "# Prescriptions",
               col = pr_main$year
)

# Zoom on the low end, < 4000 patients
zoomer <- subset(pr_main, pr_main$Registered_Patients < 3000)
scatter.smooth(x = zoomer$Registered_Patients,
               y = zoomer$P_Month_Avg,
               main = "Patient vs Prescription numbers",
               sub = "By C_Group",
               xlab = "Patients",
               ylab = "# Prescriptions",
               col = zoomer$year
                , pch= 2
)


par(mfrow = c(1, 1))

# Confirmed no obvious patterns by town/LCG/C_Group
# Year interesting but maybe not conclusive 

#--------------------------------------
# Check for outliers
#--------------------------------------
library(ggplot2)

ggplot(pr_main, aes( y=Registered_Patients)) + 
      labs(title="# Registered Patients", y = "Registered Patients") +
      geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2 ,
               fill='#A4A4A4', color="black" 
               #fill = '#26AC82', color="black"
               )  


# A few larger than normal practices, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$Registered_Patients > 14000 
                      #                      select = c("PracNo", "Registered_Patients", "Town", "County")
))
chk
# 5 larger than average practices, based on town names its a mix of urban and rural practices
# the largest practice is PracNo 306 in Castlederg with over 15 000 patients, 12 times larger 
# than the smallest practice, PracNo 55 @ 1198 patients
# Larger practices becomming the new norm? (source)
# The data can't be removed, too valuable

ggplot(pr_main, aes( y=Total_Items_P)) + 
  labs(title="# Total Items Prescribed", y = "Total Items Prescribed") +
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2 ,
               fill='#A4A4A4', color="black" 
               #fill = '#26AC82', color="black"
)  


# A few larger than normal practices, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$Total_Items_P > 1750000, 
))
chk

# At least four 2 000 000 items, note and keep

# Check Avg items prescribed

ggplot(pr_main, aes( y=P_Month_Avg)) + 
  labs(title="Monthly Average of Items", y = "Average of Items") +
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2 ,
               fill='#A4A4A4', color="black" 
               #fill = '#26AC82', color="black"
  )  


# A few larger than normal practices, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$P_Month_Avg > 21000, 
))
chk

# Not the same list though several overlaps

# Create items / patient ratios for total and average items
pr_main$item_total_patient_ratio <- pr_main$Total_Items_P / pr_main$Registered_Patients
pr_main$items_avg_patient_ratio <- pr_main$P_Month_Avg / pr_main$Registered_Patients

# Check for outliers and view dist curves
par(mfrow = c(1, 2))

ggplot(pr_main, aes( y=item_total_patient_ratio)) + 
  labs(title="Total Items / Patient", y = "Total Items / Patient") +
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2 ,
               fill='#A4A4A4', color="black" 
               #fill = '#26AC82', color="black"
  )  

chk  <- unique(subset(pr_main, pr_main$item_total_patient_ratio > 210, 
))
chk

chk  <- unique(subset(pr_main, pr_main$item_total_patient_ratio > 200, 
))
chk

chk  <- unique(subset(pr_main, pr_main$item_total_patient_ratio < 65, 
))
chk


ggplot(pr_main, aes( y=items_avg_patient_ratio)) + 
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2,
               fill='#A4A4A4', color="black"
               )  + labs(title="Avg Items / Patient", y = "Avg Items per Patient")


chk  <- unique(subset(pr_main, pr_main$items_avg_patient_ratio > 2.45, ))
chk

chk  <- unique(subset(pr_main, pr_main$items_avg_patient_ratio > 2.5, ))
chk

chk  <- unique(subset(pr_main, pr_main$items_avg_patient_ratio <= 0.8, ))
chk

# View all 4 in one pane
par(mfrow = c(2, 2))

# show with dist curves
boxplot(x = pr_main$item_total_patient_ratio,
        main = "Total Items / Patient",
        sub = paste("outlier rows: ",
                    boxplot.stats(x = pr_main$item_total_patient_ratio)$out),
        ylab = "# Items",
        col = "steelblue",
        border = "black"
)

# # ######################################
# colSums(is.na(pr_main))
# pr_main <- pr_main[complete.cases(pr_main), ]
# # ######################################

# Outliers on both end of the scale, look at the dist curve
dense_p <- density(pr_main$item_total_patient_ratio)

plot(dense_p,
     main = "Kernel Density of Total Items / Patient"
) 
polygon(dense_p, col="red", border="red")

boxplot(x = pr_main$items_avg_patient_ratio,
        main = "Avg Items / Patient",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$items_avg_patient_ratio)$out),
        ylab = "# Items",
        col = "steelblue",
        border = "black"
)

# Outliers on both end of the scale, look at the dist curve
dense_p <- density(pr_main$items_avg_patient_ratio)

plot(dense_p,
     main = "Kernel Density of Avg Items / Patient"
) 
polygon(dense_p, col="red", border="red")


chk  <- unique(subset(pr_main, pr_main$item_total_patient_ratio > 200, ))
chk

chk  <- unique(subset(pr_main, pr_main$item_total_patient_ratio < 60, ))
chk


chk  <- unique(subset(pr_main, pr_main$items_avg_patient_ratio > 2.5, ))
chk

chk  <- unique(subset(pr_main, pr_main$items_avg_patient_ratio < 0.8, ))
chk

# why???
subset(docs_in, docs_in$PracNo == 267)
subset(pr_main, pr_main$Practice == 201)

# items_avg_patient_ratio < 0.8
# Why the outliers?
chk  <- unique(subset(pr_main, pr_main$Practice == "1", ))
chk
# ==> 1 probable outlier
junk <- subset(docs_in,  docs_in$PracNo == "663")
junk
junk <- unique(subset(pr_main, pr_main$Practice == "663", ))
junk
junk <- subset(docs_in,  docs_in$PracNo == "1")
junk
junk <- subset(docs_in,  docs_in$PracNo == "598")
junk


junk <- unique(subset(pr_main, pr_main$Practice == "75", ))
junk

# items_avg_patient_ratio > 2.45
# Why the outliers?
# prac 663 on Omagh, rest all Belfast
chk  <- unique(subset(pr_main, pr_main$Town == "OMAGH", ))
chk
# ==> 663 probable outlier

junk <- subset(docs_in,  docs_in$PracNo == "663")
junk
junk <- subset(docs_in,  docs_in$PracNo == "1")
junk

chk  <- unique(subset(pr_main, pr_main$Town == "BELFAST" , ))
# chk  <- unique(subset(pr_main, pr_main$Town == "BELFAST" & pr_main$County == "ANTRIM", ))
chk

chk <- unique(subset(chk, chk$items_avg_patient_ratio > 2.5 | chk$items_avg_patient_ratio <= 0.8, ))
chk

# check the removal criteria
remove_practices <- unique(subset(pr_main, pr_main$items_avg_patient_ratio > 2.5 | pr_main$items_avg_patient_ratio <= 0.8, ))
remove_practices

pr_main <- pr_main[!pr_main$Practice %in% remove_practices$Practice, ]
pr_main <- droplevels(pr_main)

# Recheck the boxplot/dist curve

# bug stopping par reset?
par(mfrow = c(1, 2))

boxplot(x = pr_main$items_avg_patient_ratio,
        main = "Avg Items / Patient",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$items_avg_patient_ratio)$out),
        ylab = "# Items",
        col = "steelblue",
        border = "black"
)

# Outliers on both end of the scale, look at the dist curve
dense_p <- density(pr_main$items_avg_patient_ratio)

plot(dense_p,
     main = "Kernel Density of Avg Items / Patient"
) 
polygon(dense_p, col="red", border="red")


##################################################


# First run
#--------------------

# Is normal distribution?
e1071::skewness(pr_main$items_avg_patient_ratio)
# Skew = 0.22387
# Skew = 0.473
# Data is skewed to the right, slighly noticable in the dist curve
# If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.
# Within acceptable limits - proceed

e1071::kurtosis(pr_main$items_avg_patient_ratio)
# kurtosis = 0.19025
# kurtosis = 0.756
# Within acceptable limits - proceed

chk <-pr_main[ ,-7]
# subset(pr_main, select != c( C_Group))

pr_main

summary(chk)

numeric_variable_list <- sapply(chk, is.numeric)
numeric_variable_list["year"] <- FALSE
numeric_variable_list["item_total_patient_ratio"] <- FALSE
numeric_variable_list["Total_Items_P"] <- FALSE
numeric_variable_list
numerical_data <- chk[numeric_variable_list]
colnames(numerical_data)

# build summary table
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary


# Categories
#---------------------------------------------------------------------------

# Use to categorise practices by size using patient number quartiles
# Categorisation by quartiles

# Practice size, patients
patient_1stq  <- numerical_summary["1st Qu.",'Registered_Patients']
patient_mean  <- numerical_summary["Mean",'Registered_Patients']
patient_3rdq  <- numerical_summary["3rd Qu.",'Registered_Patients']

summary(pr_main)

#---------------------------------------------------------------------------
# Categorise the practices data 
#---------------------------------------------------------------------------

numeric_variable_list <- sapply(pr_main, is.numeric)
numeric_variable_list["year"] <- FALSE
numeric_variable_list["item_total_patient_ratio"] <- FALSE
numeric_variable_list["Total_Items_P"] <- FALSE
numeric_variable_list

# create a subset of the numerical data
numerical_data <- pr_main[numeric_variable_list]
colnames(numerical_data)

# build summary table
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary


# Categories
#---------------------------------------------------------------------------

# Use to categorise practices by size using patient number quartiles
# Categorisation by quartiles

# Practice size, patients
patient_1stq  <- numerical_summary["1st Qu.",'Registered_Patients']
patient_mean  <- numerical_summary["Mean",'Registered_Patients']
patient_3rdq  <- numerical_summary["3rd Qu.",'Registered_Patients']

pr_main <- pr_main %>% mutate(practice_size = 
                                case_when(Registered_Patients <= patient_1stq  ~ '1',
                                          Registered_Patients > patient_1stq  & Registered_Patients <= patient_mean ~ '2',
                                          Registered_Patients > patient_mean  & Registered_Patients <= patient_3rdq ~ '3',
                                          Registered_Patients > patient_3rdq ~ '4'
                                ))

# Items avg patient
items_avg_patient_ratio_1stq  <- numerical_summary["1st Qu.",'items_avg_patient_ratio']
items_avg_patient_ratio_mean  <- numerical_summary["Mean",'items_avg_patient_ratio']
items_avg_patient_ratio_3rdq  <- numerical_summary["3rd Qu.",'items_avg_patient_ratio']

pr_main <- pr_main %>% mutate(items_total_ratio = 
                                case_when(items_avg_patient_ratio <= items_avg_patient_ratio_1stq  ~ '1',
                                          items_avg_patient_ratio > items_avg_patient_ratio_1stq  & items_avg_patient_ratio <= items_avg_patient_ratio_mean ~ '2',
                                          items_avg_patient_ratio > items_avg_patient_ratio_mean  & items_avg_patient_ratio <= items_avg_patient_ratio_3rdq ~ '3',
                                          items_avg_patient_ratio > items_avg_patient_ratio_3rdq ~ '4'
                                ))


str(pr_main)

# reorder based on avg item patient ratios, this being a variable of interest
plot_data <- pr_main
plot_data$Practice <- factor(plot_data$Practice, 
                             levels = plot_data$Practice[order(plot_data$items_avg_patient_ratio)])

library("ggplot2") 

ggplot(plot_data, aes(x = Practice, y = items_avg_patient_ratio, fill=LCG) ) + 
  geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
  ggtitle("Avg Items per patient per month") + 
  labs(title="Avg Items per patient \nCoded by LCG",
       x ="Practice No", y = "Avg Items")


ggplot(plot_data, aes(x = Practice, y = items_avg_patient_ratio, fill=C_Group) ) + 
  geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
  ggtitle("Avg Items per patient per month") + 
  labs(title="Avg Items per patient \nCoded by Cluster",
       x ="Practice No", y = "Avg Items")


# Majority of data appears spread over the LCGs, with exceptions at the higher end of the scale
# Note overall curve shape, matches tailed dist curve - kurtosis

par(mfrow = c(1, 1))

# Small sanity checks
# Drop unused factors
pr_main <- droplevels(pr_main)
# re=check for NAs
colSums(is.na(pr_main))

# Reset practice id factor
pr_main$Practice <- as.factor(pr_main$Practice)

str(pr_main)

par(mfrow = c(1, 3))

hist(pr_main$Registered_Patients,  main = "Registered Patients", xlab = "", freq = FALSE)
curve(
  dnorm(x, mean=mean(pr_main$Registered_Patients), sd= sd(pr_main$Registered_Patients) )
  , add=TRUE,
  col = "blue",
  lwd = "2"
)

hist(pr_main$P_Month_Avg ,  main = "Monthly Items per Patient", xlab = "", freq = FALSE)
curve(
  dnorm(x, mean=mean(pr_main$P_Month_Avg), sd= sd(pr_main$P_Month_Avg) )
  , add=TRUE,
  col = "blue",
  lwd = "2"
)

hist(pr_main$items_avg_patient_ratio ,  main = "Avg Items per Patient per Month", xlab = "", freq = FALSE)
curve(
  dnorm(x, mean=mean(pr_main$items_avg_patient_ratio), sd= sd(pr_main$items_avg_patient_ratio) )
  , add=TRUE,
  col = "blue",
  lwd = "2"
)

# All stats in a normal dist, which is expected - check correlation
# Pearson method as all are continuous variables, all appear acceptably normally distributed

#install.packages("psych")
library(psych)

par(mfrow = c(1, 1))

colnames(pr_main)
pr_main$practice_size <- as.numeric(pr_main$practice_size)


pair_variables <- c("Registered_Patients",
                    "items_avg_patient_ratio",
                    "P_Month_Avg"
)

pairs.panels(pr_main[pair_variables], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of patient numbers and item prescription figures"
)

# Weak, inverse relationship, between registered patients and average items per patient per month, -0.19
# Stronmg relationship between # registered patients and pratice items per month - not surprising
# Weak relationship between average items per patient per month and either registered patients or 
# overall practice average


# All location data is independant data, rest dependant
# Location data to use is LCG , Postcode (posttown?) - later the grouping
# Concentrate on items per patient per month - more reliable over any given time period
# Test for a continuous variable (items avg) against anominal value (LCG)
# items avg normally dist
# data is normally distributed, spread across more than one group?
# ANOVA test
# one or two way?
# only one group, so one way test

# Null hypothesis: the means of the different groups are the same - ie cannot predict
# Alternative hypothesis: At least one sample mean is not equal to the others - ie can predcit

summary(pr_main)

anova_one_way <- aov(items_avg_patient_ratio ~ LCG, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(P_Month_Avg ~ LCG, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(Registered_Patients ~ LCG, data = pr_main)
summary(anova_one_way)

## different factors, country, town etc

anova_one_way <- aov(items_avg_patient_ratio ~ County, data = pr_main)
summary(anova_one_way)
# Significant to 0.05 -> p = 0.326

anova_one_way <- aov(items_avg_patient_ratio ~ Town, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(items_avg_patient_ratio ~ Postcode, data = pr_main)
summary(anova_one_way)
# significant 

anova_one_way <- aov(items_avg_patient_ratio ~ C_Group, data = pr_main)
summary(anova_one_way)
# Significant to 0.04 -> p = 0.0437



str(pr_main)

anova_one_way <- aov(Registered_Patients ~ LCG, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(Registered_Patients ~ County, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(Registered_Patients ~ Town, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(Registered_Patients ~ Postcode, data = pr_main)
summary(anova_one_way)

anova_one_way <- aov(Registered_Patients ~ C_Group, data = pr_main)
summary(anova_one_way)

# Most interesting results
# CHECK
ms_anova_one_way <- aov(items_avg_patient_ratio ~ LCG, data = pr_main)
summary(ms_anova_one_way)

anova_one_way <- aov(items_avg_patient_ratio ~ County, data = pr_main)
summary(anova_one_way)

ms_anova_one_way <- aov(items_avg_patient_ratio ~ C_Group, data = pr_main)
summary(ms_anova_one_way)



group_by(pr_main, LCG) %>%
  summarise(
    count = n(),
    mean = mean(items_avg_patient_ratio, na.rm = TRUE),
    sd = sd(items_avg_patient_ratio, na.rm = TRUE)
  )

library("ggpubr")
ggline(pr_main, x = "LCG", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "LCG")  + geom_point(aes(color=C_Group))

ggline(pr_main, x = "C_Group", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "C_Group") 

TukeyHSD(ms_anova_one_way)

# Checking the p-values, Southern is the main difference.

# Test 	small 	medium 	large
# tests for proportions (p) 	0.2 	0.5 	0.8
# tests for means (t) 	0.2 	0.5 	0.8
# chi-square tests (chisq) 	0.1 	0.3 	0.5
# correlation test (r) 	0.1 	0.3 	0.5
# anova (anov) 	0.1 	0.25 	0.4
# general linear model (f2) 	0.02 	0.15 	0.35

# power checking
# install.packages("pwr")

library(pwr)

cohen.ES(test = "anov", size = "large")
cohen.ES(test = "anov", size = "medium")
cohen.ES(test = "anov", size = "small")

# k = number of groups, 5
# n = ?, minimum group size
# f = from Cohen test
# sig.level = 0.05
# power = 90%

pr_main %>% group_by(LCG) %>% tally()

pwr.anova.test(k = 5 , 
               f = 0.25, 
               sig.level = 0.05, 
               power = 0.95 ) 


summary(ms_anova_one_way)

# There is a differencce but most likely the effect of the Southern LCG?
# What are the major diffs?

# Split into two groups in line with county boudaries
# Broadly in line with the gene cluster map but is an estimate
# g1 = Derry, Antrim, Down
# g2 Armagh, Tyrone, Fermanagh

# t-test possible if using two groups
# one or two tailed test
# Two sided test to know simply if there is a difference, the direction doesn't matter

pr_main %>% group_by(C_Group) %>% tally()
# Note, group sizes not equal

# H0: There is no difference in the groups
# H1: There is a difference

t.test(items_avg_patient_ratio ~ C_Group,
       data = pr_main,
       alternative = "two.sided",
       paired = FALSE
)


# p-value of 0.03841
# Therefore reject the hypothesis, accept the null hypothesis
# Broad split on county boundaries indicates a difference 
# This split co-incides with split of the LCG boundaries - not perfect match, LCG and county
# boundaries are not exactly aligned, only imperfectly aligned, small overlaps
# Need more granularity

no_bel <- subset(pr_main, pr_main$LCG != "Belfast")
no_bel

ggline(no_bel, x = "LCG", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "LCG")  + geom_point(aes(color=C_Group))

ggline(no_bel, x = "C_Group", y = "items_avg_patient_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "items_avg_patient_ratio", xlab = "C_Group") + geom_point(aes(color=LCG))

# Removal of main conurbation - Belfast
t.test(items_avg_patient_ratio ~ C_Group,
       data = no_bel,
       alternative = "two.sided",
       paired = FALSE
)

ms_anova_one_way <- aov(items_avg_patient_ratio ~ LCG, data = no_bel)
summary(ms_anova_one_way)

ms_anova_one_way <- aov(items_avg_patient_ratio ~ C_Group, data = no_bel)
summary(ms_anova_one_way)

# Compare with wilcox test
wilcox.test(items_avg_patient_ratio ~ C_Group, data = no_bel)

ggplot(no_bel, aes( y=items_avg_patient_ratio)) + 
  geom_boxplot(notch=TRUE,
               outlier.colour="red",
               outlier.size=2,
               fill='#A4A4A4', color="black"
  )  + labs(title="Avg Items / Patient", y = "Avg Items per Patient")



junk <- subset(pr_main, junk$items_avg_patient_ratio < 2.5 )
junk


# EOF #######################################################
