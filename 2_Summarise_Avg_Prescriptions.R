# diss01
# run 2 
# Reads summary data to create total and average figures 
# Several datasets required apart from the prescription data itself
# Practice data
# Post code data (for practice geo-location)

# --------------------------------------------------
# Get practise data
# --------------------------------------------------

# Init required libraries
library("readr")
library("tidyr")
library("dplyr")

main_path <- "~/courses/dissertation/data"

datapath <- "portal_data/years"

datapath <- paste(main_path, datapath, sep = "/")
datapath

coltypes <- list(
  col_integer(), col_integer(), col_integer(), 
  col_character(), 
  col_integer()
)

# Read in the data
data_read_in <- list.files(path=datapath, full.names=TRUE, recursive = TRUE, pattern = "*_summary.csv") %>% 
  lapply(read_csv, col_types = coltypes) %>%   bind_rows

# Check for notable problems or warnings 
problems(data_read_in)
warnings()

# Check Structure
str(data_read_in)


# Sanity check on the inputs by counting number of years/month
# Expecting  12 090 116 (approx 450k per month @ 26 Jan2018 to date) - this isnt right
n_distinct(data_read_in$Year)
n_distinct(data_read_in$Month)
months <- unique(data_read_in$Month)
months
years <- unique(data_read_in$Year)
years

colSums(is.na(data_read_in))
# Numbers of NA are low (in the columns of interest) comparative to dataaset size
# Will be removed after other checks

str(data_read_in)

# dataset review
str(data_read_in)

# Not all are suitable types, eg Practice is an int, should be factor, 
# year and month are int, could be a combined date
# grab col names and replace blanks with underscore
col_names <- colnames(data_read_in)
col_names <- gsub(" ", "_", col_names)
col_names <- gsub("-", "_", col_names)
col_names <- trimws(col_names)
colnames(data_read_in) <- col_names
str(data_read_in)

data_in <- data_read_in
summary(data_in)

# Plot some visuals to help understand the shape of the data set
library("viridis")

orig_par <- par()

# plot practice  frequency stacked by  chapter
# Indication of volumes and types of items, not frequency per se
# To show the range of chapters included in the data
plot_table0 <- table(data_in$BNF_Chapter)

barplot(plot_table0, 
        main = "Basic count of BNF Chapters", 
        sub = "Includes non-relevant Chapter data",
        xlab = "Chapter", 
        ylab = "Frequency",
        col = viridis(5),
        cex.names = 0.75,
        beside=TRUE,
        args.legend=list(bty="n",horiz=TRUE),
)

# data_in <- subset(data_in, select = -c(BNF.Code))

data_in$BNF_Chapter[data_in$BNF_Chapter == "-"] <- NA
data_in$BNF_Chapter[data_in$BNF_Chapter == "99"] <- NA
data_in$BNF_Chapter[data_in$BNF_Chapter == "0"] <- NA

str(data_in)
data_in <- data_in[complete.cases(data_in), ]
str(data_in)

data_in$BNF_Chapter  <- as.integer(data_in$BNF_Chapter)
# data_in$BNF_Paragraph  <- as.integer(data_in$BNF_Paragraph)

colSums(is.na(data_in))
na_check <- data_in[!complete.cases(data_in), ]
na_check

data_in <- data_in[complete.cases(data_in), ]
colSums(is.na(data_in))

data_in$BNF_Chapter <- replace_na(data_in$BNF_Chapter, 0)

colSums(is.na(data_in))

# recheck changes
plot_table0 <- table(data_in$BNF_Chapter)

barplot(plot_table0, 
        main = "Basic count of BNF Chapters", 
        sub = "Includes non-relevant Chapter data",
        xlab = "Chapter", 
        ylab = "Frequency",
        col = viridis(5),
        cex.names = 0.75,
        beside=TRUE,
        args.legend=list(bty="n",horiz=TRUE),
)

###########################################
# Sum and Avg for Practices and Chapters
# Sum and Avg for Practices
###########################################

# Practices and chapters
# Get sum for each, by year/month
practice_items_sum_GT_YMPC  <- aggregate(data_in$Total_Items,
                                       by=list(
                                         data_in$Year,
                                         data_in$Month,
                                         data_in$Practice,
                                         data_in$BNF_Chapter
                                       ),
                                       FUN=sum)
colnames(practice_items_sum_GT_YMPC) <- c("Year", "Month", "Practice","BNF_Chapter", "Total_Items") 

# count entries by practice, chapter - and year and month
tally_PC <- practice_items_sum_GT_YMPC %>% group_by(Practice, BNF_Chapter) %>% tally(name = "PC_count")

practice_items_sum_GT_PC  <- aggregate(practice_items_sum_GT_YMPC$Total_Items,
                                       by=list(
                                         practice_items_sum_GT_YMPC$Practice,
                                         practice_items_sum_GT_YMPC$BNF_Chapter
                                       ),
                                       FUN=sum)
colnames(practice_items_sum_GT_PC) <- c("Practice", "BNF_Chapter", "Total_Items_PC") 

# Divide items summed by practice and chapter 
# by number of entries of year and month to get monthly average  
practice_chapter_month <- left_join(practice_items_sum_GT_PC, tally_PC, 
                                    by = c("Practice" = "Practice", "BNF_Chapter" = "BNF_Chapter")
)

# create the average
# This is the actual average based on the total of items and the number of months
# the relevant chapter has appeared
practice_chapter_month$PC_Month_Avg <- practice_chapter_month$Total_Items_PC / practice_chapter_month$PC_count

str(practice_chapter_month)

practice_chapter_month <- select(practice_chapter_month, -c(PC_count))


##### now practices only
practice_items_sum_GT_YMP  <- aggregate(practice_items_sum_GT_YMPC$Total_Items,
                                         by=list(
                                           practice_items_sum_GT_YMPC$Year,
                                           practice_items_sum_GT_YMPC$Month,
                                           practice_items_sum_GT_YMPC$Practice
                                         ),
                                         FUN=sum)
colnames(practice_items_sum_GT_YMP) <- c("Year", "Month", "Practice", "Total_Items") 

practice_items_sum_GT_P  <- aggregate(practice_items_sum_GT_YMPC$Total_Items,
                                      by=list(
                                        practice_items_sum_GT_YMPC$Practice
                                      ),
                                      FUN=sum)
colnames(practice_items_sum_GT_P) <- c("Practice", "Total_Items_P") 

tally_P <- practice_items_sum_GT_YMP %>% group_by(Practice) %>% tally(name = "P_count")

practice_month <- left_join(practice_items_sum_GT_P, tally_P, 
                                    by = c("Practice" = "Practice")
)

practice_month$P_Month_Avg <- practice_month$Total_Items_P / practice_month$P_count

practice_month <- select(practice_month, -c(P_count))

##### end practices only

# Join practice info at the different levels
practice_table <- left_join(practice_month, practice_chapter_month,
                         by = c("Practice" = "Practice"
                         ))

str(data_in)

practice_summary_info_file <- "portal_data/practice_summary_info.csv"
practice_summary_info_file <- paste(main_path, practice_summary_info_file, sep = "/")
practice_summary_info_file

prescription_sub_file <- "portal_data/prescription_sub.csv"
prescription_sub_file <- paste(main_path, prescription_sub_file, sep = "/")
prescription_sub_file


write.csv(file=practice_summary_info_file, x=practice_table, quote=TRUE, row.names = FALSE)
# write.csv(file=prescription_sub_file, x=data_in, quote=TRUE, row.names = FALSE)

head(practice_table, n=10)

practice_table$BNF_Chapter <- as.factor(practice_table$BNF_Chapter)

data_in$BNF_Chapter <- as.factor(data_in$BNF_Chapter)
plot_table0 <- table(data_in$BNF_Chapter)

library("ggplot2") 

par(mfrow = c(1, 2))

ggplot(practice_table, aes(x = BNF_Chapter, y = Total_Items_PC, fill = BNF_Chapter) ) + 
  geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
  ggtitle("Items dispensed") + 
  labs(title="Total items dispensed per Chapter",
       x ="BNF Chapter", y = "# Items")   + theme(legend.position="none")

ggplot(practice_table, aes(x = BNF_Chapter, y = PC_Month_Avg, fill = BNF_Chapter) ) + 
  geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
  ggtitle("Items dispensed") + 
  labs(title="Avg items dispensed per Chapter",
       x ="BNF Chapter", y = "# Items")   + theme(legend.position="none")


# ##  EOF

