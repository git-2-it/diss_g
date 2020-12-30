# diss01
# run 3
# GP data ingested, amalgamated and saved to file.


library(tidyverse)

# install.packages("PostcodesioR")
library(PostcodesioR)
# citation(package = "PostcodesioR")


# --------------------------------------------------
# Get practise data
# --------------------------------------------------

main_path <- "~/courses/dissertation/data"

datapath <- "portal_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath


docs_in20 <- read.csv(paste(datapath, "gp-practice-reference-file--january-2020.csv", sep = "/"))
docs_in20$year = "2020"
col_names <- colnames(docs_in20)
col_names

docs_in19 <- read.csv(paste(datapath,"gp-practice-reference-file---january-2019.csv", sep = "/"))
docs_in19$year = "2019"
colnames(docs_in19) <- col_names

docs_in18 <- read.csv(paste(datapath, "gp-practice-reference-file---january-2018.csv", sep = "/"))
docs_in18$year = "2018"
colnames(docs_in18) <- col_names

docs_in17 <- read.csv(paste(datapath, "gp-practice-reference-file---january-2017.csv", sep = "/"))
docs_in17$year = "2017"
colnames(docs_in17) <- col_names

docs_in16 <- read.csv(paste(datapath, "gp-practice-reference-file---january-2016.csv", sep = "/"))
docs_in16$year = "2016"
colnames(docs_in16) <- col_names

# July 2015 the oldest data
docs_in15 <- read.csv(paste(datapath, "gp-practice-reference-file---july-2015.csv", sep = "/"))
docs_in15$year = "2015"
colnames(docs_in15) <- col_names

col_names

# bind order important to keep the year
docs_in <- rbind(docs_in20, docs_in19, docs_in18, docs_in17, docs_in16, docs_in15)
str(docs_in)

docs_in$PracticeName[docs_in$PracticeName == "NULL"] <- NA

colSums(is.na(docs_in))
chk <- docs_in[!complete.cases(docs_in), ]

# too many bad records to just let pass?
# deduplicate, to see what remains after
chk <- subset(docs_in, !duplicated(docs_in$PracNo))
colSums(is.na(chk))

chk$PracNo <- as.factor(chk$PracNo)
str(chk)
docs_in$PracNo <- as.factor(docs_in$PracNo)
str(docs_in)

# the deduplication clears out the bad data
# here and the checked NA should be removed.

docs_in <- docs_in[complete.cases(docs_in), ]
colSums(is.na(docs_in))
str(docs_in)

docs_in <- subset(docs_in, !duplicated(docs_in$PracNo))
str(docs_in)

docs_in$year <- as.factor(docs_in$year)
str(docs_in)

head(docs_in)

# Examine structure of the file
str(docs_in)

# Required fields - 
# PracNo 
# Name
# Address 1 for identification
# Postcode
# LCG (Local Commissioning Group) - group responsible for addressing health and social care needs
# Registered Patients

docs_in <- docs_in[, -c(4,5)]

docs_in <- droplevels(docs_in)

# Want to join this with postcode data to have town identifier

# --------------------------------------------------
# Read Postcode data using API
# --------------------------------------------------

# create postcode finder for lat/long
# lat_long_chk <- function(long_in, lat_in)
postcode_api_lookup <- function(x)
{
  # str(x)
  
  find_code <- x["Postcode" ]

  #found_code <- postcode_lookup(find_code)
  # return(found_code$postcode)
  # str(found_code)
  
  postcode <- "NOTFOUND"
  
  tryCatch(
    {
      foundcode <- postcode_lookup(find_code)
      postcode <- foundcode$postcode
    } , 
    error=function(cond) {
      postcode <- "NOTFOUND"
    },
    warning=function(cond) {
      postcode <- "NOTFOUND"
    }
  )
  
  return(postcode)
  
} # end function

# citation(package = "cluster")
docs_in$postcode_2 <- apply(docs_in, 1, postcode_api_lookup)
str(docs_in$postcode_2)

# postcode_api_lookup("BT postcode")

########################################################
# The postcode database has found all practice address?
# API call sets missing postcode to NOTFOUND

colSums(is.na(docs_in))
chk  <- subset(docs_in, docs_in$postcode_2 == "NULL")
chk

# Manually fix
str(docs_in)
docs_in$postcode_2[docs_in$Postcode == "BT12 7DP"] <- "BT12 7DN"

#########################################################
# Noting the format difference between the 2 datasets, practices data includes a 
# space in the postcode
# Normalise this to same format as postcodes dataset, ie; remove the space.
# docs_in$Postcode <-   gsub('\\s+', '',docs_in$Postcode)
docs_in$postcode_2 <-   gsub('\\s+', '',docs_in$postcode_2)

# docs_in$Postcode <- as.factor(docs_in$Postcode)
docs_in$Postcode <- as.character(docs_in$Postcode)

str(docs_in)

#######################################################
# Read list of post towns and add to data set

datapath <- "general"

datapath <- paste(main_path, datapath, sep = "/")
datapath

posttowns_in <- read.csv(paste(datapath, "postcodes_posttowns.csv", sep = "/"))
posttowns_in
str(posttowns_in)

#posttowns_in <- posttowns_in[, 1:2, ]
posttowns_in <- select(posttowns_in, Postcode.district, Post.town, County )
posttowns_in

colnames(posttowns_in) <- c("District", "Town", "County")
posttowns_in

posttown_api_lookup <- function(x)
{
  # str(x)
  
  find_code <- x["Postcode" ]
  
  postcode <- "NOTFOUND"
  
  tryCatch(
    {
      foundcode <- postcode_lookup(find_code)
      postcode <- foundcode$outcode
    } , 
    error=function(cond) {
      postcode <- "NOTFOUND"
    },
    warning=function(cond) {
      postcode <- "NOTFOUND"
    }
  )
  
  return(postcode)
  
} # end function


docs_in$District <- apply(docs_in, 1, posttown_api_lookup)
str(docs_in$District)

docs_in <- merge(x = docs_in, y = posttowns_in, by.x = "District", by.y = "District")


#######################################################
# Save clean practice dataset to file
docs_in <- subset(docs_in, !duplicated(docs_in$PracNo))


datapath <- "portal_data"
datapath <- paste(main_path, datapath, sep = "/")
datapath

cleaned_practice_data_file <- "cleaned_practice_data.csv"
cleaned_practice_data_file <- paste(datapath, cleaned_practice_data_file, sep = "/")

write.csv(file=cleaned_practice_data_file, x=docs_in, quote=TRUE, row.names = FALSE)

########################################################
# Group the practices
# Grouping based on county bundaries
# Grouping needs tp be based on the gene clusters
# Previous caveats still apply
docs_in$C_Group <- "g2"
docs_in$C_Group[docs_in$County == "ANTRIM"] <- "g1"
docs_in$C_Group[docs_in$County == "LONDONDERRY"] <- "g1"
docs_in$C_Group[docs_in$County == "DOWN"] <- "g1"

write.csv(file=cleaned_practice_data_file, x=docs_in, quote=TRUE, row.names = FALSE)


