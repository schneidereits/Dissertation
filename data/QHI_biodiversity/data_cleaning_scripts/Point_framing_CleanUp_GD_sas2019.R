#################################################
#                                               #
#  Point Framing Data Clean Up & Analysis Prep  #
#                                               #
#    Written by Gergana Daskalova 02/02/2017    #
#             gndaskalova@gmail.com             #
#                                               #
#################################################

# Packages
library(data.table)
library(plyr) # Load plyr first to avoid clashes with dplyr
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)


# Setting the working directory to where the csv files are and loading them in
# Careful not to have any other csvs in that folder

setwd("data/QHI_biodiversity/Point_framing_siteplot/")
list <- list.files(pattern = ".csv")

# A function to add a filename (i.e. plot number) column
read_csv_filename <- function(filename){
  point_framing <- read.csv(filename)
  point_framing$Plot <- filename
  point_framing
}

# Loading all the csv files into one dataframe
pointfr <- ldply(list, read_csv_filename)

# Turning SP01.csv, SP02.csv, etc., into just SP01

pointfr <- pointfr %>% separate(Plot, c("PlotN", "filename"), sep="\\.") 



pointfr <- pointfr %>% 
  select (-filename )%>% 
  # to replace NA with coridinates of first value
  fill(X, Y) %>%
  mutate(YEAR = "2019",
         SITE = "QHI", 
         # extract only vegetation type
         SUBSITE = case_when(grepl("HE|KO", PlotN) ~
                            stringr::str_extract(PlotN, "HE|KO")),
         # create plot colomn and remove vegetation type
         PLOT = PlotN, 
         PLOT = str_remove_all(PLOT, "HE|KO"),
         TRTMT = "CTL", 
         Hit.Order = "N/A") %>%
  rename(STATUS = Status,
         TISSUE = Tissue, 
         Abundance = Count)

# add in baseR as colonm was not recognized as object in dpylr
colnames(pointfr)[3] <- "SPP"

head(pointfr)
str(pointfr)


# Checking the data

# Does each of the 36 sub-plots have 36 height observations? ------NO------
36*36 # Meant to have 1296 HEIGHT observations

pointfr %>% filter(!is.na(Height..cm.)) 
58+1234 # 1292 
1296-1292 # 4 are misssing

length(pointfr$Height..cm.)
summary(pointfr$Height..cm.) # 4294 observation in total, of which 3002are NAs

# Do all observations have spatial XY coordinates (multiple rows can share the same XY coordinates)

unique(pointfr$X) 
unique(pointfr$Y) # No NAs, all fine



#summary(pointfr$X) # 11 NAs in the X column
# which(is.na(pointfr$X)) # Rows 3836-3846 - went back and deleted them, they were just extra blank rows

# Do all unique locations have a height reading? -----NO------
    # Fixed the ones which have hit the surface (added height=0)
    # A few have hit Salix richardsonii, but don't have anything written down for Height
          # I imagine the salix did have some height, wasn't just zero? Especially the ones that were stems...

# A reality check on the height values (no >100 cm)
# Corrected a 2m tall gramminoid to 19.6cm (SP10 1x5)
# Highest height 103cm
summary(pointfr$Height..cm.)

# How many subplots are missing observations

# SP19 1x5 no height
# SP22 3x6, 4x1, 4x2, 4x3, 4x4 MISSING; 4x5 missing height for Salix r.
# SP23 1x5 missing height salix r.
# SP277 1x6 missing height, and 1x2 has TWO height measurements
# SP32 4x6 missing height
# SP33 6x1, 6x2, 6x3, 6x4, 6x5, 6x6 MISSING
# SP36 6x1, 6x2, 6x3, 6x4, 6x5, 6x6 MISSING

# Ensuring that all row/observations with a value of 'XXXothermoss' in column Tissue have 'N/A' in the Tissue variable 
         # and  'Live' in the Status variable.

# Need to make the variables characters to make the change

pointfr$STATUS <- as.character(pointfr$STATUS)
pointfr$TISSUE <- as.character(pointfr$TISSUE)
pointfr[pointfr$SPP == 'XXXothermoss',]$TISSUE <- "NA"
pointfr[pointfr$SPP == 'XXXothermoss',]$STATUS <- "Live"

# Ensuring that all row/observations with a value of 'XXXlitter' in column C have 'Dead' in the Status variable.

pointfr[pointfr$SPP== 'XXXlitter',]$STATUS <-  "Dead"
  
# Standardising the case (live vs Live) used in the Status column
# Making the variables factors again
pointfr$STATUS <- as.factor(pointfr$STATUS)
pointfr$TISSUE <- as.factor(pointfr$TISSUE)


pointfr$STATUS <- recode(pointfr$STATUS, live="Live")
levels(pointfr$STATUS)
levels(pointfr$TISSUE) # Need to make blank cells, N/A and NA all NAs

# Empty rows to NAs

# Define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

## Transform all blank cells to NAs
pointfr <- pointfr %>% mutate_each(funs(empty_as_na)) 
str(pointfr)

# As a side effect of transforming spaces and blank cells to NAs, R now thinks factors are characters, changing them back
pointfr$STATUS <- as.factor(pointfr$STATUS)
pointfr$SPP <- as.factor(pointfr$SPP)
pointfr$TISSUE <- as.factor(pointfr$TISSUE)
pointfr$PlotN <- as.factor(pointfr$PlotN)
str(pointfr)

# Changing them to NA
pointfr$STATUS <- recode(pointfr$STATUS, "N/A"="NA")
levels(pointfr$STATUS)

pointfr$TISSUE <- recode(pointfr$TISSUE, "N/A"="NA")
levels(pointfr$TISSUE)

# Standardising Equisetum and Equisetum spp. to be the same
levels(pointfr$Species)
pointfr$Species <- recode(pointfr$Species, "Equisetum spp." = "Equisetum")


#write.csv(pointfr, file="pointfr.csv")
