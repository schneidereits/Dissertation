# CONVERTING POINT FRAME DATA TO COVER
# Created by Haydn Thomas, 24 April 2015
# Run for 2019 QHI Data by Shawn Schneidereit

# Note - make sure the species names are consistent!

# Import Data -------------------------------------------------------------------
abundance <- read.csv("scripts/users/gdaskalova/Herschel_ITEXdata_1999-2018.csv", stringsAsFactors = FALSE, strip.white = TRUE)
head(abundance)

# Note that depending on how the data has been entered, species names may not be in a usable format. For the Herschel data abbreviations have been used so it requires an additional step later to convert these into species names. If data is entered electronically in the future this step can be bypassed.

# Subset for desired year to add
abundance$plotID <- paste(abundance$SUBSITE, abundance$PLOT, sep="")
abundance$plot_yrID <- paste(abundance$plotID, abundance$YEAR, sep="")

# Run cover calculations--------------------------
# Convert abundance to cover
Out<-NULL
for (i in unique(abundance$plot_yrID)){
  mydata <- abundance[abundance$plot_yrID==i,]
  xycoords <- unique(mydata[,c("X","Y")])
  npts <- as.integer(nrow(xycoords))
  sppts <- unique(mydata[,c("SPP","X","Y")])
  for (j in unique (sppts$SPP)){
    mypts <- as.integer(nrow (sppts[sppts$SPP==j,]))
    Out <- rbind(Out, c(j,i,npts,mypts))
  }
}

# Covert to data frame, to numeric, and calculate cover
Out <- data.frame(Out)
names(Out) <- c("Species","plotID", "npts", "mypts")
Out$npts <- as.numeric(as.character(Out$npts))
Out$mypts <- as.numeric(as.character(Out$mypts))
Out$relCover <- Out$mypts/Out$npts

# Extract relevant columns
cover <- Out[,c(1,2,5)]
cover$SITE <- substr(cover$plotID, 1,2)
cover$PLOT <- substr(cover$plotID, 3,3)
cover$YEAR <- substr(cover$plotID, 4,7)

# Add species data------------------------
# Check for missing data
as.data.frame(unique(cover[is.na(cover$Species),1]))
# Check carefully that no abbreviations or wrong species names have made their way in

#Match column names to ITEX dataset-----------------------------------
cover$sub_id <- ""
cover[cover$SITE=="HE",]$sub_id <- "i36.02";cover[cover$SITE=="KO",]$sub_id <- "i36.01"
cover$site_id <- "i36"
cover$site_name <- "QHI"
cover$sub_name <- paste("QHI",cover$SITE,sep=":")
cover$cover <- round(cover$relCover*100,1)
cover$year <- cover$YEAR
cover$trtmt <- "CTL"
cover$sub_lat <- 69.58
cover$sub_long <- ""
cover[cover$SITE=="HE",]$sub_long <- 138.86;cover[cover$SITE=="KO",]$sub_long <- 138.87
cover$sub_ele <- ""
cover[cover$SITE=="HE",]$sub_ele <- 76;cover[cover$SITE=="KO",]$sub_ele <- 73

#Create output dataset with relevant and ordered columns
QHI_cover_1999_2018_ITEX <- cover[,c(7:11,1,12,13,5,14,15,16)]

#Write csv if required-------------------
write.csv(QHI_cover_1999_2018_ITEX, file="scripts/users/gdaskalova/QHI_cover_1999_2018_ITEX.csv")
