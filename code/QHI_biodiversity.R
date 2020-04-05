# Starter script for Shawn
# 31.3.2020

library(tidyverse)
library(vegan)

# loading data ---- 
setwd("~/Documents/university work/Dissertation/Dissertation")
QHI_pointframe <- read.csv("Dissertation/data/QHI_biodiversity/Herschel_ITEXdata_1999-2018_updated.csv")
head(QHI_pointframe)
levels(QHI_pointframe$SPP) # Checking all spelling is correct
unique(QHI_pointframe$YEAR) # Check years

QHI_cover <- read.csv("Dissertation/data/QHI_biodiversity/QHI_cover_1999_2019_sas.csv")
head(QHI_cover)
levels(QHI_cover$name) # Checking all spelling is correct
unique(QHI_cover$year) # Check years


# Steps 
# 1 Integrate 2019 data
# 2 Check plot order using cover of different species - especially in KO
# 3 Confirm species IDs and spellings are consistent
# 4 Calculate biodiversity metrics

unique(sort(QHI_pointframe_full$SPP))
# species with spelling error
# Kobresia myotosoides
# "Pedicularis longsdorfi"     "Pedicularis longsdorfi "  
# "Cetraria spp that is brown" "Cladina (brown)"
# "Poa ?"  "Poa arctica  "Poa arctica "
#  "Senecio astropurpureus" "Senecio atropurpureus"


QHI_pointframe_full$SPP <- recode(QHI_pointframe_full$SPP, "Pedicularis longsdorfi " = "Pedicularis longsdorfi")
QHI_pointframe_full$SPP <- recode(QHI_pointframe_full$SPP, "Kobresia myosuroides" = "Kobresia myotosoides")
QHI_pointframe_full$SPP <- recode(QHI_pointframe_full$SPP, "Poa arctica " = "Poa arctica")
QHI_pointframe_full$SPP <- recode(QHI_pointframe_full$SPP, "Senecio atropurpureus " = "Senecio atropurpureus")
# still need to confirm Cetraria spp that is brown" "Cladina (brown)" , "Poa ?",  "Cetraria spp"



# sorting plot ----

sort_QHI <- QHI_cover %>%
  #add_column(n="plot") %>%
  mutate(n = "plot",
         n2 = "year") %>%
 # unite("plot2" , c(n,plot)) %>%
  unite("year2", c(n2, year)) %>%
  group_by(sub_name, year2, PLOT) %>%
  #filter(name == "Arctagrostis latifolia" | name == "Dryas integrifolia" | name == "Eriophorum angustifolium" | name =="Lupinus arcticus") %>%
  filter(Species == "Lupinus arcticus" | Species == "LUPARC") %>%
  summarise(Species = first(Species),
            cover = first(cover))
  

ggplot(sort_QHI, aes(x=PLOT, y= cover, color= year2)) +
       geom_point() +
  geom_line() +
  facet_wrap(.~sub_name) 

# KO 2017, 2018, 2019  need to be flipped 

  
# filter and prep 2018 & 2019 data ----

pointfr_2018_2019 <- QHI_pointframe_full %>%
  filter(YEAR == "2018" | YEAR == "2019") %>%
  # flip KO plots are they were found to be reverse. HE stays the same (not pretty but it works)
  mutate(PLOT = case_when(SUBSITE == "HE" & PLOT == 1 ~ 1,
                          SUBSITE == "HE" & PLOT == 2 ~ 2,
                          SUBSITE == "HE" & PLOT == 3 ~ 3,
                          SUBSITE == "HE" & PLOT == 4 ~ 4,
                          SUBSITE == "HE" & PLOT == 5 ~ 5,
                          SUBSITE == "HE" & PLOT == 6 ~ 6,
                          SUBSITE == "KO" & PLOT == 1 ~ 6,
                          SUBSITE == "KO" & PLOT == 2 ~ 5,
                          SUBSITE == "KO" & PLOT == 3 ~ 4,
                          SUBSITE == "KO" & PLOT == 4 ~ 3,
                          SUBSITE == "KO" & PLOT == 5 ~ 2,
                          SUBSITE == "KO" & PLOT == 6 ~ 1))
  

cover_2018_2019 <- QHI_cover_1999_2019_sas %>%
  filter(year == "2018" | year == "2019") %>%
  # flip KO plots are they were found to be reverse. HE stays the same (not pretty but it works)
  mutate(PLOT = case_when(sub_name == "QHI:HE" & PLOT == 1 ~ 1,
                          sub_name == "QHI:HE" & PLOT == 2 ~ 2,
                          sub_name == "QHI:HE" & PLOT == 3 ~ 3,
                          sub_name == "QHI:HE" & PLOT == 4 ~ 4,
                          sub_name == "QHI:HE" & PLOT == 5 ~ 5,
                          sub_name == "QHI:HE" & PLOT == 6 ~ 6,
                          sub_name == "QHI:KO" & PLOT == 1 ~ 6,
                          sub_name == "QHI:KO" & PLOT == 2 ~ 5,
                          sub_name == "QHI:KO" & PLOT == 3 ~ 4,
                          sub_name == "QHI:KO" & PLOT == 4 ~ 3,
                          sub_name == "QHI:KO" & PLOT == 5 ~ 2,
                          sub_name == "QHI:KO" & PLOT == 6 ~ 1))


# ** Bareground cover data ----

# source code: Qikiqtaruk Ecological Monitoring manuscript script
# Code for all modelling and data visualisation within the manuscript
# Written by Isla Myers-Smith, Anne Bjorkman, Haydn Thomas, Sandra Angers-Blondin and Gergana Daskalova

pointfr_2018_2019$uniqueID <- paste(pointfr_2018_2019$YEAR, pointfr_2018_2019$SUBSITE, 
                                    pointfr_2018_2019$PLOT, pointfr_2018_2019$X, pointfr_2018_2019$Y, sep="")  # Assign unique ID to every point
bareground_IDs <- pointfr_2018_2019[pointfr_2018_2019$SPP=="XXXlitter" | pointfr_2018_2019$SPP=="XXXlitter " | 
                              pointfr_2018_2019$SPP=="XXXbareground" | pointfr_2018_2019$SPP=="XXXbareground " | 
                              pointfr_2018_2019$SPP=="XXXrock" | pointfr_2018_2019$SPP =="XXXrock " | 
                              pointfr_2018_2019$SPP =="XXXfeces" | pointfr_2018_2019$SPP =="XXXfeces " | 
                              pointfr_2018_2019$SPP =="XXXstandingwater" | pointfr_2018_2019$SPP =="XXXstandingwater " | 
                              pointfr_2018_2019$SPP =="XXXspider", "uniqueID"]  # Identify points with non-vegetation indicators
pointfr_2018_2019_BGs <- pointfr_2018_2019[pointfr_2018_2019$uniqueID %in% bareground_IDs,]  # Extract only points that have non-veg indicators

Out=NULL  # Set up loop
for(i in unique(pointfr_2018_2019_BGs$uniqueID)){  # For each point
  a <- subset(pointfr_2018_2019_BGs, uniqueID==i)  # create dataframe of all entries for that point
  b <- a[a$SPP == "XXXlitter" | a$SPP == "XXXlitter " | a$SPP == "XXXbareground" | 
           a$SPP == "XXXbareground " | a$SPP == "XXXrock" | a$SPP == "XXXrock " | 
           a$SPP == "XXXfeces" | a$SPP == "XXXfeces " | a$SPP == "XXXstandingwater" | 
           a$SPP == "XXXstandingwater " | a$SPP == "XXXspider",]  # Identify how many entries are not vegetation
  c <- nrow(a) - nrow(b)  # Finnd out if any entries are vegetation (i.e. total rows - non-veg rows)
  Out <- rbind(Out, c(i, c))  # Extract point name and number vegetation entries
}

BGs <- as.data.frame(Out)  # Convert into data frame
BGs <- subset(BGs,V2=="0")  # Extract points for which there are only non-veg data (i.e.i.e. total rows - non-veg rows = 0)
BGs <- BGs[,1]  # Extract only first column (unique points)

bareground <- pointfr_2018_2019_BGs <- pointfr_2018_2019[pointfr_2018_2019$uniqueID %in% BGs,]  # Create dataframe of bare ground points 
bareground <- ddply(bareground,.(YEAR, PLOT, SUBSITE), summarise,
                    Bareground = sum(Abundance))  # Count number of bare ground points per plot

# Create dummy dataframe with all plots because if plots have no bare ground they wont be included
bareground_full <- ddply(pointfr_2018_2019,.(YEAR, SUBSITE, PLOT), summarise,
                         Bareground = 0)  

# Replace dummy bareground with real baregound
bareground_full$Bareground <- bareground$Bareground[match(paste(bareground_full$YEAR,
                                                                bareground_full$SUBSITE,
                                                                bareground_full$PLOT),
                                                          paste(bareground$YEAR,
                                                                bareground$SUBSITE,
                                                                bareground$PLOT))] 
bareground <- bareground_full  # Rename to original
bareground[is.na(bareground$Bareground),]$Bareground <- 0  # Replace NAs from match with zeros


# ** Community composition and diversity measures ----
# source code: Qikiqtaruk Ecological Monitoring manuscript script
# Code for all modelling and data visualisation within the manuscript
# Written by Isla Myers-Smith, Anne Bjorkman, Haydn Thomas, Sandra Angers-Blondin and Gergana Daskalova


# Remove rows with no cover
diversity <- subset(cover_2018_2019, cover>0)
# Remove non veg
diversity <- subset(diversity, Species !="XXXlitter" & Species !="XXXlitter " & 
                      Species !="XXXbareground" & Species !="XXXbareground " & 
                      Species !="XXXrock" & Species !="XXXrock " & 
                      Species !="XXXfeces" & Species !="XXXfeces " & 
                      Species !="XXXstandingwater" & Species !="XXXstandingwater " & 
                      Species !="XXXspider"& Species !="Xxxspider"& Species !="XXXspider ")
# Add unique plots
diversity$plot_unique <- paste(diversity$sub_name,diversity$PLOT,diversity$year,sep="_")

# Convert to relative cover
plot_cover <- ddply(diversity,.(plot_unique), summarise,
                    total_cover = sum(cover))
diversity$total_cover <- plot_cover$total_cover[match(diversity$plot_unique, plot_cover$plot_unique)]
diversity$rel_cover <- diversity$cover/diversity$total_cover*100

# richness 
#modified from GD spectra hub

unique(sort(cover_2018_2019$Species))

richness <- cover_2018_2019 %>% 
  # removing non species 
  filter(!Species %in% c("XXXbareground", "XXXfeces",
                         "XXXlitter", "XXXstandingwater", "XXXrock")) %>%
  # standardizing moss
  mutate(case_when(Species == "Xxxmoss" ~ "XXXothermoss")) %>%
  select(sub_name, PLOT, Species, year) %>%
  distinct() %>%
  group_by(sub_name, PLOT, year) %>% 
  tally() 

colnames(richness)[4] <- "richness"

richness <- richness %>%
  rename()

# bio diverstiy
# source code from gergana daskalova spectra_hub/02-scale-biodiv-GD.R

unique(sort(cover_2018_2019$Species))

biodiv <- cover_2018_2019 %>% dplyr::select(sub_name, PLOT, Species, cover, year) %>%
  filter(!Species %in% c("XXXbareground", "XXXfeces",
                         "XXXlitter", "XXXstandingwater", "XXXrock")) %>%
  # standardizing moss
  mutate(case_when(Species == "Xxxmoss" ~ "XXXothermoss")) %>%
  distinct()

unique(sort(biodiv$Species))

biodiv_long <- spread(biodiv, Species, cover)
biodiv_long[is.na(biodiv_long)] <- 0

# create new unique plot colunm
biodiv_long$plot_unique <- paste(biodiv_long$sub_name,biodiv_long$PLOT,biodiv_long$year,sep="_")

# remove extra colunms
biodiv_long <- biodiv_long %>% select(-sub_name, -PLOT, -year)

# diversity indicies 

richness <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(richness=sum(x[-1]>0))
  })

shannon <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(shannon=diversity(x[-1], index="shannon"))
  })

simpson <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(simposon=diversity(x[-1], index="simpson"))
  })

evenness <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(evenness=diversity(x[-1], index="shannon")/log(sum(x[-1]>0)))
  })

# bind with bareground data 
  
t <- left_join(richness, shannon) %>% 
  left_join(simpson) %>%
  left_join(evenness) %>%
  


head(bareground)
