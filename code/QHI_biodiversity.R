# Starter script for Shawn
# 31.3.2020

library(tidyverse)
library(vegan)

# loading data ---- 
setwd("~/Documents/university work/Dissertation/Dissertation")
QHI_pointframe <- read.csv("data/QHI_biodiversity/Herschel_ITEXdata_1999-2018_updated.csv")
head(QHI_pointframe)
levels(QHI_pointframe$SPP) # Checking all spelling is correct
unique(QHI_pointframe$YEAR) # Check years


QHI_cover <- read.csv("data/QHI_biodiversity/QHI_cover_1999_2019_sas.csv")
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

unique(pointfr_2019$SPP)

bareground <-  pointfr_2018_2019 %>% 
  unite(plot_unique, c(SUBSITE, PLOT, YEAR), sep="_") %>%
  # create true/false colunm for soil background
  mutate(bareground = SPP == "XXXlitter" | SPP == "XXXlitter " | 
           SPP == "XXXbareground" | SPP == "XXXbareground " |
           SPP == "XXXrock" | SPP == "XXXrock " |
           SPP == "XXXstandingwater" | SPP == "XXXstandingwater "|
           SPP == "XXXspider" | SPP == "XXXstandingwater ") %>%
  group_by(uniqueID) %>%
  # count both soil background and presence of vegitation
   dplyr::count(bareground) 

 
# filter distinct values 
bareground_IDs <- count(t_IDs$uniqueID) %>% # count number of rows per id 
  filter(freq == 1)                     # fiter only unique rows

colnames(bareground_IDs)[1] <- "uniqueID"   # rename variable


bareground <- bareground %>% 
  filter(uniqueID %in% bareground_IDs$uniqueID, # filter out only unique rows
         bareground == TRUE)                # filter plots that only have soil

bareground <- pointfr_2018_2019 %>%
  filter(uniqueID %in% bareground$uniqueID) %>%
  unite(plot_unique, c(SUBSITE, PLOT, YEAR), sep="_") %>%
  dplyr::count(plot_unique) 

colnames(bareground)[2] <- "bareground"
  

# biodiverstiy ----
# data filtering adapted gergana daskalova spectra_hub/02-scale-biodiv-GD.R

unique(sort(cover_2018_2019$Species))

biodiv <- cover_2018_2019 %>% dplyr::select(sub_name, PLOT, Species, cover, year) %>%
  unite(plot_unique, c(sub_name, PLOT, year), sep="_") %>%
  filter(!Species %in% c("XXXbareground", "XXXfeces",
                         "XXXlitter", "XXXstandingwater", "XXXrock")) %>%
  # standardizing moss
  mutate(case_when(Species == "Xxxmoss" ~ "XXXothermoss")) %>%
  distinct()

unique(sort(biodiv$Species))

biodiv_long <- spread(biodiv, Species, cover)
biodiv_long[is.na(biodiv_long)] <- 0

# remove QHI: form plot unique
biodiv_long <- biodiv_long %>% mutate(plot_unique = str_remove_all(plot_unique, "QHI:"))


# diversity indicies ----

richness <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(richness=sum(x[-c(1:4)]>0))
  })

shannon <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(shannon=diversity(x[-c(1:4)], index="shannon"))
  })

simpson <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(simpson=diversity(x[-c(1:4)], index="simpson"))
  })

evenness <- ddply(biodiv_long,~plot_unique,function(x) {
  data.frame(evenness=diversity(x[-c(1:4)], index="shannon")/log(sum(x[-1]>0)))
  })


  
# status (dead cover) ----

# status (dead cover)
unique(pointfr_2018_2019$STATUS)

str(pointfr_2018_2019)

# filter only top of canopy pointfr hits (ones that are seen by spectrometer)
dead_IDs <-  pointfr_2018_2019 %>% filter(Height..cm. > 1) %>%
  unite(plot_unique, c(SUBSITE, PLOT, YEAR), sep="_") %>%
  filter(STATUS == "Standing dead" | 
           STATUS == "Dead") %>%
  # remove duplicate xy cordinates (only one)
  mutate(duplicated = duplicated(uniqueID)) %>%
  filter(!duplicated == "TRUE")

dead <- dead_IDs %>%
  dplyr::count(plot_unique)

# add in baseR as colonm was not recognized as object in dpylr
colnames(dead)[2] <- "dead"


# Tissue 

unique(pointfr_2018_2019$TISSUE)

# grouping flower and seed pod together as these are likely to contribure to spectral heteogenetiy 

reproducitve_tissue_IDs <-  pointfr_2018_2019 %>% filter(Height..cm. > 1) %>%
  filter(TISSUE == "Flower" | 
           TISSUE == "Seed pod") %>%
  # remove duplicate xy cordinates (none)
  mutate(duplicated = duplicated(uniqueID)) %>%
  filter(!duplicated == "TRUE")

# add standardized plot_unique
reproducitve_tissue_IDs$plot_unique <- paste(reproducitve_tissue_IDs$SUBSITE,
                                             reproducitve_tissue_IDs$PLOT,
                                             reproducitve_tissue_IDs$YEAR,sep="_")

reproducitve_tissue <- reproducitve_tissue_IDs %>%
  dplyr::count(plot_unique)

# add in baseR as colonm was not recognized as object in dpylr
colnames(reproducitve_tissue)[2] <- "reproductive_tissue"

# total cover ----

# Remove rows with no cover
total_cover <- subset(cover_2018_2019, cover>0)
# Remove non veg
total_cover <- subset(total_cover, Species !="XXXlitter" & Species !="XXXlitter " & 
                      Species !="XXXbareground" & Species !="XXXbareground " & 
                      Species !="XXXrock" & Species !="XXXrock " &  Species !="XXXothermoss" &
                      Species !="XXXfeces" & Species !="XXXfeces " & 
                      Species !="XXXstandingwater" & Species !="XXXstandingwater " & 
                      Species !="XXXspider"& Species !="Xxxspider"& Species !="XXXspider ")

# Add unique plots
total_cover$plot_unique <- paste(total_cover$sub_name,
                                 total_cover$PLOT,
                                 total_cover$year,sep="_") 

  # remove QHI: from cover data entry formate
total_cover <- total_cover %>%  mutate(plot_unique = str_remove_all(plot_unique, "QHI:"))

# Convert to relative cover
total_cover <- ddply(total_cover,.(plot_unique), summarise,
                    total_cover = sum(cover))


# gramanoid vs shrub cover ----


unique(sort(pointfr_2018_2019$SPP))

# shrub ----

# grouping flower and seed pod together as these are likely to contribure to spectral heteogenetiy 

shrub_IDs <-  pointfr_2018_2019 %>% filter(Height..cm. > 1) %>%
  filter(SPP == "Salix arctica" | SPP == "Salix phlebophylla" | 
           SPP == "Salix pulchra" | SPP == "Salix reticulata") %>%
  # remove duplicate xy cordinates (none)
  mutate(duplicated = duplicated(uniqueID)) %>%
  filter(!duplicated == "TRUE")

# add standardized plot_unique
shrub_IDs$plot_unique <- paste(shrub_IDs$SUBSITE,
                               shrub_IDs$PLOT,
                               shrub_IDs$YEAR,sep="_")

shrub <- shrub_IDs %>%
  dplyr::count(plot_unique)

# add in baseR as colonm was not recognized as object in dpylr
colnames(shrub)[2] <- "shrub"

# gramanoid cover

unique(sort(pointfr_2018_2019$SPP))

graminoid_IDs <-  pointfr_2018_2019 %>% filter(Height..cm. > 1) %>%
  filter(SPP == "Alopecurus alpinus" | SPP == "Arctagrostis latifolia" | 
           SPP == "Eriophorum angustifolium" | SPP == "Eriophorum vaginatum" |
           SPP == "Festuca baffinensis" | SPP == "Poa arctica" |
           SPP == "Poa arctica " | SPP == "XXXkobresia" |
           SPP == "XXXotherforb" | SPP == "XXXothergram" |
           SPP == "XXXunkgram") %>%
  # remove duplicate xy cordinates (none)
  mutate(duplicated = duplicated(uniqueID)) %>%
  filter(!duplicated == "TRUE")
  

# add standardized plot_unique
graminoid_IDs$plot_unique <- paste(graminoid_IDs$SUBSITE,
                                   graminoid_IDs$PLOT,
                                   graminoid_IDs$YEAR,sep="_")

graminoid <- graminoid_IDs %>%
  dplyr::count(plot_unique)

# add in baseR as colonm was not recognized as object in dpylr
colnames(graminoid)[2] <- "graminoid"

# binding plot level environmental data ----


QHI_plotdata <- left_join(richness, shannon) %>% 
  left_join(simpson) %>%
  left_join(evenness) %>%
  left_join(bareground) %>%
  left_join(dead) %>%
  left_join(reproducitve_tissue) %>%
  left_join(total_cover) %>%
  left_join(shrub) %>%
  left_join(graminoid) %>%
  replace(is.na(.), 0) %>%
  mutate(gram_shrub_ratio = graminoid/shrub)

write.csv(QHI_plotdata, file="~/Documents/university work/Dissertation/Dissertation/data/QHI_biodiversity/QHI_plotdata_2018_2019_sas.csv")


