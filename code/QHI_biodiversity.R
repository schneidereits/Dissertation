# Starter script for Shawn
# 31.3.2020

library(tidyverse)

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
  

mutate(cg = case_when(carb <= 2 ~ "low",
                      carb > 2  ~ "high")
