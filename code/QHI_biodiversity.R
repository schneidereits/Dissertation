# Starter script for Shawn
# 31.3.2020


# loading data ---- 
setwd("~/Documents/university work/Dissertation/Dissertation")
QHI_pointframe <- read.csv("data/QHI_biodiversity/Herschel_ITEXdata_1999-2018_updated.csv")
head(QHI_pointframe)
levels(QHI_pointframe$SPP) # Checking all spelling is correct
unique(QHI_pointframe$YEAR) # Check years

QHI_cover <- read.csv("data/QHI_biodiversity/QHI_cover_1999_2018_ITEX_updated.csv")
head(QHI_cover)
levels(QHI_cover$name) # Checking all spelling is correct
unique(QHI_cover$year) # Check years


# Steps 
# 1 Integrate 2019 data
# 2 Check plot order using cover of different species - especially in KO
# 3 Confirm species IDs and spellings are consistent
# 4 Calculate biodiversity metrics

# 2019 data

library(readr)
pointfr_2019_sas <- read_csv("data/QHI_biodiversity/QHI_pointfr_2019_sas.csv")

str(pointfr_2019_sas)

# sorting plot ----

sort_QHI <- QHI_cover %>%
  #add_column(n="plot") %>%
  mutate(n = "plot",
         n2 = "year") %>%
 # unite("plot2" , c(n,plot)) %>%
  unite("year2", c(n2, year)) %>%
  group_by(sub_name, year2, plot) %>%
  #filter(name == "Arctagrostis latifolia" | name == "Dryas integrifolia" | name == "Eriophorum angustifolium") %>%
  filter(name == "Lupinus arcticus") %>%
  summarise(test = first(name),
            cover = first(cover))
  


ggplot(sort_QHI, aes(x=plot, y= cover, color= year2, shape = test)) +
       geom_point() +
  geom_line() +
  facet_wrap(.~sub_name) 

  
  


