# coding club data viz 
# 11.4.2020

# Purpose of the script
# Your name, date and email

# Libraries ----
# if you haven't installed them before, run the code install.packages("package_name")
library(tidyverse)
library(ggthemes)  # for a mapping theme
library(ggalt)  # for custom map projections
library(ggrepel)  # for annotations
library(viridis)  # for nice colours

# Data ----
# Load data - site coordinates and plant records from
# the Long Term Ecological Research Network
# https://lternet.edu and the Niwot Ridge site more specifically
lter <- read.csv("lter.csv")
niwot_plant_exp <- read.csv("niwot_plant_exp.csv")


# MAPS ----
# Get the shape of North America
north_america <- map_data("world", region = c("USA", "Canada"))

# Exclude Hawaii if you want to
north_america <- north_america[!(north_america$subregion %in% "Hawaii"),]

# A very basic map
(lter_map1 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    # Add points for the site locations
    geom_point(data = lter, 
               aes(x = long, y = lat)))

# You can ignore this warning message, it's cause we have forced
# specific lat and long columns onto geom_map()
# Warning: Ignoring unknown aesthetics: x, y

# if you wanted to save this (not amazing) map
# you can use ggsave()
ggsave(lter_map1, filename = "map1.png",
       height = 5, width = 8)  # the units by default are in inches

# the map will be saved in your working directory
# if you have forgotten where that is, use this code to find out
getwd()

(lter_map2 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               # when you set the fill or colour to vary depending on a variable
               # you put that (e.g., fill = ele) inside the aes() call
               # when you want to set a specific colour (e.g., colour = "grey30"),
               # that goes outside of the aes() call
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))

ggsave(lter_map2, filename = "map2.png",
       height = 5, width = 8)

(lter_map3 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    # you can change the projection here
    # coord_proj("+proj=wintri") +
    # the wintri one above is good for the whole world, the one below for just North America
    coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                      " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))

# You don't need to worry about the warning messages
# that's just cause we've overwritten the default projection

ggsave(lter_map3, filename = "map3.png",
       height = 5, width = 8)

(lter_map4 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                      " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
               # zooming in by setting specific coordinates
               ylim = c(25, 80), xlim = c(-175, -50)) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))

ggsave(lter_map4, filename = "map4.png",
       height = 5, width = 8)

# DISTRIBUTIONS ----
# Setting a custom ggplot2 function
# This function makes a pretty ggplot theme
# This function takes no arguments 
# meaning that you always have just niwot_theme() and not niwot_theme(something else here)

theme_niwot <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),          
          legend.title = element_blank(),                              
          legend.position = c(0.95, 0.15), 
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent", 
                                           size = 2, linetype = "blank"))
}



# We will use a function by Ben Marwick
# This code loads the function in the working environment
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


# Calculate species richness per plot per year
niwot_richness <- niwot_plant_exp %>% group_by(plot_num, year) %>%
  mutate(richness = length(unique(USDA_Scientific_Name))) %>% ungroup()

# Now we can make the plot!
(distributions5 <- 
    ggplot(data = niwot_richness, 
           aes(x = reorder(fert, desc(richness)), y = richness, fill = fert)) +
    # The half violins
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    # The points
    geom_point(aes(y = richness, color = fert), 
               position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
    # The boxplots
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    # \n adds a new line which creates some space between the axis and axis title
    labs(y = "Species richness\n", x = NULL) +
    # Removing legends
    guides(fill = FALSE, color = FALSE) +
    # Setting the limits of the y axis
    scale_y_continuous(limits = c(0, 30)) +
    # Picking nicer colours
    scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
    theme_niwot())


(distributions6 <- 
    ggplot(data = niwot_richness, 
           aes(x = reorder(fert, desc(richness)), y = richness, fill = fert)) +
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    geom_point(aes(y = richness, color = fert), 
               position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    labs(y = "\nSpecies richness", x = NULL) +
    guides(fill = FALSE, color = FALSE) +
    scale_y_continuous(limits = c(0, 30)) +
    scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
    coord_flip() +
    theme_niwot())

ggsave(distributions6, filename = "distributions6.png",
       height = 5, width = 5)
