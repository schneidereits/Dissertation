# Isla's code for one file
gr080119new_384 <- read.csv2("~/Desktop/Fieldspec/gr080119new_384.asc", header=FALSE, sep="")

View(gr080119new_384)

names(gr080119new_384) <- c("wavelength", "reference", "target", "reflectence")

ggplot(gr080119new_384, aes(x = wavelength, y = reflectence)) + 
  geom_point() + 
  theme_bw()


# Gergana's code for multiple files
list_of_files <- list.files(path = "data/2019/her", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

library(tidyverse)

# theme for graphs
theme_spectra <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, hjust = -2),
          legend.text = element_text(size = 12),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9), 
          legend.spacing.x = unit(0.3, 'cm'),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent", 
                                           size = 2, linetype = "blank"))
}


# Read all the files and create a FileName column to store filenames
her_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName")

names(her_df) <- c("id", "wavelength", "reference", "target", "reflectance")

# her_df_clean <- her_df[grep("[[:digit:]]", her_df$wavelenght), ]

her_df$wavelength2 <- parse_number(as.character(her_df$wavelength))/100
her_df$reflectance2 <- parse_number(as.character(her_df$reflectance))/100

her_df_clean <- her_df %>% drop_na(reflectance2) %>% 
  drop_na(wavelength2)

(her_all_graph <- ggplot(her_df_clean, aes(x = wavelength2, y = reflectance2, group = id)) + 
  geom_line(alpha = 0.2, colour = "#ffa544") + 
  theme_spectra() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n"))

ggsave(her_all_graph, filename = "figures/her_2019clean.png", 
       height = 6, width = 6)

list_of_files <- list.files(path = "data/2019/kom", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

kom_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName")

names(kom_df) <- c("id", "wavelength", "reference", "target", "reflectance")

kom_df$wavelength2 <- parse_number(as.character(kom_df$wavelength))/100
kom_df$reflectance2 <- parse_number(as.character(kom_df$reflectance))/1000

kom_df_clean <- kom_df %>% drop_na(reflectance2) %>% drop_na(wavelength2)

kom_test <- kom_df_clean %>% filter(reflectance2 < 100 & reflectance2 > 0)

(kom_all_graph <- ggplot(kom_test, aes(x = wavelength2, y = reflectance2, group = id)) + 
    geom_line(alpha = 0.2) + 
    theme_bw())

ggsave(kom_all_graph, filename = "figures/kom_2019clean.png", 
       height = 8, width = 8)


list_of_files <- list.files(path = "data/2019/ko1", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

kom1_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName")

names(kom1_df) <- c("id", "wavelength", "reference", "target", "reflectence")

(kom1_all_graph <- ggplot(kom1_df, aes(x = wavelength, y = reflectence, group = id)) + 
    geom_line(alpha = 0.2) + 
    theme_bw())

ggsave(kom1_all_graph, filename = "figures/kom1_2019.png", 
       height = 8, width = 8)

list_of_files <- list.files(path = "data/2019/he1", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

her1_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName")

names(her1_df) <- c("id", "wavelength", "reference", "target", "reflectence")

(her1_all_graph <- ggplot(her1_df, aes(x = wavelength, y = reflectence, group = id)) + 
    geom_line(alpha = 0.2) + 
    theme_bw())

ggsave(her1_all_graph, filename = "figures/her1_2019.png", 
       height = 8, width = 8)
