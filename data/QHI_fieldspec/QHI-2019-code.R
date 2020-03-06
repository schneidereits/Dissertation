# QHI 2019 field spec 
# shawn schneidereit edit
# 5.3.2020

library(tidyverse)

# time sorting ----
list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)
QHI <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec"),
         V3 = str_remove_all(V3, ",16.02.2018"),
         V3 = str_remove_all(V3, ",28.08.2016"),
         V3 = str_remove_all(V3, ",27.08.2016"),
         time = V4,
         time = as.factor(time)) %>%
  # filter only measurment times 
           filter( V1 == "time=") #%>%  V4 = as.factor(V4))

unique(QHI$V4)

QHI <- QHI %>% mutate(time = case_when(time == "00:01:38") ~  (time=="24:01:38"))

#QHI <- QHI %>% mutate( V4 = case_when(grepl("00:0", V4))   ~ 
#                                              mutate(V4 = V4 + 24))

#QHI <- QHI %>% mutate( V4 = V4[365:366] + 24)

ggplot(QHI, aes(x=time, fill=V4)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(QHI, aes(x=time, y=V3, color=V4)) +
  geom_point(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Isla's code for one file ----
sp300 <- read.csv2("/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/gr080119_300.asc", header=FALSE, sep="")

names(sp163) <- c("wavelength", "reference", "target", "reflectence")

ggplot(sp163, aes(x = wavelength, y = reflectence)) + 
  geom_point() + 
  theme_bw()

library(tidyverse)

# functions ----
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

# ploting spectral signatures 

spec_plot <- function(x){
  names(x) <- c("id", "wavelength", "reference", "target", "reflectance")
  
  # her_df_clean <- her_df[grep("[[:digit:]]", HE_LTP_6_df$wavelenght), ]
  
  x$wavelength2 <- parse_number(as.character(x$wavelength))/100
  x$reflectance2 <- parse_number(as.character(x$reflectance))/100
  
  x_clean <- x %>% drop_na(reflectance2) %>% 
    drop_na(wavelength2)
  
  (test_graph <- ggplot(x_clean, aes(x = wavelength2, y = reflectance2, group = id)) + 
      geom_line(alpha = 0.2, colour = "#ffa544") + 
      theme_spectra() +
      labs(x = "\nWavelength (mm)", y = "Reflectance\n"))
}

spec_fct_plot <- function(x){
  names(x) <- c("id", "wavelength", "reference", "target", "reflectance")
  
  # her_df_clean <- her_df[grep("[[:digit:]]", HE_LTP_6_df$wavelenght), ]
  
  x$wavelength2 <- parse_number(as.character(x$wavelength))/100
  x$reflectance2 <- parse_number(as.character(x$reflectance))/100
  
  x_clean <- x %>% drop_na(reflectance2) %>% 
    drop_na(wavelength2)
  
  (test_graph <- ggplot(x_clean, aes(x = wavelength2, y = reflectance2, group = id)) + 
      geom_line(alpha = 0.2, colour = "#ffa544") + 
      theme_spectra() +
      labs(x = "\nWavelength (mm)", y = "Reflectance\n")) +
    facet_wrap(.~id)
}

ref_fct_plot <- function(x){
  names(x) <- c("id", "wavelength", "reference", "target", "reflectance")
  
  # her_df_clean <- her_df[grep("[[:digit:]]", HE_LTP_6_df$wavelenght), ]
  
  x$wavelength2 <- parse_number(as.character(x$wavelength))/100
  x$reference2 <- parse_number(as.character(x$reference))/100
  
  x_clean <- x %>% drop_na(reference2) %>% 
    drop_na(wavelength2)
  
  (test_graph <- ggplot(x_clean, aes(x = wavelength2, y = reference2, group = id)) + 
      geom_line(alpha = 0.2, colour = "#ffa544") + 
      theme_spectra() +
      labs(x = "\nWavelength (mm)", y = "Reference\n")) +
    facet_wrap(.~id)
}

target_fct_plot <- function(x){
  names(x) <- c("id", "wavelength", "reference", "target", "reflectance")
  
  # her_df_clean <- her_df[grep("[[:digit:]]", HE_LTP_6_df$wavelenght), ]
  
  x$wavelength2 <- parse_number(as.character(x$wavelength))/100
  x$target2 <- parse_number(as.character(x$target))/100
  
  x_clean <- x %>% drop_na(target2) %>% 
    drop_na(wavelength2)
  
  (test_graph <- ggplot(x_clean, aes(x = wavelength2, y = target2, group = id)) + 
      geom_line(alpha = 0.2, colour = "#ffa544") + 
      theme_spectra() +
      labs(x = "\nWavelength (mm)", y = "target\n")) +
    facet_wrap(.~id)
}

## all HE_LPT ----

# Gergana's code for multiple files
list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/HE_LTP/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
HE_LTP_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort"))


spec_plot(HE_LTP_df)
spec_fct_plot(HE_LTP_df)


## baby KO+HE ----
# Gergana's code for multiple files
list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/baby/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
baby_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName")%>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort"))


spec_plot(baby_df)
spec_fct_plot(baby_df)

# KO_LTP_4 ----

list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/KO_LTP/", recursive = TRUE,
                           pattern = "\\.asc$", 
                           full.names = TRUE)

# Read all the files and create a FileName column to store filenames
KO_LTP_df <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort"))


spec_plot(KO_LTP_df)
spec_fct_plot(KO_LTP_df)

## test ----

############ 349-384 

# Gergana's code for multiple files
list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/test/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
test <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
    mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort"))

(p_test <- spec_plot(test))
#ggsave(p_test,path = "figures", filename = "QHI_384-360.png", height = 6, width = 8)

(p_test <- spec_fct_plot(test))
#ggsave(p_test, path = "figures", filename = "QHI_fct384-360.png", height = 8, width = 10)

(p_test <- ref_fct_plot(test))
#ggsave(p_test, path = "figures", filename = "QHI_ref384-360.png", height = 8, width = 10)

(p_test <- target_fct_plot(test))
#ggsave(p_test, path = "figures", filename = "QHI_target384-360.png", height = 8, width = 10)


########### test 2 142-175 ------
  
  # Gergana's code for multiple files
  list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/test_2/", recursive = TRUE,
                              pattern = "\\.asc$", 
                              full.names = TRUE)
  
  # Read all the files and create a FileName column to store filenames
  test_2 <- list_of_files %>%
    set_names(.) %>%
    map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
    mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort"))
  
  spec_plot(test_2)
  spec_fct_plot(test_2)
  ref_fct_plot(test_2)
  
  #ggsave(p_test_2, path = "figures", filename = "QHI_142-175.png", height = 8, width = 10)
  
########## test 3 246-275 ----
  
  list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/test_3/", recursive = TRUE,
                              pattern = "\\.asc$", 
                              full.names = TRUE)
  
  # Read all the files and create a FileName column to store filenames
  test_3 <- list_of_files %>%
    set_names(.) %>%
    map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
    mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort"))

(p_test_3 <- spec_plot(test_3))
#ggsave(p_test_3, path = "figures", filename = "QHI_246-275.png", height = 8, width = 10)
  
(p_test_3 <- spec_fct_plot(test_3))
#ggsave(p_test_3, path = "figures", filename = "QHI_fct246-275.png", height = 8, width = 10)
  
(p_test_3 <- ref_fct_plot(test_3))
#ggsave(p_test_3, path = "figures", filename = "QHI_ref246-275.png", height = 8, width = 10)
  
(p_test_3 <- target_fct_plot(test_3))
#ggsave(p_test_3, path = "figures", filename = "QHI_target246-275.png", height = 8, width = 10)
  
  
  