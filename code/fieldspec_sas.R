# QHI 2019 field spec 
# shawn schneidereit edit
# 5.3.2020

library(tidyverse)
library(cowplot)
library(lavaan)
library(smooth)
library(Hmisc)
library(vegan)
library(ape)
library(ggfortify)
library(cluster)
library(viridis)

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")



# Isla's code for one file ----
sp300 <- read.csv2("/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/gr080119_300.asc", header=FALSE, sep="")

names(sp163) <- c("wavelength", "reference", "target", "reflectence")

ggplot(sp163, aes(x = wavelength, y = reflectence)) + 
  geom_point() + 
  theme_bw()



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
          legend.position = c(0.95, 0.9), 
          legend.spacing.x = unit(0.3, 'cm'),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent", 
                                           size = 3, linetype = "blank"))
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

raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
  axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))


# time sorting ----
list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)
QHI_time <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec"),
         V3 = str_remove_all(V3, ",16.02.2018"),
         V3 = str_remove_all(V3, ",28.08.2016"),
         V3 = str_remove_all(V3, ",27.08.2016"),
         V3 = str_remove_all(V3, ",25.04.2017"),
         time = V4,
         time = as.factor(time)) %>%
  # filter only measurment times 
  filter( V1 == "time=")  #%>%  V4 = as.factor(V4))

unique(QHI_time$time)

#QHI_time <- QHI_time %>% mutate(time = case_when(time == "00:01:38") ~  (time=="24:01:38"))

#QHI <- QHI %>% mutate( V4 = case_when(grepl("00:0", V4))   ~ 
#                                              mutate(V4 = V4 + 24))

#QHI <- QHI %>% mutate( V4 = V4[365:366] + 24)

ggplot(QHI_time, aes(x=time, fill=V4)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(QHI_time, aes(x=time, y=V3, color=V4)) +
  geom_point(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## full QHI fieldspec ----

list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
full_QHI <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/"),
         # to extract id from file name
         id = stringr::str_extract(FileName, "_\\d*"),
         id = str_remove_all(id, "_"),
         id = gsub("(?<![0-9])0+", "", id, perl = TRUE))

# added id column
names(full_QHI) <- c("name", "wavelength", "reference", "target", "reflectance", "id")

# her_df_clean <- her_df[grep("[[:digit:]]", HE_LTP_6_df$wavelenght), ]

full_QHI$wavelength2 <- parse_number(as.character(full_QHI$wavelength))/100
full_QHI$reflectance2 <- parse_number(as.character(full_QHI$reflectance))/100

full_QHI <- full_QHI %>% drop_na(reflectance2) %>% 
  drop_na(wavelength2) %>%
  filter(between(wavelength2, 400, 985))

# for subset of data
(ggplot(subset(full_QHI ,id %in% c(384:320))) +
    aes(x = wavelength2, y = reflectance2, group = id, color = id)) + 
  geom_line(alpha = 0.9) + 
  scale_color_viridis_d(option = "D") +
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Reflectance\n") +
  geom_hline( yintercept= c(50,70), color = "red") +
  facet_wrap(.~id) 

spec_plot(full_QHI)


## all HE_LPT ----
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

############ test 1 349-384 

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

########## test 3  ----

list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/test_3/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
test_3 <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/test_3/"),
         id = stringr::str_extract(FileName, "_\\d*"),
         id = str_remove_all(id, "_"),
         id = gsub("(?<![0-9])0+", "", id, perl = TRUE),
         veg_type = case_when(grepl("HE|KO", FileName)   ~ 
                                stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*")))

str(test_3)
names(test_3) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "veg_type", "plot")

test_3 <- test_3 %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         veg_type = as.factor(veg_type)) %>%
  drop_na(reflectance) %>% 
  drop_na(wavelength) %>%
  drop_na(reference) %>%
  drop_na(target)  %>%
  filter(between(wavelength, 400, 985))

#### test 3 vis -------


# single wavelengths plot
(p_test_3 <-  ggplot(test_3, aes(x = wavelength, y = reflectance, group = id, color = veg_type)) + 
    geom_line(alpha = 0.3) + 
    theme_spectra() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
    theme(legend.position = "right") +
   # scale_color_viridis_d(option = "C") +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)

# single wavelengths plot
(p_test_3 <-  ggplot(test_3, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
   geom_line(alpha = 0.2) + 
   theme_spectra() +
   labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
   theme(legend.position = "right") +
   # scale_color_viridis_d(option = "C") +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)


test_3_small <- test_3 %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# violin of mean by vegetation type
ggplot(test_3_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  theme_cowplot()


# cloud of mean by vegetation type
(p_test_3 <- ggplot(test_3_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(position = position_jitter(width = .05), size = .8) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    theme_cowplot())

(p_test_4 <- ggplot() +
    geom_flat_violin(data = test_3_small, aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = test_3_small, aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = .8) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    theme_cowplot())

#ggsave(p_test_3, path = "figures", filename = "cloud_specmean.png", height = 8, width = 10)



# violin of cv by vegtation type
ggplot(test_3_small, aes(x=veg_type, y=CV, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  theme_cowplot()

# cloud of spec mean by VT
(p_test_3 <-ggplot(test_3_small, aes(x=veg_type, y=CV, fill=veg_type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(position = position_jitter(width = .05), size = .8) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    theme_cowplot())
# ggsave(p_test_3, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)

# group by wavelength 

test_3_small_wvlgth <- test_3 %>%
  group_by(veg_type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# GD advice: split full spec into regions (via background colors) and make seperate raincloud plot at each spec_region. (for full snazzyness add color of spec_region to backround)
#plot spectral mean
ggplot(test_3_small_wvlgth, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
  geom_line(alpha = 0.2) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "mean reflectance\n")


## SMOOTHING NOT CORRECT
ggplot(test_3_small_wvlgth, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "mean reflectance\n")

# plot CV

ggplot(test_3_small_wvlgth, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
  geom_line(alpha = 0.2) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "CV\n")

# SMOOTHING NOT CORRECT
ggplot(test_3_small_wvlgth, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "CV\n")


# test_3 PCA ----

pca <- test_3 %>%
          select(reflectance, wavelength)
pca <- prcomp(pca, scale = TRUE)

(p_pca <-autoplot(pca, loadings = TRUE, loadings.label = TRUE,
                  data = test_3, colour = 'veg_type', alpha = 0.5))
#ggsave(p_pca, path = "figures", filename = "pca_attempt.png", height = 8, width = 10)

(p_pca <-autoplot(pca, loadings = TRUE, loadings.label = TRUE,
                  data = test_3, colour = 'veg_type', alpha = 0.5))

(p_pca <- autoplot(pam(pca), frame = TRUE, frame.type = 'norm'))


########## QHI spec  ----

list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/fieldspec_sorted_sas/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
QHI <- list_of_files %>%
  set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/fieldspec_sorted_sas"),
         id = stringr::str_extract(FileName, "_\\d*"),
         id = str_remove_all(id, "_"),
         id = gsub("(?<![0-9])0+", "", id, perl = TRUE),
         veg_type = case_when(grepl("HE|KO", FileName)   ~ 
                                stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*")))

str(QHI)
names(QHI) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "veg_type", "plot")

QHI <- QHI %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         veg_type = as.factor(veg_type)) %>%
  drop_na(reflectance) %>% 
  drop_na(wavelength) %>%
  drop_na(reference) %>%
  drop_na(target)  %>%
  filter(between(wavelength, 400, 985),
         !id %in% c(148:171, 196, 210:211, 250:259))

#### QHI vis -------


# single wavelengths VT
(p_QHI <-  ggplot(QHI, aes(x = wavelength, y = reflectance, group = id, color = veg_type)) + 
   geom_line(alpha = 0.3) + 
   theme_spectra() +
   labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
   theme(legend.position = "right") +
   # scale_color_viridis_d(option = "C") +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)

# single wavelengths plot
(p_QHI <-  ggplot(QHI, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_spectra() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_viridis_d(option = "C") +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)


QHI_small <- QHI %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# for vis inspecting 
#QHI_small <- QHI %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))%>%
  filter(between(spec_mean, 25, 35) )

# violin of mean by vegetation type
ggplot(QHI_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  theme_cowplot()

# cloud of mean by vegetation type
(p_QHI <- ggplot(QHI_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(position = position_jitter(width = .05), size = .8) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    theme_cowplot())

(p_QHI <- ggplot() +
    geom_flat_violin(data = QHI_small, aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_small, aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = 3) +
    geom_boxplot(data = QHI_small, aes(x=veg_type, y=spec_mean, colour= veg_type),
                  width=0.2, fill="white", alpha = 0.3) +
    scale_color_brewer(palette = "Paired") +
    theme_cowplot())

#ggsave(p_test_3, path = "figures", filename = "cloud_specmean.png", height = 8, width = 10)



# violin of cv by vegtation type
ggplot(QHI_small, aes(x=veg_type, y=CV, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  theme_cowplot()

# cloud of spec mean by VT
(p_QHI <- ggplot() +
    geom_flat_violin(data = QHI_small, aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_small, aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .05), size = 3) +
    geom_boxplot(data = QHI_small, aes(x=veg_type, y=CV, colour= veg_type),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_brewer(palette = "Paired") +
    theme_cowplot())

# ggsave(p_test_3, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)

# group by wavelength 

QHI_small_wvlgth <- QHI %>%
  group_by(veg_type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

QHI_check <- QHI %>%
  group_by(veg_type, plot) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# GD advice: split full spec into regions (via background colors) and make seperate raincloud plot at each spec_region. (for full snazzyness add color of spec_region to backround)
#plot spectral mean
ggplot(QHI_small_wvlgth, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
  geom_line(alpha = 0.7, size=1.) + 
  theme_spectra() +
  theme(legend.position = "right") +
 
  labs(x = "\nWavelength (mm)", y = "mean reflectance\n")


## SMOOTHING NOT CORRECT
ggplot(QHI_small_wvlgth, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "mean reflectance\n")

# plot CV

ggplot(QHI_small_wvlgth, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
  geom_line(alpha = 0.2) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "CV\n")

# SMOOTHING NOT CORRECT
ggplot(QHI_small_wvlgth, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "CV\n")


# test_3 PCA ----

pca <- QHI %>%
  select(reflectance, wavelength)
pca <- prcomp(pca, scale = TRUE)

(p_pca <-autoplot(pca, loadings = TRUE, loadings.label = TRUE,
                  data = QHI, colour = 'veg_type', alpha = 0.5))
#ggsave(p_pca, path = "figures", filename = "pca_attempt.png", height = 8, width = 10)

(p_pca <-autoplot(pca, loadings = TRUE, loadings.label = TRUE,
                  data = QHI, colour = 'plot', alpha = 0.5))

(p_pca <- autoplot(pam(pca), frame = TRUE, frame.type = 'norm'))

