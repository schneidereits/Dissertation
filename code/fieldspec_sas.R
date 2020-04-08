# QHI 2019 field spec 
# shawn schneidereit edit
# 5.3.2020

library(tidyverse)
library(cowplot) # probaly can remove?
library(lavaan) 
library(smooth)
library(ggpubr) # for panel plot
library(Hmisc)
library(vegan) # pca (maybe remove)
library(ape) # pca (maybe remove)
library(ggfortify) # pca (maybe remove)
library(cluster) # pca (maybe remove)
library(viridis) # color pallet
library(grid) # for pannel plot
library(FactoMineR) # for pca
library(factoextra) # for pca visulizaiton
library(ggpmisc) # for ISI minima visulizaiton
library(RColorBrewer)


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

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



theme_rgb_mean <- list( annotate("rect", xmin = 400, xmax = 500, ymin = 0.5,
                                 ymax = 100, alpha = .15, fill = "blue"),
                        annotate("rect", xmin = 500, xmax = 600, ymin = 0.5, 
                                 ymax = 100, alpha = .15, fill = "green"), 
                        annotate("rect", xmin = 600, xmax = 680, ymin = 0.5, 
                                 ymax = 100, alpha = .15, fill = "red"), 
                        annotate("rect", xmin = 680, xmax = 800, ymin = 0.5, 
                                 ymax = 100, alpha = .15, fill = "tomato"),
                        annotate("rect", xmin = 800, xmax = 985, ymin = 0.5, 
                                 ymax = 100, alpha = .15, fill = "darkgrey"),
                        scale_y_continuous(expand = expand_scale(mult = c(0, .1))),
                        scale_x_continuous(expand = expand_scale(mult = c(0, .1))))

theme_rgb_CV <- list(annotate("rect", xmin = 400, xmax = 500, ymin = 0, ymax = 0.5, 
                          alpha = .15, fill = "blue"),
                     annotate("rect", xmin = 500, xmax = 600, ymin = 0, ymax = 0.5, 
                          alpha = .15, fill = "green"),
                     annotate("rect", xmin = 600, xmax = 680, ymin = 0, ymax = 0.5, 
                          alpha = .15, fill = "red"),
                     annotate("rect", xmin = 680, xmax = 800, ymin = 0, ymax = 0.5, 
                          alpha = .15, fill = "tomato"),
                     annotate("rect", xmin = 800, xmax = 985, ymin = 0, ymax = 0.5, 
                         alpha = .15, fill = "darkgrey"))
  
scale_color_QHI <- list(scale_color_manual(values = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF", "#A8A8A8")))
scale_color_collison <- list(scale_color_manual(values = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF")))

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

# finding extrema; source: https://github.com/stas-g/findPeaks
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}


# time sorting ----
list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)


QHI_time <- list_of_files %>%
  purrr::set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/exctracted_Fieldspec"),
        # remove occational date in V3 (time)
         V3 = str_remove_all(V3, ",16.02.2018"),
         V3 = str_remove_all(V3, ",28.08.2016"),
         V3 = str_remove_all(V3, ",27.08.2016"),
         V3 = str_remove_all(V3, ",25.04.2017"),
         time = V4,
         time = as.factor(time)) %>%
  # filter only measurment times 
  filter( V1 == "time=")  #%>%  V4 = as.factor(V4))

unique(QHI_time$time)

# attempt to change 00: to 24: to allow for time to be linear and more interpretable 
#QHI_time <- QHI_time %>% mutate(time = case_when(time == "00:01:38") ~  (time=="24:01:38"))

#QHI <- QHI %>% mutate( V4 = case_when(grepl("00:0", V4))   ~ 
#                                              mutate(V4 = V4 + 24))

#QHI <- QHI %>% mutate( V4 = V4[365:366] + 24)

ggplot(QHI_time, aes(x=time, fill=V4)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# full QHI fieldspec ----

# data manipulation  ----

list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/fieldspec_sorted_sas/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
QHI <- list_of_files %>%
  purrr::set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/fieldspec_sorted_sas//gr080119"),
         id = stringr::str_extract(FileName, "_\\d*"),
         id = str_remove_all(id, "_"),
         id = gsub("(?<![0-9])0+", "", id, perl = TRUE),
         veg_type = case_when(grepl("HE|KO", FileName)   ~ 
                                stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO|PS2", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*|PS2")))

names(QHI) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "veg_type", "plot")

QHI <- QHI %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         veg_type = as.factor(veg_type),
         veg_type =  fct_explicit_na(veg_type, na_level = "mixed"),
         year = "2019") %>%
  drop_na(reflectance) %>% 
  drop_na(wavelength) %>%
  drop_na(reference) %>%
  drop_na(target)  %>%
  # filter range of device measurment accuracy
  filter(between(wavelength, 400, 985),
         # filtering out device based measurment errors
         !id %in% c(148:171, 196, 210:211, 250:259))

# which id hace reflectance>100
QHI %>% filter(reflectance>100) %>%
  count(id)

#filter(wavelength %in% ISI_band_selection$wavelength) %>%

# remove measuments with reflectance values over 100
QHI <- QHI %>%
  filter(!id %in% c(100, 104, 106, 147, 172, 207, 208))

# group by id
QHI_small <- QHI %>%
  group_by(year, veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

collison <- QHI %>%
  filter(veg_type != "mixed")

PS2 <- QHI %>%
  filter(veg_type == "mixed")

##### correction factros for 250-259 reflectance. but to small. need > 3. also dont know how to apply to refletance 
#HE1_corr <- full_QHI %>%
#filter(id %in% c("249", "250")) %>%
#  group_by(wavelength) %>%
#  mutate(reference = parse_number(as.character(reference))/100,
#         reflectance = parse_number(as.character(reflectance))/100,
#         correction_factor = (reference[id == "250"] / reference[id == "249"]),
#         reflectance = reflectance/correction_factor)


# spectral data 2018 #NOTE: THAT THESE ARE GROUPED BY PLOT

spec_2018 <- read_csv("data/spec_bio.csv")

spec_2018 <- spec_2018 %>%
  select(site, plot, Reflectance_mean, Reflectance_sd, Reflectance_cv, Wavelength) %>%
  mutate(year = "2018",
         veg_type = site,
         # multiple to match reflectance of QHI 
         Reflectance_mean = Reflectance_mean*100) %>%
 # rename(spec_mean = Reflectance_mean,
     #    spec_SD = Reflectance_sd,
      #   CV = Reflectance_cv) %>%
  # to correspond with QHI plot formate
  unite("plot", c(site, plot), sep = "") %>%
  ungroup() %>%
  group_by(year, veg_type, plot) %>%
  # SUMARIZE BY PLOT? DOES NOT CORRESPOND WITH QHI FORMATE WHICH IS AT MEASURMENT LEVEL
  # had to add na.rm=TRUE, although there were no NAs in the data 
  summarise(spec_mean = mean(Reflectance_mean, na.rm=TRUE),
            spec_SD = mean(Reflectance_sd, na.rm=TRUE),
            CV = mean(Reflectance_cv, na.rm=TRUE))
 

histogram(spec_2018$spec_mean)
histogram(QHI_small$spec_mean)

# binding 2018 & 2019
QHI_2018_2019 <- bind_rows(QHI_small, spec_2018)

# adding a unique plot id 
QHI_2018_2019 <- QHI_2018_2019 %>%
  ungroup() %>%
  # temporary colunm with only plot number 
  mutate(plot2 = str_remove_all(plot, "HE|KO"),
         plot_unique = paste(veg_type,plot2,year,sep="_")) %>%
  select(-plot2)



# violin of mean by year
ggplot(QHI_2018_2019, aes(x=year, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5, aes(fill = veg_type)) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()



# testing 2018 spectral raw

spectra_040818 <- read_csv("~/Downloads/spectra_040818.csv",
                         col_types = cols(X1 = col_skip()))
head(spectra_040818)
unique(spectra_040818$type)
unique(spectra_040818$site)
unique(spectra_040818$plot)
unique(spectra_040818$method)
unique(spectra_040818$measurement)

spectra_040818 %>% count(type) # looks like target is what im after

spec_2018 <- spectra_040818 %>%
  # filter range of device measurment accuracy
  filter(between(Wavelength, 400, 985),
         site == "Herschel" |  site == "komukuk",
         type == "target",
         method == "point",
         !plot== "H20",
         # no NAs in data, but if not filtered unique(spec_2018$measurement) aswell as plot indicate there are NAs...
         !measurement == "NA") %>%
  mutate(Reflectance = Reflectance*100,
         plot = str_remove_all(plot, "LT"),
         year = "2018",
         veg_type = case_when(site == "Herschel" ~ "HE",
                              site == "komukuk" ~ "KO")) %>%
         # add id by groups
         mutate(id = as.character(group_indices(., veg_type, plot, measurement) + 385)) %>% # 384 IDs for 2019 spectral data
  unite(plot, c(veg_type, plot), sep = "", remove = FALSE)
  


# summarized by measurment spec 2018 data 
spec_2018_small <- spec_2018 %>%
  group_by(year, veg_type, plot, id) %>%
  summarise(spec_mean = mean(Reflectance),
            spec_SD = sd(Reflectance),
            CV = mean(sd(Reflectance)/mean(Reflectance)))

# binding 2018 & 2019 spec data


QHI_2018_2019 <- bind_rows(QHI_small, spec_2018_small)

# adding a unique plot id 
QHI_2018_2019 <- QHI_2018_2019 %>%
  ungroup() %>%
  # temporary colunm with only plot number 
  mutate(plot2 = str_remove_all(plot, "HE|KO"),
         plot_unique = paste(veg_type,plot2,year,sep="_")) %>%
  select(-plot2)


ggplot(spec_2018, aes(x = Wavelength, y = Reflectance, group = id, color = veg_type)) + 
    geom_line(alpha = 0.3) + 
    theme_spectra() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_viridis_d(option = "C") +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    guides(colour = guide_legend(override.aes = list(size=5)))



ggplot(spec_2018_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

ggplot(spec_2018_small, aes(x=veg_type, y=CV, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

#  band selection ----

# supervised band selection

supervised_band_selection <- tibble(wavelength = 
                                      # need to sequence by 0.01 to subsequently filter wavelengths
                                      c(seq(430, 450, by = 0.01), # Chlorophyll & carotenoid absorption
                                        seq(660, 680, by = 0.01), # Max absorption of chlorophyll
                                        seq(700, 725, by = 0.01), # Middle of red-edge transition
                                        seq(745, 755, by = 0.01), # End of red-edge transition
                                        seq(920, 985, by = 0.01)))# Vascular plant structures & H20 

supervised_band_selection <- collison %>%
  filter(wavelength %in% supervised_band_selection$wavelength) %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))
 

# ISI band selection and SZU 

collison_ISI <- collison %>%
  filter(veg_type %in% c("KO" , "HE")) %>%
  group_by(wavelength) %>%
  summarise(ISI = (1.96*(mean(reflectance[veg_type=="HE"]) + mean(reflectance[veg_type=="KO"])))/
                        abs(sd(reflectance[veg_type=="HE"] - sd(reflectance[veg_type=="KO"])))) 

ISI_band_selection <- collison_ISI %>%
    mutate(n = row_number()) %>%
     # filter wavelengths that are local ISI minima; (-) is to denote minima
    filter(n %in% find_peaks(-collison_ISI$ISI))          
 

SZU <- collison_ISI %>%
  arrange(ISI) %>%
  mutate(ISI = as.numeric(ISI),
    n = row_number(),
    d_ISI = (lead(ISI)/ISI - 1) *100,
         # 0.015 is the trade-off value (q)
         d_qi = (0.015- d_ISI),
    # number of bands is equal to max DISIi 
         D_ISIi = cumsum(d_qi))

# the selcted wavelengths according to SZU
head(SZU, n=5)

# reduced dimentionality; product of ISI band selection
lowD <- collison %>%
  filter(wavelength %in% ISI_band_selection$wavelength) %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# need to spruce up https://www.rdocumentation.org/packages/ggpmisc/versions/0.3.3/topics/stat_peaks
# plot of ISI by wavelength and local minima
(p_ISI <-  ggplot(collison_ISI, aes(x=wavelength, y=ISI)) +
  geom_line() +
  theme_cowplot() +
  stat_valleys(span = 3, shape = 1, size = 2, color = "black", fill = NA) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  scale_x_continuous(expand = expand_scale(mult = c(0, .1))) +
  annotate("rect", xmin = 400, xmax = 500, ymin = 16,
           ymax = 23, alpha = .15, fill = "blue") + 
  annotate("rect", xmin = 500, xmax = 600, ymin = 16, 
         ymax = 23, alpha = .15, fill = "green") +
  annotate("rect", xmin = 600, xmax = 680, ymin = 16, 
         ymax = 23, alpha = .15, fill = "red") + 
  annotate("rect", xmin = 680, xmax = 800, ymin = 16, 
         ymax = 23, alpha = .15, fill = "tomato") +
  annotate("rect", xmin = 800, xmax = 985, ymin = 16, 
         ymax = 23, alpha = .15, fill = "darkgrey"))

#ggsave(p_ISI, path = "figures", filename = "ISI_by_wavelength.png", height = 10, width = 12)
  
# plot of trends in accumulated D_ISIi values
(p_SZU <- ggplot(SZU, aes(x=n, y=D_ISIi)) +
  geom_line() +
  theme_cowplot())

#ggsave(p_SZU, path = "figures", filename = "SZU.png", height = 10, width = 12)


#  QHI vis -------

# single wavelengths VT
(p_QHI <-  ggplot(QHI, aes(x = wavelength, y = reflectance, group = id, color = veg_type)) + 
   geom_line(alpha = 0.3) + 
   theme_spectra() +
   labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
   theme(legend.position = "right") +
   scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
   # scale_color_viridis_d(option = "C") +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)

# single wavelengths plot
(p_QHI <-  ggplot(QHI, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_spectra() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_QHI +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)

# for subsets of measurements 
(ggplot(subset(QHI ,id %in% c(100, 104, 106, 147, 172, 207, 208))) +
    aes(x = wavelength, y = reflectance, group = id, color = id)) + 
  geom_line(alpha = 0.9) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Reflectance\n") #+
  geom_hline( yintercept= c(50,70), color = "red") #+
facet_wrap(.~id) 


# violin of mean by year
ggplot(QHI_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5, aes(fill = veg_type)) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# cloud of mean by vegetation type

(p_QHI <- ggplot(QHI_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_small, aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = 2) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_QHI +
    theme_cowplot())
#ggsave(p_QHI, path = "figures", filename = "cloud_specmean.png", height = 8, width = 10)


# violin of cv by vegtation type
ggplot(QHI_small, aes(x=veg_type, y=CV, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# cloud of spec mean by VT
(p_QHI <- ggplot() +
    geom_flat_violin(data = QHI_small, aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_small, aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(data = QHI_small, aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_QHI +
    theme_cowplot())
# ggsave(p_QHI, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)


# group by wavelength 
QHI_wavelength <- QHI %>%
  group_by(veg_type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))


# GD advice: split full spec into regions (via background colors) and make seperate raincloud plot at each spec_region. (for full snazzyness add color of spec_region to backround)
#plot spectral mean
(p_QHI <- ggplot(QHI_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_color_brewer(palette = "Paired") +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n") +
    theme_spectra()+
    theme(legend.position = "right") +
    theme_rgb_mean)


#ggsave(p_QHI, path = "figures", filename = "spec_sig_plot.png", height = 8, width = 10)

## SMOOTHING NOT CORRECT
ggplot(QHI_wavelength, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n") +
  theme_rgb_mean

# plot CV

ggplot(QHI_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
  geom_line(alpha = 0.9) + 
  theme_spectra() +
  theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(x = "\nWavelength (mm)", y = "CV\n") + 
  scale_color_QHI +
  theme_rgb_CV

#ggsave(p_QHI, path = "figures", filename = "CV_plot.png", height = 8, width = 10)

# SMOOTHING NOT CORRECT
ggplot(QHI_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "CV\n") +
  scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_rgb_CV

#  collison vis -------

# single wavelengths VT
(p_collison <-  ggplot(collison, aes(x = wavelength, y = reflectance, group = id, color = veg_type)) + 
   geom_line(alpha = 0.3) + 
   theme_spectra() +
   labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
   theme(legend.position = "right") +
   # scale_color_viridis_d(option = "C") +
   scale_color_manual(values = c("#ffa544", "#2b299b")) +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_collison, path = "figures", filename = "spec_sig_collison.png", height = 10, width = 12)

# single wavelengths plot
(p_collison <-  ggplot(collison, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_spectra() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_collison +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig_collison.png", height = 8, width = 10)

# group by id
collison_small <- collison %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# group by wavelength grouping
collison_wavelength <- collison %>%
  group_by(veg_type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# violin of mean by vegetation type
ggplot(collison_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# cloud of mean by vegetation type

(p_collison <- ggplot(collison_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = collison_small, aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = 2) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_collison +
    theme_cowplot())
#ggsave(p_collison, path = "figures", filename = "cloud_specmean_collison.png", height = 8, width = 10)


# violin of cv by vegtation type
ggplot(collison_small, aes(x=veg_type, y=CV, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# cloud of spec mean by VT
(p_collison <- ggplot() +
    geom_flat_violin(data = collison_small, aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = collison_small, aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .05), size = 2) +
    geom_boxplot(data = collison_small, aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_collison +
    theme_cowplot())
# ggsave(p_collison, path = "figures", filename = "cloud_CV_collison.png", height = 8, width = 10)

# plots mean reflectance 
(p_col_mean <- ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_color_brewer(palette = "Paired") +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_spectra() +
    theme(legend.position = "bottom") +
    scale_color_collison +
    theme_rgb_mean)
#ggsave(p_col_mean, path = "figures", filename = "spec_sig_plot_collison.png", height = 8, width = 10)

##  smoothed plots mean reflectance SMOOTHING NOT CORRECT
ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")

# plot CV

(p_col_CV <- ggplot(collison_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_color_brewer(palette = "Paired") +
    labs(x = "Wavelength (mm)", y = "CV") +
    theme_spectra() +
    theme(legend.position = "bottom") +
    scale_color_collison +
    theme_rgb_CV)

#ggsave(p_col_CV, path = "figures", filename = "CV_plot_collison.png", height = 8, width = 10)

# SMOOTHING NOT CORRECT
ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "CV\n")

#  collison facet plot ----
#  spectral mean and CV violin plots by region ----

collison_wavelength <- collison %>%
  group_by(veg_type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance))) %>%
  mutate(region = case_when(between(wavelength, 400, 500) ~ "blue",
                            between(wavelength, 500, 600) ~ "green",
                            between(wavelength, 600, 680) ~ "red",
                            between(wavelength, 680, 800) ~ "NIR",
                            between(wavelength, 800, 1000) ~ "IR"))

# blue mean
(p_blue_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("blue")), 
                     aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
      geom_point(data = subset(collison_wavelength, region %in% c("blue")),
               aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = .5) +
     geom_boxplot(data = subset(collison_wavelength, region %in% c("blue")),
                 aes(x=veg_type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    scale_color_collison +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "#cfe2fd")))



# blue CV
(p_blue_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("blue")), 
                     aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("blue")),
               aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .05), size = .5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("blue")),
                 aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "#cfe2fd")))

# green mean
(p_green_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("green")), 
                     aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("green")),
               aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = .5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("green")),
                 aes(x=veg_type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "lightgreen")))


# green CV
(p_green_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("green")), 
                     aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("green")),
               aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .05), size = .5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("green")),
                 aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "lightgreen")))

# red mean
(p_red_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("red")), 
                     aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("red")),
               aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = .5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("red")),
                 aes(x=veg_type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "red")))


# red CV
(p_red_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("red")), 
                     aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("red")),
               aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .05), size = .5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("red")),
                 aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "red")))

# NIR mean
(p_NIR_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("NIR")), 
                     aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("NIR")),
               aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("NIR")),
                 aes(x=veg_type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "tomato")))


# NIR CV
(p_NIR_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("NIR")), 
                     aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("NIR")),
               aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("NIR")),
                 aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "tomato")))

# IR mean
(p_IR_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("IR")), 
                     aes(x=veg_type, y=spec_mean, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("IR")),
               aes(x=veg_type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("IR")),
                 aes(x=veg_type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "darkgrey")))


# IR CV
(p_IR_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("IR")), 
                     aes(x=veg_type, y=CV, fill=veg_type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("IR")),
               aes(x=veg_type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("IR")),
                 aes(x=veg_type, y=CV),
                 width=0.2, fill="white", alpha = 0.3) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "darkgrey")))


#  facet plot for spectral mean and cv ----

# STILL NEED TO ADD AVERAGE OF ENTIRE VEGETATION TYPE 

# Move to a new page
grid.newpage()

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 5)))

# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
  
} 


# Arrange the plots
print(p_col_mean, vp = define_region(row = 1:2, col = 1:5))   # Span over two columns
print(p_blue_mean + rremove("legend") + rremove("xlab"), vp = define_region(row = 3, col = 1))
print(p_green_mean + rremove("legend") + rremove("xylab"), vp = define_region(row = 3, col = 2))
print(p_red_mean + rremove("legend") +  rremove("xylab"), vp = define_region(row = 3, col = 3))
print(p_NIR_mean + rremove("legend") + rremove("xylab"), vp = define_region(row = 3, col = 4))
print(p_IR_mean + rremove("legend")  + rremove("xylab"), vp = define_region(row = 3, col = 5))


# facet plot for spectral CV

# Move to a new page
grid.newpage()

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 5)))

# arranging plots
print(p_col_CV, vp = define_region(row = 1:2, col = 1:5))
print(p_blue_CV + rremove("legend") + rremove("xlab"), vp = define_region(row = 3, col = 1))
print(p_green_CV + rremove("legend") + rremove("xylab"), vp = define_region(row = 3, col = 2))
print(p_red_CV + rremove("legend") +  rremove("xylab"), vp = define_region(row = 3, col = 3))
print(p_NIR_CV + rremove("legend") + rremove("xylab"), vp = define_region(row = 3, col = 4))
print(p_IR_CV + rremove("legend")  + rremove("xylab"), vp = define_region(row = 3, col = 5))


#  H1 model----

# spectral mean
 
# histogram 

library(lme4)
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(glmmTMB) # to visualise model predictions
library(dotwhisker) # to visulaise effect whiskerplots

collison_small <- collison %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance))) #%>%
  mutate(spec_mean = scale(spec_mean, center = TRUE, scale = TRUE))


(hist <- ggplot(collison_small, aes(x = spec_mean)) +
   geom_histogram() +
   theme_classic())

# linear model for H1
m_H1a <- glm(data = collison_small, spec_mean ~ veg_type + plot)

m_H1a <- lmer(data = collison_small, spec_mean ~ veg_type + (1|plot))


summary(m_H1a)

plot(m_H1a)
qqnorm(resid(m_H1a))
qqline(resid(m_H1a))  # points fall nicely onto the line - good!

# Visualises random effects 
(re.effects <- plot_model(m_H1a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H1a, show.values = TRUE))


ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")


# CV

(hist <- ggplot(collison_small, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

m_H1b <- lmer(data = collison_small, CV ~ veg_type + (1|plot))

summary(m_H1b)

plot(m_H1b)
qqnorm(resid(m_H1b))
qqline(resid(m_H1b)) 

# Visualises random effects 
(re.effects <- plot_model(m_H1b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H1b, show.values = TRUE))

ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")




collison_small_VT <- collison %>%
  group_by(veg_type,wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

ggplot(collison, aes(x = wavelength, y = reflectance, group = veg_type, color = veg_type)) + 
    stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE ) +
    geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_spectra() +
    theme(legend.position = "bottom")

# model H3 (band selection) ----

# spectral mean

(hist <- ggplot(lowD, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3a <- glm(data = lowD, spec_mean ~ veg_type + plot)

m_H3a <- lmer(data = lowD, spec_mean ~ veg_type + (1|plot))

summary(m_H3a)

plot(m_H3a)
qqnorm(resid(m_H3a))
qqline(resid(m_H3a)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3a, show.values = TRUE))

ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")

# CV

(hist <- ggplot(lowD, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3b <- glm(data = lowD, CV ~ veg_type + plot)

m_H3b <- lmer(data = lowD, CV ~ veg_type + (1|plot))

summary(m_H3b)

plot(m_H3b)
qqnorm(resid(m_H3b))
qqline(resid(m_H3b)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3b, show.values = TRUE))

ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")


# models with supervised band selection for dimention reduction 

# spectral mean

(hist <- ggplot(supervised_band_selection, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3c <- glm(data = supervised_band_selection, spec_mean ~ veg_type + plot)

m_H3c <- lmer(data = supervised_band_selection, spec_mean ~ veg_type + (1|plot))

summary(m_H3c)

plot(m_H3c)
qqnorm(resid(m_H3c))
qqline(resid(m_H3c)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3c, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3c, show.values = TRUE))

ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")

# CV

(hist <- ggplot(supervised_band_selection, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3d <- glm(data = supervised_band_selection, CV ~ veg_type + plot)

m_H3d <- lmer(data = supervised_band_selection, CV ~ veg_type + (1|plot))

summary(m_H3d)

plot(m_H3d)
qqnorm(resid(m_H3d))
qqline(resid(m_H3d)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3d, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3d, show.values = TRUE))

ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")


# combined model vis

library(broom)

# spectral mean

dwplot(list(m_H1a, m_H3a, m_H3c), 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
  theme_spectra() +
  theme(legend.position = c(0.8, 0.5),
        legend.justification = c(0, 0),
        legend.title.align = .5)

# CV

# effect sizes and error dont seem to correspond with model summary...
dwplot(list(m_H1b, m_H3b, m_H3d), 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
  theme_spectra() +
  theme(legend.position = c(0.8, 0.8),
        legend.justification = c(0, 0),
        legend.title.align = .5)

# If needed I could use ggpredict to creat boxplot of predicted spec_mean and cv by VT by Model (but might not be compatable with lme4)
# https://strengejacke.github.io/ggeffects/reference/ggpredict.html



dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
ggplot(dat, aes(x, predicted, colour = group)) +
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  ) +
  scale_x_discrete(breaks = 1:3, labels = get_x_labels(dat))



#  QHI PCA ----

# for all QHI measurements 
pca <- QHI_small 

# simple pca
pca <- prcomp(pca[,4:5], scale = TRUE, center = TRUE) # for adding number of ranks (rank. = 4)

summary(pca)

(p_pca <-autoplot(pca, loadings = TRUE, loadings.label = TRUE,
                  data = QHI_small, colour = 'veg_type', alpha = 0.5))
#ggsave(p_pca, path = "figures", filename = "pca_attempt.png", height = 8, width = 10)

(p_pca <-autoplot(pca, loadings = TRUE, loadings.label = TRUE,
                  data = QHI, colour = 'plot', alpha = 0.5))

ggbiplot(pca, ellipse=TRUE,  labels=rownames(QHI), groups=QHI)

(p_pca <- autoplot(pam(pca), frame = TRUE, frame.type = 'norm'))


# detailed pca; adapted from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#pca-data-format
res.pca <- PCA(pca[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca results
print(res.pca)

# eigen values

eig.val <- get_eigenvalue(res.pca)
eig.val

# pca barplot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# saving results
var <- get_pca_var(res.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# plotting variables
fviz_pca_var(res.pca, col.var = "black")

# to visulize correlation on of varibals in each dimention
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# contributions of variables 
corrplot(var$contrib, is.corr=FALSE)    

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# grouped by elipsise
(p_pca <- fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = QHI_small$veg_type, # color by groups
             palette = c("#ffa544", "#2b299b", "gray65"),
             addEllipses = TRUE, # Concentration ellipses
             # ellipse.type = "confidence",
             ellipse.level = 0.95, # confidence level specification
             mean.point = TRUE, # braycenter mean point
             legend.title = "Groups",
             axes.linetype = "dashed",
             xlab = "PC1", ylab = "PC2", 
             ggtheme = theme_spectra()))

#ggsave(p_pca, path = "figures", filename = "QHI_pca.png", height = 10, width = 12)



# pca for all QHI measurements, with band selection
pca_lowD <- QHI %>%
  filter(wavelength %in% ISI_band_selection$wavelength) %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

res.pca_lowD <- PCA(pca_lowD[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca 
(p_pca <- fviz_pca_ind(res.pca_lowD,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = QHI_small$veg_type, # color by groups
             palette = c("#ffa544", "#2b299b", "gray65"),
             addEllipses = TRUE, # Concentration ellipses
             # ellipse.type = "confidence",
             ellipse.level = 0.95, # confidence level specification
             mean.point = TRUE, # braycenter mean point
             legend.title = "Groups",
             axes.linetype = "dashed",
             xlab = "PC1", ylab = "PC2", 
             ggtheme = theme_spectra()))

#ggsave(p_pca, path = "figures", filename = "QHI_lowD_pca.png", height = 10, width = 12)

# biplot
(p_pca <- fviz_pca_biplot(res.pca_lowD,
                repel = TRUE, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = QHI_small$veg_type, # color by groups
                palette = c("#ffa544", "#2b299b", "gray65"),
                addEllipses = TRUE, # Concentration ellipses
                # ellipse.type = "confidence",
                ellipse.level = 0.95, # confidence level specification
                mean.point = TRUE, # braycenter mean point
                legend.title = "Groups",
                axes.linetype = "dashed",
                xlab = "PC1", ylab = "PC2", 
                ggtheme = theme_spectra()))


# multi year PCA
res.pca_QHI_2018_2019 <- PCA(QHI_2018_2019[,c(5,7)], scale.unit = TRUE, ncp = 5, graph = TRUE)


# pca 
(p_pca <- fviz_pca_ind(res.pca_QHI_2018_2019,
                       geom.ind = "point", # show points only (nbut not "text")
                       col.ind = QHI_2018_2019$year, # color by groups
                       palette = c("#ffa544", "#2b299b", "gray65"),
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       legend.title = "Groups",
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2", 
                       ggtheme = theme_spectra()))

#ggsave(p_pca, path = "figures", filename = "QHI_lowD_biplot.png", height = 10, width = 12)

# PS2 ----

# PS2 24-144 (PS2) ------


list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/sort/PS2", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
PS2 <- list_of_files %>%
  purrr::set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/sort/PS2"),
         id = stringr::str_extract(FileName, "_\\d*"),
         id = str_remove_all(id, "_"),
         id = gsub("(?<![0-9])0+", "", id, perl = TRUE),
         veg_type = case_when(grepl("HE|KO", FileName)   ~ 
                                stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*")))


names(PS2) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "veg_type", "plot")

PS2 <- PS2 %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         veg_type = as.factor(veg_type)) %>%
  drop_na(reflectance) %>% 
  drop_na(wavelength) %>%
  drop_na(reference) %>%
  drop_na(target)  %>%
  # remove erronious measurments from spectrometer 
  filter(between(wavelength, 400, 985),
         !id %in% c(148:171, 196, 210:211, 250:259))

# PS2 vis -------


# single wavelengths VT
(p_PS2 <-  ggplot(PS2, aes(x = wavelength, y = reflectance, group = id, color = veg_type)) + 
   geom_line(alpha = 0.3) + 
   theme_spectra() +
   labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
   theme(legend.position = "right") +
   scale_color_manual(values = c("#ffa544", "#2b299b")) +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)


PS2_small <- PS2 %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))


# violin of mean by vegetation type
ggplot(PS2_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  theme_cowplot()

# cloud of mean by vegetation type
(p_PS2 <- ggplot(PS2_small, aes(x=veg_type, y=spec_mean, fill=veg_type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(position = position_jitter(width = .05), size = .8) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

# violin of cv by vegtation type
ggplot(PS2_small, aes(x=veg_type, y=CV, fill=veg_type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

ggplot(PS2_small, aes(x=veg_type, y=CV, fill=veg_type)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
  geom_point(position = position_jitter(width = .05), size = .8) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# ggsave(p_QHI, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)

# group by wavelength 
PS2_wavelength <- PS2 %>%
  group_by(veg_type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# GD advice: split full spec into regions (via background colors) and make seperate raincloud plot at each spec_region. (for full snazzyness add color of spec_region to backround)
#plot spectral mean
(p_PS2 <- ggplot(PS2_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    theme_spectra() +
    theme(legend.position = "right") +
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "\nWavelength (mm)", y = "CV\n"))
#ggsave(p_QHI, path = "figures", filename = "spec_sig_plot.png", height = 8, width = 10)


## SMOOTHING NOT CORRECT
ggplot(PS2_wavelength, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "mean reflectance\n")

# plot CV

ggplot(PS2_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
  geom_line(alpha = 0.9) + 
  theme_spectra() +
  theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(x = "\nWavelength (mm)", y = "CV\n")

# SMOOTHING NOT CORRECT
ggplot(PS2_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_spectra() +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "CV\n")


# PS2 PCA ----

pca_PS2 <- PS2_small

res.pca_PS2 <- PCA(pca_PS2[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)


# PS2 pca by vegtype

# PS2 pca 
(p_PS2 <- fviz_pca_ind(res.pca_PS2,
                       geom.ind = FALSE, # show points only (nbut not "text")
                       col.ind = PS2_small$veg_type, # color by groups
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       legend.title = "Groups",
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2", 
                       ggtheme = theme_spectra()))
#ggsave(p_PS2, path = "figures", filename = "PS2_pca.png", height = 10, width = 12)

# PS2 pca by plot #FIX SHAPES
(p_PS2 <- fviz_pca_ind(res.pca_PS2,
             geom.ind = FALSE, # show points only (nbut not "text")
             col.ind = PS2_small$plot, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             # ellipse.type = "confidence",
             ellipse.level = 0.95, # confidence level specification
             mean.point = TRUE, # braycenter mean point
             legend.title = "Groups",
             axes.linetype = "dashed",
             xlab = "PC1", ylab = "PC2", 
             ggtheme = theme_spectra()))

#ggsave(p_PS2, path = "figures", filename = "PS2_plot_pca.png", height = 10, width = 12)

# PS2 biplot
fviz_pca_biplot(res.pca_PS2,
                geom.ind = FALSE, # show points only (nbut not "text")
                col.ind = PS2_small$plot, # color by groups
                addEllipses = TRUE, # Concentration ellipses
                # ellipse.type = "confidence",
                ellipse.level = 0.95, # confidence level specification
                mean.point = TRUE, # braycenter mean point
                legend.title = "Groups",
                axes.linetype = "dashed",
                xlab = "PC1", ylab = "PC2", 
                ggtheme = theme_spectra())




# PCA check if sorted PS2 HE and KO plot discriminate ----

list_of_files <- list.files(path = "~/Documents/university work/Dissertation/local/QHI_fieldspec/fieldspec_sorted_sas_12.4.2020/", recursive = TRUE,
                            pattern = "\\.asc$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
QHI_PS2plot <- list_of_files %>%
  purrr::set_names(.) %>%
  map_df(read.csv2, header = FALSE, sep = "", .id = "FileName") %>%
  mutate(FileName = str_remove_all(FileName, "/Users/shawn/Documents/university work/Dissertation/local/QHI_fieldspec/fieldspec_sorted_sas_12.4.2020//gr080119"),
         id = stringr::str_extract(FileName, "_\\d*"),
         id = str_remove_all(id, "_"),
         id = gsub("(?<![0-9])0+", "", id, perl = TRUE),
         veg_type = case_when(grepl("HE|KO", FileName)   ~ 
                                stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO|PS2", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*|PS2")))

names(QHI_PS2plot) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "veg_type", "plot")

QHI_PS2plot <- QHI_PS2plot %>%
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
         # 100, 147 have reflectances >100
         !id %in% c(148:171, 196, 210:211, 250:259))

# which id hace reflectance>100
QHI_PS2plot %>% filter(reflectance>100) %>%
  count(id)

QHI_PS2plot_small <- QHI_PS2plot %>%
  mutate(veg_type =  fct_explicit_na(veg_type, na_level = "NA")) %>%
  group_by(veg_type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))


# PS2 pca

res.pca_PS2 <- PCA(PS2_small[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca results
print(res.pca_PS2)

# eigen values

eig.val <- get_eigenvalue(res.pca_PS2)
eig.val

# pca barplot
fviz_eig(res.pca_PS2, addlabels = TRUE, ylim = c(0, 50))

# saving results
var <- get_pca_var(res.pca_PS2)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# plotting variables
fviz_pca_var(res.pca_PS2, col.var = "black")

# to visulize correlation on of varibals in each dimention
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca_PS2, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca_PS2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# contributions of variables 
corrplot(var$contrib, is.corr=FALSE)    

fviz_pca_var(res.pca_QHI_PS2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# grouped by elipsise
(p_pca_PS2 <- fviz_pca_ind(res.pca_PS2,
             geom.ind = c("point"), # show points only (nbut not "text")
             col.ind = PS2_small$veg_type, # color by groups
             palette = c("#ffa544", "#2b299b"),
             
             addEllipses = TRUE, # Concentration ellipses
             # ellipse.type = "confidence",
             legend.title = "Groups"))


# H2 Plot data ----

# data import

QHI_plotdata <- read_csv("data/QHI_biodiversity/QHI_plotdata_2018_2019_sas.csv", 
                         col_types = cols(X1 = col_skip()))

# (redundant) adding sperate columns for vegtype, plot, and year
#QHI_plotdata <- QHI_plotdata %>%
  mutate(veg_type = case_when(grepl("HE|KO", plot_unique, ignore.case=TRUE) ~ 
                                stringr::str_extract(plot_unique, "HE|KO")),
         plot = case_when(grepl("_", plot_unique)   ~ 
                            stringr::str_extract(plot_unique, "_\\d*")),
         plot = str_remove_all(plot, "_"),
         # differnet methods, maybe nor great pratice, but it works...
         year = substring(plot_unique,6,9))
         

QHI_spec_plot <- left_join(QHI_2018_2019, QHI_plotdata, value = "plot_unique") %>%
  filter(!plot == "PS2")

# H2 model ----

# correlation plot

correlation <- cor(QHI_spec_plot[,c(5, 7, 9, 12:16, 19)])

corrplot(correlation, method="circle", type="upper", #order="hclust",
         tl.srt=45, tl.col="black", col=brewer.pal(n=10, name="RdYlBu"))



# spectral mean 
(hist <- ggplot(QHI_spec_plot, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())

# linear model for H2
m_H2a <- glm(data = QHI_spec_plot, spec_mean ~ veg_type + richness + evenness + bareground + (1|Year))

m_H2a <- lmer(data = QHI_spec_plot, spec_mean ~ veg_type + richness + evenness + bareground + (1|plot) + (1|year))


summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 

# Visualises random effects 
(re.effects <- plot_model(m_H2a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2a, show.values = TRUE))


ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")


# CV

(hist <- ggplot(QHI_spec_plot, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

m_H2b <- lmer(data = QHI_spec_plot, CV ~ veg_type + richness + evenness + bareground + (1|plot) + (1|year))

summary(m_H2b)

plot(m_H2b)
qqnorm(resid(m_H2b))
qqline(resid(m_H2b)) 

# Visualises random effects 
(re.effects <- plot_model(m_H2b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2b, show.values = TRUE))

ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=veg_type, color = veg_type)) + 
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE )+
  theme_spectra() +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")




collison_small_VT <- collison %>%
  group_by(veg_type,wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

ggplot(collison, aes(x = wavelength, y = reflectance, group = veg_type, color = veg_type)) + 
  stat_smooth(method = "lm", aes(fill = veg_type, color = veg_type), se=TRUE ) +
  geom_smooth(methods = "lm", alpha = 0.2, se=TRUE) + 
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(x = "Wavelength (mm)", y = "Reflectance") +
  theme_spectra() +
  theme(legend.position = "bottom")

# H2 plot PCA ----

# only 2019

pca_H2 <- QHI_spec_plot %>%
  filter(year == 2019)

head(pca_H2)

# ncp = 10 (10 variables)
res.pca_H2 <- PCA(pca_H2[,c(5, 7, 9, 12:16, 19)], scale.unit = TRUE, ncp = 10, graph = TRUE)

# eigen values

eig.val <- get_eigenvalue(res.pca_H2)
eig.val

# pca barplot
fviz_eig(res.pca_H2, addlabels = TRUE, ylim = c(0, 50))

# saving results
var <- get_pca_var(res.pca_H2)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# plotting variables
fviz_pca_var(res.pca_H2, col.var = "black")

# to visulize correlation on of varibals in each dimention
library("corrplot")

corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca_H2, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca_H2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# contributions of variables 
corrplot(var$contrib, is.corr=FALSE)    

fviz_pca_var(res.pca_H2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# grouped by elipsise

t <- QHI_spec_plot %>%
  filter(year == 2019)

(p_pca <- fviz_pca_biplot(res.pca_H2,
                       geom.ind = "point", # show points only (nbut not "text")
                       fill.ind = t$plot_unique, # color by groups
                       pointshape = 21, 
                       col.ind = "black",
                      # palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       repel = TRUE,
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       # to color arrow by variable type
                       col.var = factor(c("spectral", "spectral", "diversity", "diversity",
                                "environmenal", "environmenal", "environmenal", 
                                "environmenal", "environmenal")),
                      # gradient.cols = c("#00AFBB", "#00AFBB", "#FC4E07", "#FC4E07",
                       #                  "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800"),
                      # col.var = "cos2",
                      # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      # alternate color gradient gradient.cols = c("blue", "yellow", "red")
                       legend.title = list(fill = "Sites", color = "cos2"),
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2")) +
  ggpubr::fill_palette(c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"))+      # Indiviual fill color
  ggpubr::color_palette("Dark2")      # Variable colors


# pca 2018 & 2019

pca_H2_2018_2019 <- QHI_spec_plot 

head(pca_H2)

# ncp = 10 (10 variables)
res.pca_H2_2018_2019 <- PCA(pca_H2_2018_2019[,c(5, 7, 9, 12:16, 19)], scale.unit = TRUE, ncp = 10, graph = TRUE)

# eigen values

eig.val <- get_eigenvalue(res.pca_H2_2018_2019)
eig.val

# pca barplot
fviz_eig(res.pca_H2_2018_2019, addlabels = TRUE, ylim = c(0, 50))

# saving results
var <- get_pca_var(res.pca_H2_2018_2019)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# plotting variables
fviz_pca_var(res.pca_H2_2018_2019, col.var = "black")

# to visulize correlation on of varibals in each dimention
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca_H2_2018_2019, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca_H2_2018_2019, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# contributions of variables 
corrplot(var$contrib, is.corr=FALSE)    

fviz_pca_var(res.pca_H2_2018_2019, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# biplot grouped by year 
(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = QHI_spec_plot$year, # color by groups
                          pointshape = 21, 
                          #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
                          addEllipses = TRUE, # Concentration ellipses
                          # ellipse.type = "confidence",
                          repel = TRUE,
                          ellipse.level = 0.95, # confidence level specification
                          mean.point = TRUE, # braycenter mean point
                          # to color arrow by variable type
                          col.var = factor(c("spectral", "spectral", "diversity", "diversity",
                                             "environmenal", "environmenal", "environmenal", 
                                             "environmenal", "environmenal")),
                          gradient.cols = c("#00AFBB", "#00AFBB", "#FC4E07", "#FC4E07",
                                            "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800"),
                          # col.var = "cos2",
                          # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          # alternate color gradient gradient.cols = c("blue", "yellow", "red")
                          legend.title = list(fill = "Sites", color = "cos2"),
                          axes.linetype = "dashed",
                          xlab = "PC1", ylab = "PC2")) +
  ggpubr::color_palette("Dark2")      # Variable colors
  #ggpubr::fill_palette(c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"))+      # Indiviual fill color
  

# biplot grouped by veg_type 
(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = QHI_spec_plot$veg_type, # color by groups
                          pointshape = 21, 
                          #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
                          addEllipses = TRUE, # Concentration ellipses
                          # ellipse.type = "confidence",
                          repel = TRUE,
                          ellipse.level = 0.95, # confidence level specification
                          mean.point = TRUE, # braycenter mean point
                          # to color arrow by variable type
                          col.var = factor(c("spectral", "spectral", "diversity", "diversity",
                                             "environmenal", "environmenal", "environmenal", 
                                             "environmenal", "environmenal")),
                          gradient.cols = c("#00AFBB", "#00AFBB", "#FC4E07", "#FC4E07",
                                            "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800"),
                          # col.var = "cos2",
                          # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          # alternate color gradient gradient.cols = c("blue", "yellow", "red")
                          legend.title = list(fill = "Sites", color = "cos2"),
                          axes.linetype = "dashed",
                          xlab = "PC1", ylab = "PC2")) +
  ggpubr::color_palette("Dark2")      # Variable colors

# pca by veg_type and year
t <- QHI_spec_plot %>%
  unite(site, c(veg_type,year))

(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = t$site, # color by groups
                          pointshape = 21, 
                        #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FFB90F", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
                          addEllipses = TRUE, # Concentration ellipses
                          # ellipse.type = "confidence",
                          repel = TRUE,
                          ellipse.level = 0.95, # confidence level specification
                          mean.point = TRUE, # braycenter mean point
                          # to color arrow by variable type
                          col.var = factor(c("spectral", "spectral", "diversity", "diversity",
                                             "environmenal", "environmenal", "environmenal", 
                                             "environmenal", "environmenal")),
                          gradient.cols = c("#00AFBB", "#00AFBB", "#FC4E07", "#FC4E07",
                                            "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800"),
                          # col.var = "cos2",
                          # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          # alternate color gradient gradient.cols = c("blue", "yellow", "red")
                          legend.title = list(fill = "Sites", color = "cos2"),
                          axes.linetype = "dashed",
                          xlab = "PC1", ylab = "PC2")) +
  ggpubr::color_palette("Dark2")  +    # Variable colors
  ggpubr::fill_palette(c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF"))     # Indiviual fill color




