# QHI 2018_2019 field spec data manipulation
# shawn schneidereit edit
# 5.3.2020


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
library(gridExtra)
library(ggpmisc) # for ISI minima visulizaiton
library(RColorBrewer)
library(broom)
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(glmmTMB) # to visualise model predictions
library(dotwhisker) # to visulaise effect whiskerplots
library(lme4) # for models
library(sp) # spatial variogram
library(gstat) # spatial variogram
library(tidyverse)
library(effects) # for model vis with interaction terms



source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# functions ----
theme_cowplot <- function(){
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
                              alpha = .15, fill = "darkgrey"),
                     scale_y_continuous(expand = expand_scale(mult = c(0, .1))),
                     scale_x_continuous(expand = expand_scale(mult = c(0, .1))))

scale_color_QHI <- list(scale_color_manual(values = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF8247", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF", "#A8A8A8")))
scale_color_collison <- list(scale_color_manual(values = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF8247", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF")))



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
      theme_cowplot() +
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
      theme_cowplot() +
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
      theme_cowplot() +
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
      theme_cowplot() +
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
find_peaks <- function (x, window = y){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - window + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + window + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

# # A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
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
         type = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO|PS2", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*|PS2")))

names(QHI) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "type", "plot")

QHI <- QHI %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         type = as.factor(type),
         type =  fct_explicit_na(type, na_level = "mixed"),
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
  dplyr::count(id)

#filter(wavelength %in% ISI_band_selection$wavelength) %>%

# remove measuments with reflectance values over 100
QHI <- QHI %>%
  filter(!id %in% c(100, 104, 106, 147, 172, 207, 208))

t_QHI_2018_2019_wavelength <- QHI_2018_2019 %>%
  mutate(plot_unique = paste(plot, year, sep="_"),
         type_year = paste(type, year, sep="_")) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance))


t_QHI_2018_2019_small <- t_QHI_2018_2019_wavelength %>%
  group_by(type, plot_unique, year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))

# group by id
QHI_small <- QHI %>%
  group_by(year, type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance))


head(QHI_small)


# group by wavelength 
QHI_2019_wavelength <- QHI %>%
  group_by(type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

collison <- QHI %>%
  filter(type != "mixed")

# only 2019 collison data
collison_small <- collison %>%
  group_by(type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

PS2 <- QHI %>%
  filter(type == "mixed")

##### correction factros for 250-259 reflectance. but to small. need > 3. also dont know how to apply to refletance 
#HE1_corr <- full_QHI %>%
#filter(id %in% c("249", "250")) %>%
#  group_by(wavelength) %>%
#  mutate(reference = parse_number(as.character(reference))/100,
#         reflectance = parse_number(as.character(reflectance))/100,
#         correction_factor = (reference[id == "250"] / reference[id == "249"]),
#         reflectance = reflectance/correction_factor)



# adding 2018 spectral data

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
  mutate(Reflectance = Reflectance,
         plot = str_remove_all(plot, "LT"),
         year = "2018",
         type = case_when(site == "Herschel" ~ "HE",
                          site == "komukuk" ~ "KO")) %>%
  # add id by groups
  mutate(id = as.character(group_indices(., type, plot, measurement) + 385)) %>% # 384 IDs for 2019 spectral data
  unite(plot, c(type, plot), sep = "", remove = FALSE) %>%
  rename(reflectance = Reflectance,
         wavelength = Wavelength)


# summarized by measurment spec 2018 data 
spec_2018_small <- spec_2018 %>%
  group_by(year, type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# binding 2018 & 2019 spec data

# df of spectra with "raw" wavelength grouping 
QHI_2018_2019 <- bind_rows(QHI, spec_2018) %>%
  ungroup() %>%
  # temporary colunm with only plot number 
  mutate(plot2 = str_remove_all(plot, "HE|KO"),
         plot_unique = paste(type,plot2,year,sep="_")) %>%
  # remove colunm
  select(-plot2)

# df of spectra with plot grouping 
QHI_2018_2019_small <- bind_rows(QHI_small, spec_2018_small) %>%
  mutate(plot_unique = paste(plot, year, sep="_"),
         type_year = paste(type, year, sep="_"))

QHI_2018_2019_small <- QHI_2018_2019 %>%
group_by(year, type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance))


collison_2018_2019_small <- QHI_2018_2019_small %>%
  filter(!type == "mixed")

# df of spectra wuth plot and wavelength grouping
QHI_2018_2019_wavelength <- QHI_2018_2019 %>%
  filter(!type =="mixed") %>%
  mutate(plot_unique = paste(plot, year, sep="_"),
         type_year = paste(type, sep="_", year)) %>%
  group_by(type, plot, wavelength, year, plot_unique, type_year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = sd(reflectance)/mean(reflectance))


# changed to just type year and wavelength
QHI_2018_2019_type_wavelength <- QHI_2018_2019 %>%
  mutate(type_year = paste(type, year, sep="_")) %>%
  group_by(type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = sd(reflectance)/mean(reflectance))


ggplot(QHI_2018_2019_type_wavelength, aes(x = wavelength, y = spec_mean, group = type_year, color = type_year)) + 
  geom_line(size=1) + 
  theme_cowplot() +
  labs(x = "Wavelength (mm)", y = "Mean CV") +
  scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
  theme(legend.position = "right")

ggplot(QHI_2018_2019_type_wavelength, aes(x = wavelength, y = CV, group = type_year, color = type_year)) + 
  geom_line(size=1) + 
  theme_cowplot() +
  labs(x = "Wavelength (mm)", y = "Mean CV") +
  scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
  theme(legend.position = "right")

# just 2018 by wavelength
spec_2018_wavelength <- spec_2018 %>%
  group_by(type, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance))


ggplot(spec_2018_wavelength, aes(x = wavelength, y = CV, group = type, color = type)) + 
  geom_line(alpha = 0.2) + 
  theme_cowplot() +
  labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
  theme(legend.position = "right") +
  # scale_color_QHI +
  guides(colour = guide_legend(override.aes = list(size=5)))

# trying violine via small df

Wavelength + site + plot + sub_site

t_QHI_2018_2019_wavelength <- QHI_2018_2019 %>%
  mutate(plot_unique = paste(plot, year, sep="_"),
         type_year = paste(type, year, sep="_")) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance))


t_QHI_2018_2019_small <- t_QHI_2018_2019_wavelength %>%
  group_by(type, plot_unique, year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))

# violin with cv per wavelength
ggplot(t_QHI_2018_2019_wavelength, aes(x=type, y=CV, fill=year)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  #  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# violin with cv per plot (mixed no violin as the is only one plot)
ggplot(t_QHI_2018_2019_small, aes(x=type, y=CV, fill=year)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
#  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

 ggplot(t_QHI_2018_2019_wavelength, aes(x = wavelength, y = CV, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    #scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot()+
    theme(legend.position = "right")


# Visible spectrum
t_spec_bio_sum_vis <- t_QHI_2018_2019_small %>% 
  group_by(plot_unique) %>%
  mutate(cv.refl = sd(spec_mean)/mean(spec_mean)) %>%
  dplyr::select(2:10, cv.refl) %>% distinct()


pec_bio_sum_vis <- spec_bio %>% 
  filter(Wavelength < 680) %>%
  group_by(sub_site) %>%
  mutate(cv.refl = sd(Reflectance_mean)/mean(Reflectance_mean)) %>%
  dplyr::select(2:10, cv.refl) %>% distinct()

ggplot(spec_bio_sum_vis, aes(x=site, y=cv.refl, fill=site)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()


# alison beamish spectral data import
# Load in spectral data


### subset to 3x3 grid sampling method in long-term plots ###
het <- spectra_040818[spectra_040818$type == "target",]
het1 <- subset(het, method == "point")
het2 <- subset(het1, site !="Moss")
het2b <- subset(het2, plot !="H20")
het2c <- subset(het2b, measurement !="bad")

het2c$Reflectance <- ifelse(het2c$Wavelength>= 1800 & het2c$Wavelength <= 2000, NA,
                            ifelse(het2c$Wavelength >= 2400,NA,het2c$Reflectance))

het2c <- droplevels(het2c)
het2c$site <- factor(het2c$site, levels = c("Herschel", "komukuk"), labels = c("HE", "KO"))
het2c$plot <- factor(het2c$plot, levels = c("LT1","LT2","LT3","LT4","LT5","LT6"), labels = c("1","2","3","4","5","6"))
het2c <- droplevels(het2c)
het2c$sub_site <- paste0("QHI:",het2c$site,het2c$plot)

t <- het2c %>% filter(between(Wavelength, 400, 1000))

(p_ali_spec <- ggplot()+
    geom_line(data = t, aes(x = Wavelength, y = Reflectance, col = site, group = measurement))+
    #geom_line(data = spec_2018, aes(x = wavelength, y = reflectance, col = plot, group = id)) +
    #scale_color_manual(values = c("#ffa544", "#2b299b"))+
    facet_wrap(~sub_site, ncol = 3)+
   # ylim(0,0.7)+
    ylab("Reflectance\n")+
    xlab("\nWavelength (nm)")+
    theme_spectra() +
  guides(col = F, linetype = F))

(p_my_spec <- ggplot()+
  geom_line(data = spec_2018, aes(x = wavelength, y = reflectance, col = plot, group = id)) + 
  facet_wrap(~plot, ncol = 3) +
  theme_spectra())
  
grid.arrange(p_ali_spec, p_my_spec)
# they are the same


# Summary table

spec_bio <- read_csv("data/spec_bio.csv")
colnames(spec_bio)[5:10] <- ""

# Visible spectrum
spec_bio_sum_vis <- spec_bio %>% 
  filter(Wavelength < 680) %>%
  group_by(sub_site) %>%
  mutate(cv.refl = sd(Reflectance_mean)/mean(Reflectance_mean)) %>%
  dplyr::select(2:10, cv.refl) %>% distinct()

spec_bio_sum_vis_long <- spec_bio_sum_vis %>%
 gather("metric", "value", 4:9)

ggplot(spec_bio_sum_vis, aes(x=site, y=cv.refl, fill=site)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

 ggplot(spec_bio_sum_vis_long, 
                                 aes(x = value, y = cv.refl,
                                     colour = site)) +
    geom_point() +
    theme_spectra() +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    guides(colour = F) +
    theme(strip.background = element_rect(fill = "white"),
          axis.line.x = element_line(),
          axis.line.y = element_line()) +
    labs(x = NULL, y = "Spectral heterogeneity\n") +
    geom_quantile(quantiles = 0.5) +
    #geom_quantile(method = "rqss") +
    facet_grid(~metric, scales = "free")


# H2 Plot data ----

# data import

QHI_plotdata <- read_csv("data/QHI_biodiversity/QHI_plotdata_2018_2019_sas.csv", 
                         col_types = cols(X1 = col_skip()))

# (redundant) adding sperate columns for vegtype, plot, and year
#QHI_plotdata <- QHI_plotdata %>%
#  mutate(type = case_when(grepl("HE|KO", plot_unique, ignore.case=TRUE) ~ 
#                                stringr::str_extract(plot_unique, "HE|KO")),
#         plot = case_when(grepl("_", plot_unique)   ~ 
#                            stringr::str_extract(plot_unique, "_\\d*")),
#         plot = str_remove_all(plot, "_"),
#         # differnet methods, maybe nor great pratice, but it works...
#         year = substring(plot_unique,6,9))


# 2018+2019 QHI spectral and plot data
QHI_spec_plot <- left_join(QHI_2018_2019, QHI_plotdata, value = "plot_unique") 

collison_spec_plot_small <- left_join(QHI_2018_2019, QHI_plotdata, value = "plot_unique") %>%
  filter(!plot == "PS2") %>%
  group_by(id, type, plot, year, plot_unique, richness, shannon,
           simpson, evenness, bareground, dead,
           reproductive_tissue, total_cover, gram_shrub_ratio) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

head(QHI_2018_2019)