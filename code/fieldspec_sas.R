# QHI 2018_2019 field spec 
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

# group by id
QHI_small <- QHI %>%
  group_by(year, type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

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
  mutate(Reflectance = Reflectance*100,
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


collison_2018_2019_small <- QHI_2018_2019_small %>%
  filter(!type == "mixed")

# df of spectra wuth plot and wavelength grouping
QHI_2018_2019_wavelength <- QHI_2018_2019 %>%
  mutate(plot_unique = paste(plot, year, sep="_"),
         type_year = paste(type, sep="_", year)) %>%
  group_by(type, plot, wavelength, year, plot_unique, type_year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

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

#  band selection ----

# supervised band selection

supervised_band_selection <- tibble(wavelength = 
                                      # need to sequence by 0.01 to subsequently filter wavelengths
                                      c(seq(430, 450, by = 0.01), # Chlorophyll & carotenoid absorption
                                        seq(545, 555, by = 0.01), # green reflectance peak
                                        seq(660, 680, by = 0.01), # Max absorption of chlorophyll
                                        seq(700, 725, by = 0.01), # Middle of red-edge transition
                                        seq(745, 755, by = 0.01), # End of red-edge transition
                                        seq(920, 985, by = 0.01)))# Vascular plant structures & H20 

supervised_band_selection <- QHI_2018_2019 %>%
  filter(!type == "mixed", 
    wavelength %in% supervised_band_selection$wavelength) %>%
  group_by(type, plot, id, year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))


# ISI band selection and SZU 

collison_ISI <- collison %>%
  filter(type %in% c("KO" , "HE")) %>%
  group_by(wavelength) %>%
  summarise(ISI = ((mean(reflectance[type=="HE"]) + mean(reflectance[type=="KO"]))*1.96)/
              (abs(sd(reflectance[type=="HE"] - sd(reflectance[type=="KO"]))))) %>%
  mutate(region = case_when(between(wavelength, 400, 500) ~ "blue",		
                            between(wavelength, 500, 600) ~ "green",		
                            between(wavelength, 600, 680) ~ "red",		
                            between(wavelength, 680, 800) ~ "NIR",		
                            between(wavelength, 800, 1000) ~ "IR")) 


ISI_band_selection <- collison_ISI %>%
  mutate(n = row_number()) %>%
  # filter wavelengths that are local ISI minima; (-) is to denote minima
  filter(n %in% find_peaks(-collison_ISI$ISI, window = 3)) %>%
  mutate(region = case_when(between(wavelength, 400, 500) ~ "blue",		
                            between(wavelength, 500, 600) ~ "green",		
                            between(wavelength, 600, 680) ~ "red",		
                            between(wavelength, 680, 800) ~ "NIR",		
                            between(wavelength, 800, 1000) ~ "IR")) 

# ISI by region
ISI_tbl <- collison_ISI %>%
  group_by(region) %>%
  summarise(ISI = sum(ISI))

# number relative ISI (as groups include differnt numbers of wavebands)
ISI_tbl <- collison_ISI %>%
  group_by(region) %>%
  count(region) %>%
  left_join(ISI_tbl) %>%
  # relative ISI colunm (ISI* portotional size of region)
  mutate(relative_ISI = ISI/(n/123)) # need to change value with number of bands in largest region

# number wavebands selected 
ISI_tbl <- ISI_band_selection %>%
  group_by(region) %>%
  count(region) %>%
  rename(wavebands_selected = n) %>%
  right_join(ISI_tbl, value="region")

# ISI of these selected wavebands   
ISI_tbl <- ISI_band_selection %>%
  group_by(region) %>%
  summarise(selected_ISI = sum(ISI)) %>%
  left_join(ISI_tbl) 
            
 

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
lowD <- QHI_2018_2019 %>%
  filter(wavelength %in% ISI_band_selection$wavelength) %>%
  group_by(type, plot, id, year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# need to spruce up https://www.rdocumentation.org/packages/ggpmisc/versions/0.3.3/topics/stat_peaks
# plot of ISI by wavelength and local minima
(p_ISI <-  ggplot(collison_ISI, aes(x=wavelength, y=ISI)) +
    geom_line() +
    theme_cowplot() +
    geom_point(data = ISI_band_selection) +
    #stat_valleys(span = 3, shape = 1, size = 2, color = "black", fill = NA) +
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
    # hardcode to match total number of selected wavebands USE: sum(ISI_tbl$wavebands_selected)
    geom_vline(xintercept = 25, linetype="dotted") + 
    geom_line() +
    labs(x= "Number of bands selected ") +
    theme_cowplot())



#ggsave(p_SZU, path = "figures", filename = "SZU.png", height = 10, width = 12)


############# QHI ISI band selection and SZU 

# inelegant solution to selecting only 2018 wavebands that have a matching 2019 evivalent 
# # # REASON: (2018 has a higher resolution that breaks selection algorythem)
# first filter only 2019 
# then round (as 2018 has no significant digits)
# finally filter QHI_2018_2019 for band in  (next chunk)
wavelengths_2019_rounded <- QHI_2018_2019 %>%
  filter(year =="2019") %>%
  mutate(wavelength = round(wavelength, digits = 0)) 

QHI_ISI <- QHI_2018_2019 %>%
  filter(type %in% c("KO" , "HE")) %>% # alothough later the selected wavebands get applied to PS2 data you only selected wavebands with known vegtypes
  mutate(wavelength = round(wavelength, digits = 0)) %>%
  filter(wavelength %in% wavelengths_2019_rounded$wavelength) %>%
  group_by(wavelength) %>%
  summarise(ISI = ((mean(reflectance[type=="HE"]) + mean(reflectance[type=="KO"]))*1.96)/
              (abs(sd(reflectance[type=="HE"] - sd(reflectance[type=="KO"]))))) %>%
  mutate(region = case_when(between(wavelength, 400, 500) ~ "blue",		
                            between(wavelength, 500, 600) ~ "green",		
                            between(wavelength, 600, 680) ~ "red",		
                            between(wavelength, 680, 800) ~ "NIR",		
                            between(wavelength, 800, 1000) ~ "IR")) 


QHI_ISI_band_selection <- QHI_ISI %>%
  mutate(n = row_number()) %>%
  # filter wavelengths that are local ISI minima; (-) is to denote minima
  filter(n %in% find_peaks(-QHI_ISI$ISI, window = 3)) %>%
  mutate(region = case_when(between(wavelength, 400, 500) ~ "blue",		
                            between(wavelength, 500, 600) ~ "green",		
                            between(wavelength, 600, 680) ~ "red",		
                            between(wavelength, 680, 800) ~ "NIR",		
                            between(wavelength, 800, 1000) ~ "IR")) 

 ggplot(QHI_ISI, aes(x=wavelength, y=ISI)) +
    geom_line() +
    theme_cowplot()
  
  
 # ISI by region
 QHI_ISI_tbl <- QHI_ISI %>%
   group_by(region) %>%
   summarise(ISI = sum(ISI))
 
 # number relative ISI (as groups include differnt numbers of wavebands)
 QHI_ISI_tbl <- QHI_ISI %>%
   group_by(region) %>%
   count(region) %>%
   left_join(QHI_ISI_tbl) %>%
   # relative ISI colunm (ISI* portotional size of region)
   mutate(relative_ISI = ISI/(n/122)) # NEED TO HARDCODE VALUE TO CORREPOND WITH THE LARGEST N VALUE IN QHI_ISI_tbl
 
 # number wavebands selected 
 QHI_ISI_tbl <- QHI_ISI_band_selection %>%
   group_by(region) %>%
   count(region) %>%
   rename(wavebands_selected = n) %>%
   right_join(QHI_ISI_tbl, value="region")
 
 # ISI of these selected wavebands   
 QHI_ISI_tbl <- QHI_ISI_band_selection %>%
   group_by(region) %>%
   summarise(selected_ISI = sum(ISI)) %>%
   left_join(QHI_ISI_tbl) 


QHI_SZU <- QHI_ISI %>%
  arrange(ISI) %>%
  mutate(ISI = as.numeric(ISI),
         n = row_number(),
         d_ISI = (lead(ISI)/ISI - 1) *100,
         # 0.015 is the trade-off value (q)
         d_qi = (0.015- d_ISI),
         # number of bands is equal to max DISIi 
         D_ISIi = cumsum(d_qi))

# the selcted wavelengths according to SZU
head(QHI_SZU, n=5)

# reduced dimentionality; product of ISI band selection
QHI_lowD <- QHI_2018_2019 %>%
  filter(wavelength %in% QHI_ISI_band_selection$wavelength) %>%
  group_by(type, plot, id, year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# need to spruce up https://www.rdocumentation.org/packages/ggpmisc/versions/0.3.3/topics/stat_peaks
# plot of ISI by wavelength and local minima
ggplot(QHI_ISI, aes(x=wavelength, y=ISI)) +
    geom_line() +
    theme_cowplot() +
    geom_point(data = QHI_ISI_band_selection, shape = 1) +
    geom_rug(data = QHI_ISI_band_selection, sides = "b" ) +
    #stat_valleys(span = 3, shape = 1, size = 2, color = "black", fill = NA) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
    scale_x_continuous(expand = expand_scale(mult = c(0, .1))) +
    annotate("rect", xmin = 400, xmax = 500, ymin = 15,
             ymax = 21, alpha = .15, fill = "blue") + 
    annotate("rect", xmin = 500, xmax = 600, ymin = 15, 
             ymax = 21, alpha = .15, fill = "green") +
    annotate("rect", xmin = 600, xmax = 680, ymin = 15, 
             ymax = 21, alpha = .15, fill = "red") + 
    annotate("rect", xmin = 680, xmax = 800, ymin = 15, 
             ymax = 21, alpha = .15, fill = "tomato") +
    annotate("rect", xmin = 800, xmax = 985, ymin = 15, 
             ymax = 21, alpha = .15, fill = "darkgrey")

#ggsave(p_ISI, path = "figures", filename = "ISI_by_wavelength.png", height = 10, width = 12)

# plot of trends in accumulated D_ISIi values
ggplot(QHI_SZU, aes(x=n, y=D_ISIi)) +
    # hardcode to match total number of selected wavebands USE: sum(QHI_ISI_tbl$wavebands_selected)
    geom_vline(xintercept = 24, linetype="dotted") + # need to pick correct vline
    geom_line() +
    labs(x= "Number of bands selected ") +
    theme_cowplot()

#ggsave(p_SZU, path = "figures", filename = "SZU.png", height = 10, width = 12)


#  QHI vis -------

# for subsets of measurements (was used for data sorting of specific measurments)
(ggplot(subset(QHI ,id %in% c(100, 104, 106, 147, 172, 207, 208))) +
   aes(x = wavelength, y = reflectance, group = id, color = id)) + 
  geom_line(alpha = 0.9) + 
  theme_cowplot() +
  labs(x = "\nWavelength (mm)", y = "Reflectance\n") #+
geom_hline( yintercept= c(50,70), color = "red") #+
facet_wrap(.~id) 


#####  2019 data
# single wavelengths VT (2019)
(p_QHI <-  ggplot(QHI, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
    geom_line(alpha = 0.3) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    # scale_color_viridis_d(option = "C") +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)

# single wavelengths at plot level (2019)
(p_QHI <-  ggplot(QHI, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_QHI +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)

# violin of mean by vegtype
ggplot(QHI_small, aes(x=type, y=spec_mean, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5, aes(fill = type)) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# cloud of mean by vegetation type

(p_QHI_cloud_mean <- ggplot(QHI_small, aes(x=type, y=spec_mean, fill=type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_small, aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_QHI +
    theme_cowplot() +
    labs(y = "Reflectance") +
    theme(legend.position = "none"))
#ggsave(p_QHI, path = "figures", filename = "cloud_specmean.png", height = 8, width = 10)

# violin of cv by vegtation type
ggplot(QHI_small, aes(x=type, y=CV, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# cloud of cv by vegtation type
(p_QHI_cloud_cv <- ggplot() +
    geom_flat_violin(data = QHI_small, aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_small, aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(data = QHI_small, aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_QHI +
    theme_cowplot() +
    theme(legend.position = "none"))
#ggsave(p_QHI, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)


# GD advice: split full spec into regions (via background colors) and make seperate raincloud plot at each spec_region. (for full snazzyness add color of spec_region to backround)
# plot spectral mean by plot
(p_QHI_specmean <- ggplot(QHI_2019_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "none") +
    scale_color_QHI +
    theme_rgb_mean)

# plot CV by plot
(p_QHI_CV <- ggplot(QHI_2019_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1) + 
    theme_cowplot() +
    theme(legend.position = "none") +
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "CV") + 
    scale_color_QHI +
    theme_rgb_CV)

##### 2018 data
# checking spectral signatures of 2018 data
ggplot(spec_2018, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
  geom_line(alpha = 0.3) + 
  theme_cowplot() +
  labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
  theme(legend.position = "right") +
  # scale_color_viridis_d(option = "C") +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  guides(colour = guide_legend(override.aes = list(size=5)))

# single wavelengths at plot level (2018)
(p_QHI <-  ggplot(spec_2018, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_QHI +
    guides(colour = guide_legend(override.aes = list(size=5))))

# violin of mean reflecatance of 2018 data
ggplot(spec_2018_small, aes(x=type, y=spec_mean, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# violin of CV of 2018 data
ggplot(spec_2018_small, aes(x=type, y=CV, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()


##### 2018 + 2019
# spectral signatures by type 2018 + 2019
(p_QHI <-  ggplot(QHI_2018_2019, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
    geom_line(alpha = 0.3) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    # scale_color_viridis_d(option = "C") +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)

# spectral signatures by plot 2018 + 2019
(p_QHI <-  ggplot(QHI_2018_2019, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_QHI +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)


# spectral signatures of mean reflectance by group and year (2018 2019)

(p_QHI_2018_2019_mean <- ggplot(QHI_2018_2019_wavelength, aes(x = wavelength, y = spec_mean, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    ggpubr::color_palette(c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_mean)

# spectral signatures of CV by group and year (2018 2019)
(p_QHI_2018_2019_cv <- ggplot(QHI_2018_2019_wavelength, aes(x = wavelength, y = CV, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_CV)


# cloud of spec mean 2018+2019
(p_QHI_cloud_mean <- ggplot() + 
    geom_flat_violin(data = QHI_2018_2019_small, aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_2018_2019_small, aes(x=type, y=spec_mean, colour=year),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(data = QHI_2018_2019_small, aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_brewer(palette = "Dark2") +
    theme_cowplot())
ggsave(p_QHI, path = "figures", filename = "cloud_spec_mean_2018_2019.png", height = 8, width = 10)


# cloud of spec diversity 2018 + 2019
(p_QHI_cloud_cv <- ggplot() +
    geom_flat_violin(data = QHI_2018_2019_small, aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = QHI_2018_2019_small, aes(x=type, y=CV, colour=year),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(data = QHI_2018_2019_small, aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_brewer(palette = "Dark2") +
    theme_cowplot())
#ggsave(p_QHI, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)


#plot spectral mean 2018+2019
(p_QHI <- ggplot(QHI_2018_2019_wavelength, aes(x = wavelength, y = spec_mean, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot()+
    theme(legend.position = "right") +
    theme_rgb_mean)


#ggsave(p_QHI, path = "figures", filename = "spec_sig_plot.png", height = 8, width = 10)

## SMOOTHING NOT CORRECT
(p_QHI_specmean <- ggplot(QHI_2018_2019_wavelength, aes(x = wavelength, y = spec_mean, group=type_year, color = type_year)) + 
    geom_smooth(alpha = 0.2, se=TRUE) +
    # scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    theme_cowplot() +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme(legend.position = c(0.05,0.7)) +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_mean)
#ggsave(p_QHI, path = "figures", filename = "CV_plot.png", height = 8, width = 10)

# SMOOTHING NOT CORRECT
(p_QHI_CV <- ggplot(QHI_2018_2019_wavelength, aes(x = wavelength, y = CV, group=type_year, color = type_year)) + 
    geom_smooth(alpha = 0.2, se=TRUE) + 
    theme_cowplot() +
    labs(x = "Wavelength (mm)", y = "CV") +
    scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_CV +
    theme(legend.position = "none"))

#H1 figure ----

# Move to a new page
grid.newpage()        

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 2)))

# Arrange the plots
print(p_QHI_cloud_mean + rremove("legend"), vp = define_region(row = 1, col = 1))   
print(p_QHI_cloud_cv, vp = define_region(row = 1, col = 2))
print(p_QHI_specmean , vp = define_region(row = 2, col = 1))
print(p_QHI_CV + rremove("legend"), vp = define_region(row = 2, col = 2))
print(p_pca_veg_year , vp = define_region(row = 3:4, col = 1:2))

#  collison head vis (2019) -------

# single wavelengths VT
(p_collison <-  ggplot(collison, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
   geom_line(alpha = 0.3) + 
   theme_cowplot() +
   labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
   theme(legend.position = "right") +
   # scale_color_viridis_d(option = "C") +
   scale_color_manual(values = c("#ffa544", "#2b299b")) +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_collison, path = "figures", filename = "spec_sig_collison.png", height = 10, width = 12)

# single wavelengths plot
(p_collison <-  ggplot(collison, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_collison +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig_collison.png", height = 8, width = 10)

# group by id
collison_small <- collison %>%
  group_by(type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# group by wavelength grouping
collison_wavelength <- collison %>%
  group_by(type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# violin of mean by vegetation type
ggplot(collison_small, aes(x=type, y=spec_mean, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# cloud of mean by vegetation type

(p_collison <- ggplot(collison_small, aes(x=type, y=spec_mean, fill=type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = collison_small, aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .05), size = 2) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_collison +
    theme_cowplot())
#ggsave(p_collison, path = "figures", filename = "cloud_specmean_collison.png", height = 8, width = 10)


# violin of cv by vegtation type
ggplot(collison_small, aes(x=type, y=CV, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# cloud of spec mean by VT
(p_collison <- ggplot() +
    geom_flat_violin(data = collison_small, aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = collison_small, aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .05), size = 2) +
    geom_boxplot(data = collison_small, aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_collison +
    theme_cowplot())
#ggsave(p_collison, path = "figures", filename = "cloud_CV_collison.png", height = 8, width = 10)

# plots mean reflectance 
(p_col_mean <- ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    scale_color_collison +
    theme_rgb_mean)
#ggsave(p_col_mean, path = "figures", filename = "spec_sig_plot_collison.png", height = 8, width = 10)

##  smoothed plots mean reflectance SMOOTHING NOT CORRECT
ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=type, color = type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_cowplot() +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n")

# plot CV

(p_col_CV <- ggplot(collison_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "CV") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    scale_color_collison +
    theme_rgb_CV)

#ggsave(p_col_CV, path = "figures", filename = "CV_plot_collison.png", height = 8, width = 10)

# SMOOTHING NOT CORRECT
ggplot(collison_wavelength, aes(x = wavelength, y = CV, group=type, color = type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot() +
  labs(x = "\nWavelength (mm)", y = "CV\n")

#  collison facet plot ----
#  spectral mean and CV violin plots by region ----

collison_wavelength <- collison %>%		
  group_by(type, plot, wavelength) %>%		
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
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("blue")),
               aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("blue")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    scale_color_collison +
    ylab("Reflectance") +
    ylim(0, 85) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "#cfe2fd")))

# blue CV
(p_blue_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("blue")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("blue")),
               aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("blue")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    ylab("Spectral diversity (CV)") +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "#cfe2fd")))

# green mean
(p_green_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("green")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("green")),
               aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("green")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 85) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "lightgreen")))


# green CV
(p_green_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("green")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("green")),
               aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("green")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "lightgreen")))

# red mean
(p_red_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("red")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("red")),
               aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("red")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 85) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "red")))


# red CV
(p_red_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("red")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("red")),
               aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("red")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "red")))

# NIR mean
(p_NIR_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("NIR")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("NIR")),
               aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("NIR")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 85) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "tomato")))


# NIR CV
(p_NIR_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("NIR")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("NIR")),
               aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("NIR")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "tomato")))

# IR mean
(p_IR_mean <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("IR")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("IR")),
               aes(x=type, y=spec_mean, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("IR")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 85) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "darkgrey")))


# IR CV
(p_IR_CV <- ggplot() +
    geom_flat_violin(data = subset(collison_wavelength, region %in% c("IR")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(collison_wavelength, region %in% c("IR")),
               aes(x=type, y=CV, colour=plot),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(collison_wavelength, region %in% c("IR")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_color_collison +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "darkgrey")))


#  facet plot for spectral mean and cv ----

# STILL NEED TO ADD AVERAGE OF ENTIRE VEGETATION TYPE 

# Move to a new page
grid.newpage()        

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 5)))

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

# spectral mean model

# model 2018 + 2019 HE & KO and mixed

# histogram 
(hist <- ggplot(QHI_2018_2019_small, aes(x = spec_mean)) +
   geom_histogram() +
   theme_classic())

# does not converge
summary(lmer(data = QHI_2018_2019_small, spec_mean ~ type + year + (1|plot))) # does not converge

# model only 2018+2019 only HE and KO

(hist <- ggplot(collison_2018_2019_small, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())

# linear model for H1

m_H1a <- (lmer(data = collison_2018_2019_small, spec_mean ~ type + year + (1|plot))) # (type-1) changes intercpt to HE 

# temporary additon of mixed
m_H1a <- (lmer(data = QHI_2018_2019_small, spec_mean ~ type + year + (1|plot))) # (type-1) changes intercpt to HE 


summary(m_H1a)

plot(m_H1a)
qqnorm(resid(m_H1a))
qqline(resid(m_H1a))  # points fall nicely onto the line - good!

# Visualises random effects 
(re.effects <- plot_model(m_H1a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H1a, show.values = TRUE))

# gpreditct by type
ggpredict(m_H1a, terms = c("type", "year"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

pred.mm <- ggpredict(m_H1a, terms = c("type")) %>%  # this gives overall predictions for the model
  rename(type = x) %>%
  mutate(type = as.character(type))

H1a_prediction <- collison_wavelength %>%
  group_by(type, wavelength) %>%
  summarise(reflectance = mean(spec_mean))

# attempt to visualize model prediciton (needs work and thinking)
H1a_prediction <- left_join(H1a_prediction, pred.mm, by = "type") %>%
  mutate(reflectance = case_when(type == "HE" ~reflectance,
                                 type == "KO" ~reflectance + predicted))

ggplot(H1a_prediction, aes(x = wavelength, y = reflectance, group = type, color = type)) + 
  geom_line(alpha = 0.7, size=1.) + 
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(x = "Wavelength (mm)", y = "Reflectance") +
  theme_cowplot() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#ffa544", "#2b299b"))
theme_rgb_mean


biomass_KO_preds_df <- cbind.data.frame(lower = biomass_KO_preds_df[,1], 
                                        mean = biomass_KO_preds_df[,2], upper = biomass_KO_preds_df[,3], year = seq(1:20))

veg.cover <- ggplot() +
  geom_point(data = biomass_hits, aes(x = YEAR, y = Biomass, colour = factor(SUBSITE)), alpha = 0.8, size = 4) +
  scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
  scale_fill_manual(values = c("#ffa544","#2b299b")) +
  scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017, 2018)) +
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5, 10)) +
  geom_ribbon(data = biomass_HE_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
              fill = "#ffa544", alpha = 0.2) +
  geom_line(data = biomass_HE_preds_df, aes(x = year + 1998, y = mean), colour = "#ffa544") +
  geom_ribbon(data = biomass_KO_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
              fill = "#2b299b", alpha = 0.2) +
  geom_line(data = biomass_KO_preds_df, aes(x = year + 1998, y = mean), colour = "#2b299b") +
  theme_QHI() +
  theme(legend.position = c(0.1, 0.95), 
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  labs(x = "", y = "Vegetation cover index\n", title = "(a) Vegetation cover\n")

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_line(aes(x = x, y = predicted + 25.5348)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted + 25.5348 - std.error, ymax = predicted + 25.5348 + std.error), 
                fill = "lightgrey", alpha = 0.5) +
    geom_point(data = QHI_spec_plot_2019,                      # adding the raw data (scaled values)
               aes(x = bareground, y = spec_mean, colour = type)) + 
    
    theme_cowplot()
)


# CV model

# QHI 2018 + 2019 model
(hist <- ggplot(QHI_2018_2019_small, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

lmer(data = QHI_2018_2019_small, CV ~ (type-1) + year + (1|plot))
# does converge but for consistence should leave out


# collison 2018 + 2019 model
(hist <- ggplot(collison_2018_2019_small, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

m_H1b <- lmer(data = collison_2018_2019_small, CV ~ type + year + (1|plot))

# temporary addition of mixed
m_H1b <- lmer(data = QHI_2018_2019_small, CV ~ type + year + (1|plot))


summary(m_H1b)

plot(m_H1b)
qqnorm(resid(m_H1b))
qqline(resid(m_H1b)) 

# Visualises random effects 
(re.effects <- plot_model(m_H1b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H1b, show.values = TRUE))

# gpreditct by type
ggpredict(m_H1b, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()


# H3 models (band selection) ----

# linear model with supervised band selection 2018+2019

m_H3a <- lmer(data = supervised_band_selection, spec_mean ~ type + year + (1|plot))

summary(m_H3a)

plot(m_H3a)
qqnorm(resid(m_H3a))
qqline(resid(m_H3a)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3a, show.values = TRUE))


# CV

(hist <- ggplot(supervised_band_selection, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3b <- lmer(data = supervised_band_selection, CV ~ type + year + (1|plot))

summary(m_H3b)

plot(m_H3b)
qqnorm(resid(m_H3b))
qqline(resid(m_H3b)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3b, show.values = TRUE))


# linear model with supervised band selection only 2019

supervised_band_selection_2019 <- supervised_band_selection %>%
  filter(year == 2019)

m_H3c <- lmer(data = supervised_band_selection_2019, spec_mean ~ type + (1|plot))

summary(m_H3c)

plot(m_H3c)
qqnorm(resid(m_H3c))
qqline(resid(m_H3c)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3c, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3c, show.values = TRUE))


# CV

(hist <- ggplot(supervised_band_selection_2019, aes(x = CV)) +
    geom_histogram() +
    theme_classic())


# models with supervised band selection for dimention reduction 

m_H3d <- lmer(data = supervised_band_selection_2019, CV ~ type + (1|plot))

summary(m_H3d)

plot(m_H3d)
qqnorm(resid(m_H3d))
qqline(resid(m_H3d)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3d, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3d, show.values = TRUE))


# ISI band selecrtion models

(hist <- ggplot(lowD, aes(x = spec_mean)) +
   geom_histogram() +
   theme_classic())

# linear model with ISI band selection (2019 only)

m_H3e <- lmer(data = QHI_lowD, spec_mean ~ type + year + (1|plot))

summary(m_H3e)

plot(m_H3e)
qqnorm(resid(m_H3e))
qqline(resid(m_H3e)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3e, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3e, show.values = TRUE))


# CV

(hist <- ggplot(lowD, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3f <- lmer(data = lowD, CV ~ type + (1|plot))

summary(m_H3f)

plot(m_H3f)
qqnorm(resid(m_H3f))
qqline(resid(m_H3f)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3f, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3f, show.values = TRUE))


# combined model vis

# spectral mean
(p_H3a <- dwplot(list(m_H1a, m_H3a, m_H3c, m_H3e), 
                 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H3a, path = "figures", filename = "H3_models_mean.png", height = 10, width = 12)

# CV
# effect sizes and error dont seem to correspond with model summary...
(p_H3b <-dwplot(list(m_H1b, m_H3b, m_H3d, m_H3f), 
                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H3b, path = "figures", filename = "H3_models_cv.png", height = 10, width = 12)

grid.arrange(p_H3a, p_H3b)  


# If needed I could use ggpredict to creat boxplot of predicted spec_mean and cv by VT by Model (but might not be compatable with lme4)
# https://strengejacke.github.io/ggeffects/reference/ggpredict.html

ggpredict(m_H3a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

ggpredict(m_H3b, terms = c("type", "year"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

dat <- ggpredict(H3a, terms = c("c172code", "c161sex"))
ggplot(H3a, aes(type, predicted, colour = group)) +
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  ) +
  scale_x_discrete(breaks = 1:3, labels = get_x_labels(dat))



#  QHI PCA ----

# for all QHI measurements(2019)
pca <- QHI_small 

# detailed pca; adapted from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#pca-data-format
res.pca <- PCA(pca[,c(5,7)], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca results
print(res.pca)

# eigen values

eig.val <- get_eigenvalue(res.pca)
eig.val


# grouped by elipsise
(p_pca_QHI <- fviz_pca_biplot(res.pca,
                           geom.ind = "point", # show points only (nbut not "text")
                           col.ind = "black", # color by groups
                           pointshape = 21,
                           fill.ind = QHI_small$type, # color by groups
                           palette = c("#ffa544", "#2b299b", "gray65"),
                           addEllipses = TRUE, # Concentration ellipses
                           # ellipse.type = "confidence",
                           ellipse.level = 0.95, # confidence level specification
                           mean.point = TRUE, # braycenter mean point
                           legend.title = "Groups",
                           axes.linetype = "dashed",
                           xlab = "PC1", ylab = "PC2", 
                           ggtheme = theme_cowplot()))

ggsave(p_pca_QHI, path = "figures", filename = "QHI_pca.png", height = 10, width = 12)



# pca for all QHI measurements, with band selection (2019)
pca_lowD <- QHI %>%
  filter(wavelength %in% ISI_band_selection$wavelength) %>%
  group_by(type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

res.pca_lowD <- PCA(pca_lowD[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca results
print(res.pca_lowD)

# eigen values

eig.val <- get_eigenvalue(res.pca_lowD)
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

# pca 
(p_pca <- fviz_pca_biplot(res.pca_lowD,
                       geom.ind = "point", # show points only (nbut not "text")
                       pointshape = 21,
                       fill.ind = QHI_small$type, # color by groups
                       col.ind = QHI_small$type, # color by groups
                       palette = c("#ffa544", "#2b299b", "gray65"),
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       legend.title = "Groups",
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2", 
                       ggtheme = theme_cowplot()))

ggsave(p_pca, path = "figures", filename = "QHI_lowD_pca.png", height = 10, width = 12)

# biplot
(p_pca <- fviz_pca_biplot(res.pca_lowD,
                          repel = TRUE, 
                          geom.ind = "point", # show points only (nbut not "text")
                          pointshape = 21,
                          fill.ind = QHI_small$type, # color by groups
                          col.ind = QHI_small$type, # color by groups
                          palette = c("#ffa544", "#2b299b", "gray65"),
                          addEllipses = TRUE, # Concentration ellipses
                          # ellipse.type = "confidence",
                          ellipse.level = 0.95, # confidence level specification
                          mean.point = TRUE, # braycenter mean point
                          legend.title = "Groups",
                          axes.linetype = "dashed",
                          xlab = "PC1", ylab = "PC2", 
                          ggtheme = theme_cowplot()))


# multi year PCA
res.pca_QHI_2018_2019 <- PCA(QHI_2018_2019_small[,c(5,7)], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca 
(p_pca <- fviz_pca_ind(res.pca_QHI_2018_2019,
                       geom.ind = "point", # show points only (nbut not "text")
                       pointshape = 21,
                       col.ind = "black",
                       fill.ind = QHI_2018_2019_small$type, # color by groups
                       #palette = c("#ffa544", "#2b299b", "gray65"),
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       legend.title = "Groups",
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2", 
                       ggtheme = theme_cowplot()))

ggsave(p_pca, path = "figures", filename = "QHI_lowD_biplot.png", height = 10, width = 12)

# multiyear pca by year and vegtype

(p_pca_veg_year <- fviz_pca_ind(res.pca_QHI_2018_2019,
                                geom.ind = "point", # show points only (nbut not "text")
                                fill.ind = QHI_2018_2019_small$type_year, # color by groups
                                color.ind = QHI_2018_2019_small$type_year,
                                pointshape = 21,
                                palette = c( "tomato", "#ffa544", "purple", "#2b299b", "gray65"),
                                addEllipses = TRUE, # Concentration ellipses
                                # ellipse.type = "confidence",
                                ellipse.level = 0.95, # confidence level specification
                                mean.point = TRUE, # braycenter mean point
                                legend.title = "Groups",
                                axes.linetype = "dashed",
                                xlab = "PC1", ylab = "PC2", 
                                ggtheme = theme_cowplot()))

ggsave(p_pca_veg_year, path = "figures", filename = "QHI_2018-2019_pca.png", height = 10, width = 12)



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
         type = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*")))


names(PS2) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "type", "plot")

PS2 <- PS2 %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         type = as.factor(type)) %>%
  drop_na(reflectance) %>% 
  drop_na(wavelength) %>%
  drop_na(reference) %>%
  drop_na(target)  %>%
  # remove erronious measurments from spectrometer 
  filter(between(wavelength, 400, 985),
         !id %in% c(148:171, 196, 210:211, 250:259))

# PS2 vis -------


# single wavelengths VT
(p_PS2 <-  ggplot(PS2, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
   geom_line(alpha = 0.3) + 
   theme_cowplot() +
   labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
   theme(legend.position = "right") +
   scale_color_manual(values = c("#ffa544", "#2b299b")) +
   guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)


PS2_small <- PS2 %>%
  group_by(type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))


# violin of mean by vegetation type
ggplot(PS2_small, aes(x=type, y=spec_mean, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  theme_cowplot()

# cloud of mean by vegetation type
(p_PS2 <- ggplot(PS2_small, aes(x=type, y=spec_mean, fill=type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(position = position_jitter(width = .05), size = .8) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

# violin of cv by vegtation type
ggplot(PS2_small, aes(x=type, y=CV, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

ggplot(PS2_small, aes(x=type, y=CV, fill=type)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
  geom_point(position = position_jitter(width = .05), size = .8) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

# ggsave(p_QHI, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)

# group by wavelength 
PS2_wavelength <- PS2 %>%
  group_by(type, plot, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# GD advice: split full spec into regions (via background colors) and make seperate raincloud plot at each spec_region. (for full snazzyness add color of spec_region to backround)
#plot spectral mean
(p_PS2 <- ggplot(PS2_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    theme_cowplot() +
    theme(legend.position = "right") +
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "\nWavelength (mm)", y = "CV\n"))
#ggsave(p_QHI, path = "figures", filename = "spec_sig_plot.png", height = 8, width = 10)


## SMOOTHING NOT CORRECT
ggplot(PS2_wavelength, aes(x = wavelength, y = spec_mean, group=type, color = type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_cowplot() +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "mean reflectance\n")

# plot CV

ggplot(PS2_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
  geom_line(alpha = 0.9) + 
  theme_cowplot() +
  theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(x = "\nWavelength (mm)", y = "CV\n")

# SMOOTHING NOT CORRECT
ggplot(PS2_wavelength, aes(x = wavelength, y = CV, group=type, color = type)) + 
  geom_smooth(alpha = 0.2, se=TRUE) + 
  theme_cowplot() +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "CV\n")


# PS2 PCA ----

pca_PS2 <- PS2_small

res.pca_PS2 <- PCA(pca_PS2[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)


# PS2 pca by vegtype

# PS2 pca 
(p_PS2 <- fviz_pca_ind(res.pca_PS2,
                       geom.ind = FALSE, # show points only (nbut not "text")
                       col.ind = PS2_small$type, # color by groups
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       legend.title = "Groups",
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2", 
                       ggtheme = theme_cowplot()))
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
                       ggtheme = theme_cowplot()))

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
                ggtheme = theme_cowplot())




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
         type = case_when(grepl("HE|KO", FileName)   ~ 
                            stringr::str_extract(FileName, "HE|KO")),
         # might need to split to just number...
         plot = case_when(grepl("HE|KO|PS2", FileName)   ~ 
                            stringr::str_extract(FileName, "HE\\d*|KO\\d*|PS2")))

names(QHI_PS2plot) <- c("name", "wavelength", "reference", "target", "reflectance", "id", "type", "plot")

QHI_PS2plot <- QHI_PS2plot %>%
  mutate(wavelength = parse_number(as.character(wavelength))/100,
         reflectance = parse_number(as.character(reflectance))/100,
         reference = parse_number(as.character(reference))/100,
         target = parse_number(as.character(target))/100,
         type = as.factor(type)) %>%
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
  mutate(type =  fct_explicit_na(type, na_level = "NA")) %>%
  group_by(type, plot, id) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))


# PS2 pca

res.pca_PS2 <- PCA(PS2_small[,4:5], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca results
print(res.pca_PS2)

# grouped by elipsise
(p_pca_PS2 <- fviz_pca_ind(res.pca_PS2,
                           geom.ind = c("point"), # show points only (nbut not "text")
                           col.ind = PS2_small$type, # color by groups
                           palette = c("#ffa544", "#2b299b"),
                           
                           addEllipses = TRUE, # Concentration ellipses
                           # ellipse.type = "confidence",
                           legend.title = "Groups"))

ggsave(p_pca_PS2, path = "figures", filename = "PS2_pca.png", height = 10, width = 12)



# H2 model ----

# correlation plot full

correlation <- cor(collison_spec_plot_small[,c(6, 9:16)])

print(collison_spec_plot_small)


(p_corr <- corrplot(correlation, method="circle", type="upper", #order="hclust",
                    tl.srt=45, tl.col="black", diag = FALSE, order="hclust", col=brewer.pal(n=10, name="RdYlBu")))

# correlation plot for model

correlation_small <- cor(collison_spec_plot_small[,c(6, 9:10, 15:16)])

(p_corr <- corrplot(correlation_small, method="circle", type="upper", #order="hclust",
                    tl.srt=45, tl.col="black", diag = FALSE, col=brewer.pal(n=10, name="RdYlBu")))


# ugly alternative 
#install.packages("PerformanceAnalytics")
#library("PerformanceAnalytics")

chart.Correlation(correlation, histogram=TRUE, pch=19)


# spectral mean 
(hist <- ggplot(collison_spec_plot_small, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())


# linear model for H2

# spectral mean

collison_spec_plot_small_2019 <- collison_spec_plot_small %>% filter(year == 2019) #%>%
# to normalize evenness
mutate(evenness = (evenness- min(evenness))/(max(evenness)-min(evenness)))

# to scale all continous varibales
collison_spec_plot_small_2019$richness <- scale(collison_spec_plot_small_2019$richness)
collison_spec_plot_small_2019$evenness <- scale(collison_spec_plot_small_2019$evenness)
collison_spec_plot_small_2019$bareground <- scale(collison_spec_plot_small_2019$bareground)

str(collison_spec_plot_small_2019)

m_H2a <- lmer(data = collison_spec_plot_small_2019, spec_mean ~ (type-1) * (type*richness) + (type*evenness) + (type*bareground) + (1|plot))


summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 

# Visualises random effects 
(re.effects <- plot_model(m_H2a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2a, show.values = TRUE))


# attempting to visulize model output
ggpredict(data = m_H2a, c("type", "richness")) %>% plot()



e <- allEffects(m_H2a)
print(e)

plot(e)

e.df <- as.data.frame(e)

ggplot(fe.effects)


ggplot(e.df$`type:richness`, aes(x=richness, y=fit, color=type, ymin=lower, ymax=upper)) + 
  geom_pointrange(position=position_dodge(width=.1), mapping = NULL) + 
  geom_ribbon(data = e.df$`type:richness`, aes(x = richness, ymin = lower, ymax = upper, 
                                               fill = type), alpha = 0.2) +
  geom_line(data = e.df$`type:richness`, aes(x = richness, y = fit)) +
  xlab("Richness") + 
  ylab("Spectral mean") +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
    geom_line(aes(x = x, y = predicted + 25.5348)) +          # slope
  
  
geom_ribbon(data = e.df$`type:richness`, aes(x = year + 1998, ymin = lower, ymax = upper), 
            fill = "#ffa544", alpha = 0.2) +
  geom_line(data = biomass_HE_preds_df, aes(x = year + 1998, y = mean), colour = "#ffa544") +
  geom_ribbon(data = biomass_KO_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
              fill = "#2b299b", alpha = 0.2) +
  geom_line(data = biomass_KO_preds_df, aes(x = year + 1998, y = mean), colour = "#2b299b") +



ggpredict(m_H2a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

(p_H2a_rich <- ggpredict(m_H2a, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

(p_H2a_even <- ggpredict(m_H2a, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

(p_H2a_ground <- ggpredict(m_H2a, terms = c("bareground", "type"), type = "fe" ) %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())


grid.arrange(p_H2a_rich, p_H2a_even, p_H2a_ground, nrow = 1)



# CV

(hist <- ggplot(collison_spec_plot_small_2019, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

m_H2b <- lmer(data = collison_spec_plot_small_2019, 
              CV ~ (type-1) + (type*richness) + (type*evenness) + (type*bareground) + (1|plot))

summary(m_H2b)

plot(m_H2b)
qqnorm(resid(m_H2b))
qqline(resid(m_H2b)) 

# Visualises random effects 
(re.effects <- plot_model(m_H2b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2b, show.values = TRUE))


ggpredict(m_H2b, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

(p_H2b_rich <- ggpredict(m_H2b, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    ylim(0.3,1) +
    theme_cowplot())

(p_H2b_even <- ggpredict(m_H2b, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

(p_H2b_ground <- ggpredict(m_H2b, terms = c("bareground", "type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

grid.arrange(p_H2a_rich, p_H2a_even, p_H2a_ground, 
             p_H2b_rich, p_H2b_even, p_H2b_ground, nrow = 2)

# alternative with interaction terms (base r)

# spectral mean (richness, eveness, and bareground)
p_H2a_base <- allEffects(m_H2a)

print(p_H2a_base)
plot(p_H2a_base)

# spectral diverstiy (richness, eveness, and bareground)
p_H2b_base <- allEffects(m_H2b)

print(p_H2b_base)
plot(p_H2b_base)



# H2 plot PCA ----

# only 2019

pca_H2 <- collison_spec_plot_small_2019

head(pca_H2)

# ncp = 10 (10 variables)
res.pca_H2 <- PCA(collison_spec_plot_small_2019[,c(6, 9:16)], scale.unit = TRUE,
                  ncp = 10, graph = TRUE)

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

(p_pca <- fviz_pca_biplot(res.pca_H2,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = collison_spec_plot_small_2019$plot_unique, # color by groups
                          pointshape = 21, 
                          col.ind = "transparent",
                          # palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
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
  ggpubr::fill_palette(c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"))+      # Indiviual fill color
  ggpubr::color_palette("Dark2")      # Variable colors


# pca 2018 & 2019

pca_H2_2018_2019 <- collison_spec_plot_small

head(pca_H2)

# ncp = 10 (10 variables)
res.pca_H2_2018_2019 <- PCA(pca_H2_2018_2019[,c(6, 9:16)], scale.unit = TRUE, ncp = 9, graph = TRUE)

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
corrplot(var$contrib, is.corr=FALSE, axes = 1:2)    


fviz_pca_var(res.pca_H2_2018_2019, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# biplot grouped by year 
(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = collison_spec_plot_small$year, # color by groups
                          coll.ind = collison_spec_plot_small$year, # color by groups
                          pointshape = 21, 
                          #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
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
#ggpubr::fill_palette(c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"))+      # Indiviual fill color


# biplot grouped by type 
(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = collison_spec_plot_small$type, # color by groups
                          pointshape = 21, 
                          #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
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

# pca by type and year
# 
collison_spec_plot_small <- collison_spec_plot_small %>%
  # add site variable 
  unite(site, c(type,year))

(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = t$site, # color by groups
                          pointshape = 21, 
                          #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
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

# H4 spatial autocorrelation ----

# spatial variogram

QHI_spatial <- collison_small %>% 
  ungroup() %>%
  #filter(type == "HE") %>%
  mutate(x = as.numeric(group_indices(., plot)),
         y = as.numeric(group_indices(., plot)),
         # add spatial distance between HE and KO plots
         x = case_when(plot == "HE2" ~ 10,
                       plot == "HE3" ~ 11,
                       plot == "HE4" ~ 26,
                       plot == "HE5" ~ 45,
                       plot == "HE6" ~ 50,
                       plot == "KO1" ~ 1000,
                       plot == "KO2" ~ 1001,
                       plot == "KO3" ~ 1012,
                       plot == "KO4" ~ 1013),
         y = case_when(plot == "HE2" ~ 1.5,
                       plot == "HE3" ~ 1.6,
                       plot == "HE4" ~ 2.6,
                       plot == "HE5" ~ 3.6,
                       plot == "HE6" ~ 3.8,
                       plot == "KO1" ~ 1,
                       plot == "KO2" ~ 1.5,
                       plot == "KO3" ~ 3,
                       plot == "KO4" ~ 3.5)) %>%
  # select only relavent colunms for variogram?
  select(x, y, spec_mean)


ggplot(QHI_spatial, aes(x = x, y = y)) +
  geom_point()

ggplot(QHI_spatial, aes(x = x, y = spec_mean)) +
  geom_point()


histogram(QHI_spatial$spec_mean)
histogram(QHI_spatial$spec_mean )
histogram

# assign cordinates
coordinates(QHI_spatial) = ~x+y

# variogram model
(v0 = variogram(spec_mean~1, QHI_spatial))

# fit under 4 different models
(v.fit0 = fit.variogram(v0, vgm(c("Exp", "Mat", "Sph", "Ste")), fit.kappa = TRUE)) # ste is selected to be the best

#semivariance
?variogramLine
plot(variogramLine(v.fit0, maxdist = 150, n=20))

# covariance
plot(variogramLine(v.fit0, maxdist = 150, n=20, covariance = TRUE))

# ggplot semivariance

preds = variogramLine(v.fit0, maxdist =  160)
head(preds)
min(preds$gamma)
max(preds$gamma)
print(preds)

(p_variogram <- ggplot(v0, aes(x = dist, y = gamma)) +
    geom_line(data = preds) +
    #  geom_point() +
    geom_vline(xintercept = 100, linetype="dashed", color = "red") +
    geom_vline(xintercept = 50, linetype="dotted") + 
    #  xlim(0,15) +
    xlab("distance (m)") +
    coord_cartesian(xlim = c(8, 130)) + 
    scale_x_continuous(breaks=seq(0, 150, 10)) +
    ylim(0,70) +
    #coord_cartesian(xlim = c(0.7, 15), ylim = c(2.5, 63)) +
    theme_cowplot())

ggsave(p_variogram, path = "figures", filename = "variogram.png", height = 10, width = 12)

# plot from tutoral that doesnt really make sense...
plot(variogramLine(vgm(1, "Ste", 1, kappa = 5), 10), type = 'l')




