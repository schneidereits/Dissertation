# QHI 2018_2019 visualiuation
# shawn schneidereit edit
# 5.3.2020


#  QHI vis -------

# for subsets of measurements (was used for data sorting of specific measurments)
(ggplot(subset(spec_2019 ,id %in% c(60:50))) +
   aes(x = wavelength, y = reflectance, group = id, color = id)) + 
  geom_line(alpha = 0.9) + 
  theme_cowplot() +
  labs(x = "\nWavelength (mm)", y = "Reflectance\n") +
geom_hline( yintercept= c(.50,.70), color = "red") +
facet_wrap(.~id) 


#####  2019 data
# single wavelengths VT (2019)
(p_QHI <-  ggplot(spec_2019, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
    geom_line(alpha = 0.3) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    # scale_color_viridis_d(option = "C") +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)

# single wavelengths at plot level (2019)
(p_QHI <-  ggplot(spec_2019, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_QHI +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)

# violin of mean by vegtype
ggplot(spec_2019_small, aes(x=type, y=spec_mean, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5, aes(fill = type)) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# cloud of mean by vegetation type

(p_QHI_cloud_mean <- ggplot(spec_2019_small, aes(x=type, y=spec_mean, fill=type)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8, trim = F ) +
    geom_point(data = spec_2019_small, aes(x=type, y=spec_mean, colour=plot_unique),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  #  scale_color_QHI +
    theme_cowplot() +
    labs(y = "Reflectance") +
    theme(legend.position = "none"))
#ggsave(p_QHI, path = "figures", filename = "cloud_specmean.png", height = 8, width = 10)

# violin of cv by vegtation type
ggplot(spec_2019_small, aes(x=type, y=CV, fill=type)) + 
  geom_violin(trim=FALSE, alpha = .5) +
  geom_point(position = position_jitter(0.05)) +
  geom_boxplot(width=0.2, fill="white", alpha = 0.3) +
  scale_fill_manual(values = c("#ffa544", "#2b299b", "gray65")) +
  theme_cowplot()

# cloud of cv by vegtation type
(p_QHI_cloud_cv <- ggplot() +
    geom_flat_violin(data = spec_2019_small, aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8, trim = F ) +
    geom_point(data = spec_2019_small, aes(x=type, y=CV, colour=plot_unique),
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
(p_QHI_specmean <- ggplot(spec_2019_wavelength, aes(x = wavelength, y = spec_mean, group = plot, color = plot)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "none") +
    scale_color_QHI +
    theme_rgb_mean)

# plot CV by plot
(p_QHI_CV <- ggplot(spec_2019_wavelength, aes(x = wavelength, y = CV, group = plot, color = plot)) + 
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
(p_QHI <-  ggplot(spec_2018_2019, aes(x = wavelength, y = reflectance, group = id, color = type)) + 
    geom_line(alpha = 0.3) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "Reflectance\n")+ 
    theme(legend.position = "right") +
    scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    # scale_color_viridis_d(option = "C") +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_QHI, path = "figures", filename = "spec_sig.png", height = 10, width = 12)

# spectral signatures by plot 2018 + 2019
(p_QHI <-  ggplot(spec_2018_2019, aes(x = wavelength, y = reflectance, group = id, color = plot)) + 
    geom_line(alpha = 0.2) + 
    theme_cowplot() +
    labs(x = "\nWavelength (mm)", y = "reflectance\n")+ 
    theme(legend.position = "right") +
    # scale_color_QHI +
    guides(colour = guide_legend(override.aes = list(size=5))))
#ggsave(p_test_3, path = "figures", filename = "spec_sig.png", height = 8, width = 10)


# spectral signatures of mean reflectance by group and year (2018 2019)

(p_spec_2018_2019_mean <- ggplot(spec_2018_2019_wavelength, aes(x = wavelength, y = spec_mean, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    ggpubr::color_palette(c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_mean)

# spectral signatures of CV by group and year (2018 2019)
(p_spec_2018_2019_cv <- ggplot(spec_2018_2019_wavelength, 
                              aes(x = wavelength, y = CV, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "CV") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_CV)

# spectral signatures of CV by group and year (2018 2019)
(p_spec_2018_2019_cv <- ggplot(spec_2018_2019_wavelength, 
                              aes(x = wavelength, y = CV, group = plot_unique, color = type_year)) + 
    geom_line(alpha = 0.7, size=1.) + 
    guides(colour = guide_legend(override.aes = list(size=5))) +
    labs(x = "Wavelength (mm)", y = "Reflectance") +
    theme_cowplot() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_CV)


# cloud of spec mean 2018+2019
(p_QHI_cloud_mean <- ggplot() + 
    geom_flat_violin(data = spec_2018_2019_small, aes(x=type, y=spec_mean, fill=type_year),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8, trim=F) +
    geom_point(data = spec_2018_2019_small, aes(x=type, y=spec_mean, colour=year),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(data = spec_2018_2019_small, aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Mean Reflectance") +
    theme_cowplot() +
theme(legend.position = c(.11,0.6)))
  
#ggsave(p_QHI, path = "figures", filename = "cloud_spec_mean_2018_2019.png", height = 8, width = 10)


# cloud of spec diversity 2018 + 2019
(p_QHI_cloud_cv <- ggplot() +
    geom_flat_violin(data = spec_2018_2019_small, aes(x=type, y=CV, fill=type_year),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8, trim=F) +
    geom_point(data = spec_2018_2019_small, aes(x=type, y=CV, colour=year),
               position = position_jitter(width = .15), size = 2) +
    geom_boxplot(data = spec_2018_2019_small, aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Spectral Diversity  \n (CV)") +
    ylim(0,0.4) +
    theme_cowplot()+ 
    theme(legend.position = "none"))
#ggsave(p_QHI, path = "figures", filename = "cloud_CV.png", height = 8, width = 10)




#ggsave(p_QHI, path = "figures", filename = "spec_sig_plot.png", height = 8, width = 10)


# just 2018 2019 by vegtype
spec_2018_2019_wavelength_plot <- spec_2018_2019 %>%
  mutate(type_year = paste(type, year, sep="_")) %>%
  group_by(type, wavelength, year, type_year) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance))

# spectral sig of mean reflectance
(p_QHI_specmean <- ggplot(spec_2018_2019_wavelength_plot, 
                          aes(x = wavelength, y = spec_mean, 
                              group = type_year, color = type_year)) + 
    geom_line(size=1) + 
    theme_cowplot() +
    labs(x = "Wavelength (mm)", y = "Mean Reflectance") +
    # scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_mean +
    theme(legend.position = "none" )) #c(0.05,0.7)

#ggsave(p_QHI, path = "figures", filename = "CV_plot.png", height = 8, width = 10)

# spectral sig of mean spectral diversity 
(p_QHI_CV <- ggplot(spec_2018_2019_wavelength_plot, 
                    aes(x = wavelength, y = CV, 
                        group = type_year, color = type_year)) + 
    geom_line(size=1) + 
    theme_cowplot() +
    labs(x = "Wavelength (mm)", y = "Spectral Diversity  \n (CV)") +
    # scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    theme_rgb_CV +
    theme(legend.position = "none" )) #c(0.05,0.7))

#H1 figure ----

# Move to a new page
grid.newpage()        

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2)))

# Arrange the plots
print(p_QHI_cloud_mean + rremove("legend"), vp = define_region(row = 1, col = 1))   
print(p_QHI_cloud_cv, vp = define_region(row = 1, col = 2))
print(p_QHI_specmean , vp = define_region(row = 2, col = 1))
print(p_QHI_CV + rremove("legend"), vp = define_region(row = 2, col = 2))



# old figure with PCA
# Move to a new page
grid.newpage()        

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 2)))

# Arrange the plots
print(p_QHI_cloud_mean + rremove("legend"), vp = define_region(row = 1:2, col = 1))   
print(p_QHI_cloud_cv, vp = define_region(row = 1:2, col = 2))
print(p_QHI_specmean , vp = define_region(row = 3:4, col = 1))
print(p_QHI_CV + rremove("legend"), vp = define_region(row = 3:4, col = 2))
print(p_pca_veg_year , vp = define_region(row = 5:7, col = 1:2))

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

spec_2018_2019_region <- spec_2018_2019 %>%	
  #remove mixed veg
  filter(!type == "mixed") %>%
  group_by(type, plot, wavelength, year) %>%		
  summarise(spec_mean = mean(reflectance),		
            CV = mean(sd(reflectance)/mean(reflectance))) %>%		
  mutate(region = case_when(between(wavelength, 400, 500) ~ "blue",		
                            between(wavelength, 500, 600) ~ "green",		
                            between(wavelength, 600, 680) ~ "red",		
                            between(wavelength, 680, 800) ~ "NIR",		
                            between(wavelength, 800, 1000) ~ "IR"))

# blue mean
(p_blue_mean <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("blue")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("blue")),
               aes(x=type, y=spec_mean, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("blue")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 0.8) +
  #  scale_color_collison +
    ylab("Reflectance") +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "#cfe2fd")))

# blue CV
(p_blue_CV <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("blue")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("blue")),
               aes(x=type, y=CV, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("blue")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    ylab("Spectral diversity (CV)") +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "#cfe2fd")))

# green mean
(p_green_mean <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("green")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("green")),
               aes(x=type, y=spec_mean, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("green")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 0.8) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "lightgreen")))


# green CV
(p_green_CV <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("green")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("green")),
               aes(x=type, y=CV, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("green")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "lightgreen")))

# red mean
(p_red_mean <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("red")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("red")),
               aes(x=type, y=spec_mean, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("red")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 0.8) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "red")))


# red CV
(p_red_CV <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("red")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("red")),
               aes(x=type, y=CV, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("red")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "red")))

# NIR mean
(p_NIR_mean <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("NIR")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("NIR")),
               aes(x=type, y=spec_mean, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("NIR")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0, 0.8) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "tomato")))


# NIR CV
(p_NIR_CV <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("NIR")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("NIR")),
               aes(x=type, y=CV, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("NIR")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "tomato")))

# IR mean
(p_IR_mean <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("IR")), 
                     aes(x=type, y=spec_mean, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("IR")),
               aes(x=type, y=spec_mean, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("IR")),
                 aes(x=type, y=spec_mean),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
   
    ylim(0, 0.8) +
    theme_spectra() +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "darkgrey")))

# IR CV
(p_IR_CV <- ggplot() +
    geom_flat_violin(data = subset(spec_2018_2019_region, region %in% c("IR")), 
                     aes(x=type, y=CV, fill=type),
                     position = position_nudge(x = .2, y = 0), alpha=0.5, adjust = .8 ) +
    geom_point(data = subset(spec_2018_2019_region, region %in% c("IR")),
               aes(x=type, y=CV, colour=type),
               position = position_jitter(width = .1), size = 1, alpha =0.5) +
    geom_boxplot(data = subset(spec_2018_2019_region, region %in% c("IR")),
                 aes(x=type, y=CV),
                 width=0.2, fill="white", alpha = 0.3, outlier.shape=NA) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_spectra() +
    ylim(0.1, 0.4) +
    theme(panel.background =  element_rect(fill = "white"),
          plot.background = element_rect(color = "darkgrey")))


#  facet plot for spectral mean and cv ----

# STILL NEED TO ADD AVERAGE OF ENTIRE VEGETATION TYPE 

# Move to a new page
grid.newpage()        

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 5)))

# Arrange the plots
print(p_QHI_specmean, vp = define_region(row = 1:2, col = 1:5))   # Span over two columns
print(p_blue_mean + rremove("legend") + rremove("xlab"), vp = define_region(row = 3:4, col = 1))
print(p_green_mean + rremove("legend") + rremove("xylab"), vp = define_region(row = 3:4, col = 2))
print(p_red_mean + rremove("legend") +  rremove("xylab"), vp = define_region(row = 3:4, col = 3))
print(p_NIR_mean + rremove("legend") + rremove("xylab"), vp = define_region(row = 3:4, col = 4))
print(p_IR_mean + rremove("legend")  + rremove("xylab"), vp = define_region(row = 3:4, col = 5))


# facet plot for spectral CV

# Move to a new page
grid.newpage()

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 5)))

# arranging plots
print(p_QHI_CV, vp = define_region(row = 1:2, col = 1:5))
print(p_blue_CV + rremove("legend") + rremove("xlab"), vp = define_region(row = 3, col = 1))
print(p_green_CV + rremove("legend") + rremove("xylab"), vp = define_region(row = 3, col = 2))
print(p_red_CV + rremove("legend") +  rremove("xylab"), vp = define_region(row = 3, col = 3))
print(p_NIR_CV + rremove("legend") + rremove("xylab"), vp = define_region(row = 3, col = 4))
print(p_IR_CV + rremove("legend")  + rremove("xylab"), vp = define_region(row = 3, col = 5))

