# QHI 2018_2019 field spec band selection
# shawn schneidereit edit
# 5.3.2020


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

supervised_band_selection <- spec_2018_2019 %>%
  #filter(!type == "mixed") %>% 
  filter(wavelength %in% supervised_band_selection$wavelength) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance)) %>%
  # MIGHT WHAT TO IMPROVE BUT I KNOW THIS WORKS...
  group_by(type, plot_unique, year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))
  

# visual representaiton of supervised  selected areas 
 ggplot(spec_2018_2019, aes(x = wavelength, y = reflectance)) + 
   #geom_line(data= spec_2018_2019, aes( x=wavelength, y=reflectance)) + 
    # scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
    theme_cowplot() +
    labs(x = "Wavelength (mm)", y = "Mean Reflectance") +
    theme(legend.position = c(0.05,0.7)) +
   ylim(0,.5) +
   xlim(400,1000) +
  coord_cartesian(ylim =c(0.03, .5)) +
    scale_color_manual(values = c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF", "grey")) +
    annotate("rect", xmin = 430, xmax = 450, alpha = .15,ymin = 0,
              ymax = .5, fill = "blue") + 
   annotate("rect", xmin = 545, xmax = 555, ymin = 0, 
            ymax = .5, alpha = .15, fill = "green") +
   annotate("rect", xmin = 660, xmax =680 , ymin = 0, 
            ymax = .5, alpha = .25, fill = "red") + 
   annotate("rect", xmin = 700, xmax = 725, ymin = 0, 
            ymax = .5, alpha = .15, fill = "tomato") +
   annotate("rect", xmin = 745, xmax = 755, ymin = 0, 
            ymax = .5, alpha = .15, fill = "tomato") +
   annotate("rect", xmin = 920, xmax = 985, ymin = 0, 
            ymax = .5, alpha = .15, fill = "darkgrey")
 
 
   
 
 # visual representaiton of ISI  selected wavebands
 ggplot(spec_2018_2019, aes(x = wavelength, y = reflectance)) + 
   geom_smooth(alpha = 0.2, se=FALSE, color = "black") +
   # scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
   theme_cowplot() +
   labs(x = "Wavelength (mm)", y = "Mean Reflectance") +
   theme(legend.position = c(0.05,0.7)) +
   ylim(0,50) +
   xlim(400,1000) +
   coord_cartesian(ylim =c(3,50)) +
   geom_vline(data =subset(ISI_band_selection, region %in% c("blue")), 
             aes(xintercept= wavelength , color = region, alpha = .1, size=.5))  +
   geom_vline(data =subset(ISI_band_selection, region %in% c("green")), 
              aes(xintercept= wavelength , color = region, alpha = .1,size=.5)) +
   geom_vline(data =subset(ISI_band_selection, region %in% c("red")), 
              aes(xintercept= wavelength , color = region, alpha = .1, size=.6)) +
   geom_vline(data =subset(ISI_band_selection, region %in% c("NIR")), 
              aes(xintercept= wavelength , color = region, alpha = .1, size=.5)) +
   geom_vline(data =subset(ISI_band_selection, region %in% c("IR")), 
              aes(xintercept= wavelength , color = region, alpha = .1, size=.5)) +
   scale_color_manual(values = c("blue", "green", "grey", "tomato", "red")) +
   theme(legend.position = "none")
   
 ggplot(spec_2018_2019, aes(x = wavelength, y = reflectance)) + 
   geom_smooth(alpha = 0.2, se=FALSE, color = "black") +
   # scale_color_manual(values = c("#ffa544", "#2b299b", "gray65")) +
   theme_cowplot() +
   labs(x = "Wavelength (mm)", y = "Mean Reflectance") +
   theme(legend.position = c(0.05,0.7)) +
   ylim(0,50) +
   xlim(400,1000) +
   coord_cartesian(ylim =c(3,50)) +
   theme_rgb_mean

############# 2018 2019 ISI band selection and SZU 

# inelegant solution to selecting only 2018 wavebands that have a matching 2019 evivalent 
# # # REASON: (2018 has a higher resolution that breaks selection algorythem)
# first filter only 2019 
# then round (as 2018 has no significant digits)
# finally filter spec_2018_2019 for band in  (next chunk)
wavelengths_2019_rounded <- spec_2018_2019 %>%
  filter(year =="2019") %>%
  mutate(wavelength = round(wavelength, digits = 0)) 

QHI_ISI <- spec_2018_2019 %>%
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
  summarise(mean_ISI = mean(ISI),
            ISI = sum(ISI))

# number relative ISI (as groups include differnt numbers of wavebands)
QHI_ISI_tbl <- QHI_ISI %>%
  group_by(region) %>%
  count(region) %>%
  left_join(QHI_ISI_tbl)

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

QHI_ISI_tbl <- QHI_ISI_tbl[c("region", "ISI", "mean_ISI",  "wavebands_selected", "selected_ISI")]


QHI_SZU <- QHI_ISI %>%
  arrange(ISI) %>%qhi
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
QHI_lowD <- spec_2018_2019 %>%
  # need to round so that 2019 correspond with selection wavebands 
  mutate(wavelength = round(wavelength, digits = 0)) %>%
  filter(wavelength %in% QHI_ISI_band_selection$wavelength) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance)) %>%
  # MIGHT WHAT TO IMPROVE BUT I KNOW THIS WORKS...
  group_by(type, plot_unique, year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))


head(QHI_lowD)

# need to spruce up https://www.rdocumentation.org/packages/ggpmisc/versions/0.3.3/topics/stat_peaks
# plot of ISI by wavelength and local minima
ggplot(QHI_ISI, aes(x=wavelength, y=ISI)) +
  geom_line() +
  theme_cowplot() +
  geom_point(data = QHI_ISI_band_selection, shape = 1, size =2.6) +
  geom_rug(data = QHI_ISI_band_selection, sides = "b" ) +
  labs(y="InStability Index \n (ISI)") +
  #stat_valleys(span = 3, shape = 1, size = 2, color = "black", fill = NA) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  scale_x_continuous(expand = expand_scale(mult = c(0, .1)),
                     breaks=seq(400, 1000, 100)) +
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
  #geom_point(QHI_SZU, aes(x=3, y = D_ISIi, color="red")) +
  # hardcode to match total number of selected wavebands USE: sum(QHI_ISI_tbl$wavebands_selected)
  geom_vline(xintercept = 24, linetype="dotted") + # need to pick correct vline
  geom_vline(xintercept = 3, linetype="dashed", color="red") + # need to pick correct vline
  coord_cartesian(xlim = c(15,365)) +
  scale_x_continuous(breaks=seq(0,365,50)) +
  geom_line() +
  labs(x= "Number of bands selected ") +
  theme_cowplot()

#ggsave(p_SZU, path = "figures", filename = "SZU.png", height = 10, width = 12)



# ISI band selection and SZU 0NLY 2019

collison_ISI <- spec_2018_2019 %>%
  filter(type %in% c("KO" , "HE"),
         year == 2019) %>%
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
  summarise(mean_ISI = mean(ISI),
            ISI = sum(ISI))

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
lowD_2019selection <- spec_2018_2019 %>%
 # round so that 2018 and 2019 wavelengths match
  mutate(round(wavelength, digits = 0)) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance)) %>%
  # MIGHT WHAT TO IMPROVE BUT I KNOW THIS WORKS...
  group_by(type, plot_unique, year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))

# need to spruce up https://www.rdocumentation.org/packages/ggpmisc/versions/0.3.3/topics/stat_peaks
# plot of ISI by wavelength and local minima
(p_ISI <-  ggplot(collison_ISI, aes(x=wavelength, y=ISI)) +
    geom_line() +
    theme_cowplot() +
    geom_point(data = ISI_band_selection, shape = 1, size =2.6) +
    geom_rug(data = ISI_band_selection, sides = "b" ) +
    labs(y="InStability Index \n (ISI)") +
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


# H2 model compiarison figure


# full spectrum 
(p_full <- ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=type, color = type)) + 
  geom_smooth(alpha = 0.2, se=F) + 
  theme_cowplot() +
  coord_cartesian(ylim =c(0.04, .85)) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n") +
    theme(legend.position = c(0.2,0.7))) 


# manual selection no color
(p_manual <- ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=type, color = type)) + 
  geom_smooth(alpha = 0.2, se=F) + 
  theme_cowplot() +
  coord_cartesian(ylim =c(0.04, .8)) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n") +
    theme(legend.position = "none") +
  annotate("rect", xmin = 400, xmax = 430, alpha = 1,ymin = 0,
           ymax = .85, fill = "white") +
  annotate("rect", xmin = 450, xmax = 545, alpha = 1,ymin = 0,
           ymax = .85, fill = "white") + 
  annotate("rect", xmin = 555, xmax = 660, alpha = 1,ymin = 0,
           ymax = .85, fill = "white") + 
  annotate("rect", xmin = 680, xmax = 700, alpha = 1,ymin = 0,
           ymax = .85, fill = "white") +
  annotate("rect", xmin = 725, xmax = 745, alpha = 1,ymin = 0,
           ymax = .85, fill = "white") + 
  annotate("rect", xmin = 755, xmax = 920, alpha = 1,ymin = 0,
           ymax = .85, fill = "white"))

# automatic selection no color
(p_automatic <- ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=type, color = type)) + 
    geom_smooth(alpha = 0.2, se=F) + 
    theme_cowplot() +
    coord_cartesian(ylim =c(0.04, .8)) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n") +
    theme(legend.position = "none") +
    annotate("rect", xmin = 400, xmax = 402, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 407, xmax = 418, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 422.78, xmax = 440, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 444.27, xmax = 460, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 464.01, xmax = 478, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 483.68, xmax = 490, alpha = 1,ymin = 0,
             ymax = .85, fill = "white")+
    annotate("rect", xmin = 496.75, xmax = 500, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 506.55, xmax = 561, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 568.34, xmax = 672, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 677.48, xmax = 701, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 707.47, xmax = 713, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 719, xmax = 749, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 755, xmax = 779, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 785, xmax = 811, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 816, xmax = 826, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 832, xmax = 847, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 853, xmax = 860, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 866, xmax = 874, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 880, xmax = 908, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 914, xmax = 915, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 921, xmax = 924, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 929, xmax = 941, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") +
    annotate("rect", xmin = 926, xmax = 951, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 956, xmax = 960, alpha = 1,ymin = 0,
             ymax = .85, fill = "white")+ 
    annotate("rect", xmin = 965, xmax = 985, alpha = 1,ymin = 0,
             ymax = .85, fill = "white"))

grid.arrange(p_full, p_manual, p_automatic,
             p_H1a, p_H3a, p_H3e, 
             p_H1b, p_H3b, p_H3f, nrow=3)




# manual selection with color
(p_manual_c <- ggplot(collison_wavelength, aes(x = wavelength, y = spec_mean, group=type, color = type)) + 
    geom_smooth(alpha = 0.2, se=F) + 
    theme_cowplot() +
    coord_cartesian(ylim =c(0.04, .8)) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    labs(x = "\nWavelength (mm)", y = "Mean Reflectance\n") +
    theme(legend.position = "none") +
    annotate("rect", xmin = 400, xmax = 430, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 430, xmax = 450, alpha = .15,ymin = 0,
             ymax = .85, fill = "blue") + 
    annotate("rect", xmin = 450, xmax = 545, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 545, xmax = 555, ymin = 0, 
             ymax = .85, alpha = .15, fill = "green") +
    annotate("rect", xmin = 555, xmax = 660, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 660, xmax =680 , ymin = 0, 
             ymax = .85, alpha = .25, fill = "red") + 
    annotate("rect", xmin = 680, xmax = 700, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 700, xmax = 725, ymin = 0, 
             ymax = .85, alpha = .15, fill = "tomato") +
    annotate("rect", xmin = 725, xmax = 745, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 745, xmax = 755, ymin = 0, 
             ymax = .85, alpha = .15, fill = "tomato") +
    annotate("rect", xmin = 755, xmax = 920, alpha = 1,ymin = 0,
             ymax = .85, fill = "white") + 
    annotate("rect", xmin = 920, xmax = 985, ymin = 0, 
             ymax = .85, alpha = .15, fill = "darkgrey"))
