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

supervised_band_selection <- QHI_2018_2019 %>%
  #filter(!type == "mixed") %>% 
  filter(wavelength %in% supervised_band_selection$wavelength) %>%
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

QHI_ISI_tbl <- QHI_ISI_tbl[c("region", "ISI", "relative_ISI",  "wavebands_selected", "selected_ISI")]




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
lowD <- QHI_2018_2019 %>%
  filter(wavelength %in% ISI_band_selection$wavelength) %>%
  group_by(type, plot, id, year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

# reduced dimentionality; product of ISI band selection
QHI_lowD <- QHI_2018_2019 %>%
  # need to sound so that 2019 correspond with selection wavebands 
  mutate(wavelength = round(wavelength, digits = 0)) %>%
  filter(wavelength %in% QHI_ISI_band_selection$wavelength) %>%
  group_by(type, plot, id, year) %>%
  summarise(spec_mean = mean(reflectance),
            CV = mean(sd(reflectance)/mean(reflectance)))

head(QHI_lowD)

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
  #geom_point(QHI_SZU, aes(x=3, y = D_ISIi, color="red")) +
  # hardcode to match total number of selected wavebands USE: sum(QHI_ISI_tbl$wavebands_selected)
  geom_vline(xintercept = 24, linetype="dotted") + # need to pick correct vline
  geom_vline(xintercept = 3, linetype="dashed", color="red") + # need to pick correct vline
  geom_line() +
  labs(x= "Number of bands selected ") +
  theme_cowplot()

#ggsave(p_SZU, path = "figures", filename = "SZU.png", height = 10, width = 12)

