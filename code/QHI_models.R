# QHI 2018_2019 models
# shawn schneidereit edit
# 5.3.2020

#  H1 model----

# spectral mean model

# model 2018 + 2019 HE & KO and mixed

spec_2018_2019_small$year <- as.factor(spec_2018_2019_small$year)

# histogram 
(hist <- ggplot(spec_2018_2019_small, aes(x = spec_mean)) +
   geom_histogram() +
   theme_classic())

# does not converge
summary(lm(data = spec_2018_2019_small, spec_mean ~ type + year)) # does not converge

# model only 2018+2019 only HE and KO

(hist <- ggplot(spec_2018_2019_small, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())

# linear model for H1

m_H1a <- (lm(data = spec_2018_2019_small, spec_mean ~ (type-1) +year)) # (type-1) changes intercpt to HE 

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
(p_H1a <- ggpredict(m_H1a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot())

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
(hist <- ggplot(spec_2018_2019_small, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

lmer(data = spec_2018_2019_small, CV ~ (type-1) + year + (1|plot))
# does converge but for consistence should leave out


# collison 2018 + 2019 model
(hist <- ggplot(collison_2018_2019_small, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

m_H1b <- lm(data = spec_2018_2019_small, CV ~ type + year)

# temporary addition of mixed
m_H1b <- lmer(data = QHI_2018_2019_small, CV ~ type- + year + (1|plot))


summary(m_H1b)

stargazer(m_H1b, type = "text")


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
supervised_band_selection$year <-  as.factor(supervised_band_selection$year)


m_H3a <- lm(data = supervised_band_selection, spec_mean ~ type + year)

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

m_H3b <- lm(data = supervised_band_selection, CV ~ type + year)

summary(m_H3b)

plot(m_H3b)
qqnorm(resid(m_H3b))
qqline(resid(m_H3b)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3b, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3b, show.values = TRUE))




############ ISI band selecrtion models

(hist <- ggplot(lowD, aes(x = spec_mean)) +
    geom_histogram() +
    theme_classic())

# linear model with ISI band selection 

QHI_lowD$year <- as.factor(QHI_lowD$year)

m_H3e <- lm(data = QHI_lowD, spec_mean ~ type + year)

summary(m_H3e)

plot(m_H3e)
qqnorm(resid(m_H3e))
qqline(resid(m_H3e)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3e, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3e, show.values = TRUE))


# CV

(hist <- ggplot(QHI_lowD, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

# linear model with band selection

m_H3f <- lm(data = QHI_lowD, CV ~ type + year)

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
(p_H3a <- dwplot(list(m_H1a, m_H3a, m_H3e), 
                 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H3a, path = "figures", filename = "H3_models_mean.png", height = 10, width = 12)

# CV
# effect sizes and error dont seem to correspond with model summary...
(p_H3b <-dwplot(list(m_H1b, m_H3b, m_H3f), 
                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
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




(p_H1a <- ggpredict(m_H1a, terms = c("type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    scale_color_manual(values = c("#ffa544", "#2b299b", "grey")) +
    theme_cowplot())


(p_H3a <- ggpredict(m_H3a, terms = c("type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) +
    ylim(0,0.6) +
    theme_cowplot())

(p_H3e <- ggpredict(m_H3e, terms = c("type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

# CV predicitons
(p_H1b <- ggpredict(m_H1b, terms = c("type"), type = "fe") %>% 
    plot(rawdata = FALSE) +
    scale_color_manual(values = c("#ffa544", "#2b299b", "grey")) +
    ylim(-0.05,0.4) +
    theme_cowplot())


(p_H3b <- ggpredict(m_H3b, terms = c("type", "year"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) ++
    ylim(-0.05,0.4) +
    theme_cowplot())

(p_H3f <- ggpredict(m_H3f, terms = c("type"), type = "fe") %>% 
    plot(rawdata = TRUE) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) +
    ylim(-0.05,0.4) +
    theme_cowplot())

grid.arrange(p_H1a, p_H3a, p_H3e, 
             p_H1b, p_H3b, p_H3f, nrow=2) 


#############.
#############.
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


#  model with ISI band selection only 2019


m_H3g <- lm(data = lowD_2019selection, spec_mean ~ type + year)

summary(m_H3g)

plot(m_H3g)
qqnorm(resid(m_H3g))
qqline(resid(m_H3g)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3g, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3g, show.values = TRUE))


# CV

(hist <- ggplot(supervised_band_selection_2019, aes(x = CV)) +
    geom_histogram() +
    theme_classic())


# models with supervised band selection for dimention reduction 

m_H3h <- lm(data = lowD_2019selection, CV ~ type +year)

summary(m_H3h)

plot(m_H3h)
qqnorm(resid(m_H3h))
qqline(resid(m_H3h)) 


# Visualises random effects 
(re.effects <- plot_model(m_H3h, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H3h, show.values = TRUE))

# combined model vis

# spectral mean
(p_H3a <- dwplot(list(m_H1a, m_H3a, m_H3e, m_H3g), 
                 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H3a, path = "figures", filename = "H3_models_mean.png", height = 10, width = 12)

# CV
# effect sizes and error dont seem to correspond with model summary...
(p_H3b <-dwplot(list(m_H1b, m_H3b, m_H3f, m_H3h), 
                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H3b, path = "figures", filename = "H3_models_cv.png", height = 10, width = 12)

grid.arrange(p_H3a, p_H3b)  




##############.
##############.



# H2 model ----

# correlation plot full

correlation <- cor(collison_spec_plot_small[,c(5, 8:15)])

print(collison_spec_plot_small)


(p_corr <- corrplot(correlation, method="circle", type="upper", #order="hclust",
                    tl.srt=45, tl.col="black", diag = FALSE, order="hclust", col=brewer.pal(n=10, name="RdYlBu")))

# correlation plot for model

correlation_small <- cor(collison_spec_plot_small[,c(5, 8:9, 14:15)])

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


# linear model for H2 2018 2019

# spectral mean

#collison_spec_plot_small_2019 <- collison_spec_plot_small %>% filter(year == 2019) #%>%
# to normalize evenness
#mutate(evenness = (evenness- min(evenness))/(max(evenness)-min(evenness)))

# to scale all continous varibales
collison_spec_plot_small$richness <- scale(collison_spec_plot_small$richness)
collison_spec_plot_small$evenness <- scale(collison_spec_plot_small$evenness)
collison_spec_plot_small$bareground <- scale(collison_spec_plot_small$bareground)

str(collison_spec_plot_small)

m_H2a <- lm(data = collison_spec_plot_small,
              spec_mean ~ (type-1) * (type*richness) + (type*evenness) + (type*bareground))


summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 


p_H2a_base <- allEffects(m_H2a)
print(p_H2a_base)

plot(p_H2a_base)





# Visualises random effects 
(re.effects <- plot_model(m_H2a, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2a, show.values = TRUE))


# attempting to visulize model output
ggpredict(data = m_H2a, c("type", "richness")) %>% plot()


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



# CV 2018 2019

(hist <- ggplot(collison_spec_plot_small, aes(x = CV)) +
    geom_histogram() +
    theme_classic())

m_H2b <- lm(data = collison_spec_plot_small, 
              CV ~ (type-1) + (type*richness) + (type*evenness) + (type*bareground))

summary(m_H2b)

plot(m_H2b)
qqnorm(resid(m_H2b))
qqline(resid(m_H2b)) 

# spectral mean (richness, eveness, and bareground)
p_H2b_base <- allEffects(m_H2b)

print(p_H2b_base)
plot(p_H2b_base)

# specmean and cv outputs together

arrange(p_H2a_base, p_H2b_base)



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

m_H2a <- lmer(data = collison_spec_plot_small,
              spec_mean ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground) + (1|plot))


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

m_H2b <- lmer(data = collison_spec_plot_small, 
              CV ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground) + (1|plot))

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
