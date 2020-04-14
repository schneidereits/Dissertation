# QHI 2018_2019 models
# shawn schneidereit edit
# 5.3.2020

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