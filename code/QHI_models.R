# QHI 2018_2019 models
# shawn schneidereit edit
# 5.3.2020

#  H1 model for spectral mean ----

# spectral mean model

# model 2018 + 2019 HE & KO 

collison_2018_2019_small$year <- as.factor(collison_2018_2019_small$year)

# linear model for H1

m_H1a <- (lm(data = collison_2018_2019_small, spec_mean ~ (type-1) + year)) # (type-1) changes intercpt to HE 



summary(m_H1a)
summary(m_H1b)
stargazer(m_H1a, type = "text")

starg

plot(m_H1a)
qqnorm(resid(m_H1a))
qqline(resid(m_H1a)) 

# visulise fixed effect
(fe.effects <- plot_model(m_H1a, show.values = TRUE))

# gpreditct by type
(p_H1a <- ggpredict(m_H1a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot())



# h1 modle for CV ------
#aka spectral diversity

m_H1b <- (lm(data = collison_2018_2019_small, CV ~ (type-1) + year)) # (type-1) changes intercpt to HE 

summary(m_H1b)

plot(m_H1b)

stargazer(m_H1b, type = "text")


plot(m_H1b)
qqnorm(resid(m_H1b))
qqline(resid(m_H1b)) 

# visulise fixed effect
(fe.effects <- plot_model(m_H1b, show.values = TRUE))

# gpreditct by type
ggpredict(m_H1b, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()


# H2 models (band selection) ----

# linear model with supervised band selection 2018+2019
supervised_band_selection$year <-  as.factor(supervised_band_selection$year)

supervised_band_selection <- supervised_band_selection %>% filter(type=="mixed")

# linear model with manual band selection for spectral diversity
m_H2a <- lm(data = supervised_band_selection, spec_mean ~ type + year)

summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 

# visulise fixed effect
(fe.effects <- plot_model(m_H2a, show.values = TRUE))


# linear model with manual band selection for spectral diversity

m_H2b <- lm(data = supervised_band_selection, CV ~ type + year)

summary(m_H2b)

plot(m_H2b)
qqnorm(resid(m_H2b))
qqline(resid(m_H2b)) 


# visulise fixed effect
(fe.effects <- plot_model(m_H2b, show.values = TRUE))




############ ISI band selecrtion models ----

# linear model with ISI band selection 

QHI_lowD$year <- as.factor(QHI_lowD$year)

m_H2e <- lm(data = QHI_lowD, spec_mean ~ type + year)

summary(m_H2e)

plot(m_H2e)
qqnorm(resid(m_H2e))
qqline(resid(m_H2e)) 


# Visualises random effects 
(re.effects <- plot_model(m_H2e, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2e, show.values = TRUE))


# CV

# linear model with band selection

m_H2f <- lm(data = QHI_lowD, CV ~ type + year)

summary(m_H2f)

plot(m_H2f)
qqnorm(resid(m_H2f))
qqline(resid(m_H2f)) 


# Visualises random effects 
(re.effects <- plot_model(m_H2f, type = "re", show.values = TRUE))
# visulise fixed effect
(fe.effects <- plot_model(m_H2f, show.values = TRUE))


# combined model vis

# spectral mean
(p_H2a <- dwplot(list(m_H1a, m_H2a, m_H2e), 
                 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H2a, path = "figures", filename = "H2_models_mean.png", height = 10, width = 12)

# CV
# effect sizes and error dont seem to correspond with model summary...
(p_H2b <-dwplot(list(m_H1b, m_H2b, m_H2f), 
                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H2b, path = "figures", filename = "H2_models_cv.png", height = 10, width = 12)

grid.arrange(p_H2a, p_H2b)  


# If needed I could use ggpredict to creat boxplot of predicted spec_mean and cv by VT by Model (but might not be compatable with lme4)
# https://strengejacke.github.io/ggeffects/reference/ggpredict.html

ggpredict(m_H2a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = F) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

ggpredict(m_H2b, terms = c("type" ), type = "fe") %>% 
  plot(rawdata = F) +
  #scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

dat <- ggpredict(H2a, terms = c("c172code", "c161sex"))
ggplot(H2a, aes(type, predicted, colour = group)) +
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  ) +
  scale_x_discrete(breaks = 1:3, labels = get_x_labels(dat))




(p_H1a <- ggpredict(m_H1a, terms = c("type[HE,KO]"), type = "fe", title = "") %>% 
    plot(rawdata = F, colors = c("#ffa544", "#2b299b")) +
    ylim(0.1,0.4) +
    labs(y= "Mean reflectance") +
    theme_cowplot())


(p_H2a <- ggpredict(m_H2a, terms = c("type[HE,KO]"), type = "fe", show.title = F) %>% 
    plot(rawdata = F) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) +
    ylim(0.1,0.4) +
    labs(y= "Mean reflectance") +
    theme_cowplot())

(p_H2e <- ggpredict(m_H2e, terms = c("type[HE,KO]"), type = "fe", show.title = F) %>% 
    plot(rawdata = F) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) +
    ylim(0.1,0.4) +
    labs(y= "Mean reflectance") +
    theme_cowplot())

# CV predicitons
(p_H1b <- ggpredict(m_H1b, terms = c("type[HE,KO]"), type = "fe",  show.title = F ) %>% 
    plot(rawdata = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b", "grey")) +
    ylim(0,0.2) +
    labs(y= "Spectral diversity") +
    theme_cowplot())


(p_H2b <- ggpredict(m_H2b, terms = c("type[HE,KO]"), type = "fe", show.title = F) %>% 
    plot(rawdata = F) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) ++
    ylim(0,0.2) +
    labs(y= "") +
    theme_cowplot())

(p_H2f <- ggpredict(m_H2f, terms = c("type[HE,KO]"), type = "fe", show.title = F) %>% 
    plot(rawdata = F) +
    #scale_color_manual(values = c("#ffa544", "#2b299b")) +
    ylim(0,0.2) +
    labs(y= "") +
    theme_cowplot())

grid.arrange(p_H1a, p_H2a, p_H2e, 
             p_H1b, p_H2b, p_H2f, nrow=2) 


#############.
#############.
# linear model with supervised band selection only 2019

supervised_band_selection_2019 <- supervised_band_selection %>%
  filter(year == 2019)

m_H2c <- lmer(data = supervised_band_selection_2019, spec_mean ~ type + (1|plot))

summary(m_H2c)

plot(m_H2c)
qqnorm(resid(m_H2c))
qqline(resid(m_H2c)) 

# visulise fixed effect
(fe.effects <- plot_model(m_H2c, show.values = TRUE))


# CV

# models with supervised band selection for dimention reduction 

m_H2d <- lmer(data = supervised_band_selection_2019, CV ~ type + (1|plot))

summary(m_H2d)

plot(m_H2d)
qqnorm(resid(m_H2d))
qqline(resid(m_H2d)) 

# visulise fixed effect
(fe.effects <- plot_model(m_H2d, show.values = TRUE))


#  model with ISI band selection only 2019


m_H2g <- lm(data = lowD_2019selection, spec_mean ~ type + year)

summary(m_H2g)

plot(m_H2g)
qqnorm(resid(m_H2g))
qqline(resid(m_H2g)) 


# visulise fixed effect
(fe.effects <- plot_model(m_H2g, show.values = TRUE))




# models with supervised band selection for dimention reduction 

m_H2h <- lm(data = lowD_2019selection, CV ~ type +year)

summary(m_H2h)

plot(m_H2h)
qqnorm(resid(m_H2h))
qqline(resid(m_H2h)) 


# visulise fixed effect
(fe.effects <- plot_model(m_H2h, show.values = TRUE))

# combined model vis

# spectral mean
(p_H2a <- dwplot(list(m_H1a, m_H2a, m_H2e, m_H2g), 
                 vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H2a, path = "figures", filename = "H2_models_mean.png", height = 10, width = 12)

# CV
# effect sizes and error dont seem to correspond with model summary...
(p_H2b <-dwplot(list(m_H1b, m_H2b, m_H2f, m_H2h), 
                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) +
    theme_cowplot() +
    coord_flip() +
    theme(legend.position = c(0.8, 0.2),
          legend.justification = c(0, 0),
          legend.title.align = .5))

ggsave(p_H2b, path = "figures", filename = "H2_models_cv.png", height = 10, width = 12)

grid.arrange(p_H2a, p_H2b)  




##############.
##############.



# H3 model ----

# correlation plot full
library(corrplot)

correlation <- cor(collison_spec_plot_small[,c(5, 9:16)])

print(collison_spec_plot_small) 


(p_corr <- corrplot(correlation, method="circle", type="upper", #order="hclust",
                    tl.srt=45, tl.col="black", diag = FALSE, order="hclust", col=brewer.pal(n=10, name="RdYlBu")))

# correlation plot for model

correlation_small <- cor(collison_spec_plot_small[,c(5, 8:9, 15:16)])

(p_corr <- corrplot(correlation_small, method="circle", type="upper", #order="hclust",
                    tl.srt=45, tl.col="black", diag = FALSE, col=brewer.pal(n=10, name="RdYlBu")))


# ugly alternative 
#install.packages("PerformanceAnalytics")
#library("PerformanceAnalytics")
#chart.Correlation(correlation, histogram=TRUE, pch=19)


# spectral mean 

# linear model for H2 2018 2019


collison_spec_plot_small_model <- collison_spec_plot_small_visable #%>% filter(year==2018)


# to scale all continous varibales
collison_spec_plot_small_model$richness <- c(scale(collison_spec_plot_small$richness, center = TRUE, scale = TRUE))
collison_spec_plot_small_model$evenness <- c(scale(collison_spec_plot_small$evenness, center = TRUE, scale = TRUE))
collison_spec_plot_small_model$bareground <- c(scale(collison_spec_plot_small$bareground, center = TRUE, scale = TRUE))
collison_spec_plot_small_model$year <- as.factor(collison_spec_plot_small$year)

# CV 2018 2019

m_H2b <- lm(data = collison_spec_plot_small_model, 
              CV ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground))

summary(m_H2b)
stargazer(m_H2b, type="text")


plot(m_H2b)
qqnorm(resid(m_H2b))
qqline(resid(m_H2b)) 

# spectral mean (richness, eveness, and bareground)
p_H2b_base <- allEffects(m_H2b)

print(p_H2b_base)
plot(p_H2b_base)

# specmean and cv outputs together

arrange(p_H2a_base, p_H2b_base)


# visulise fixed effect
(fe.effects <- plot_model(m_H2b, show.values = TRUE))


ggpredict(m_H2b, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  scale_fill_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()


 # doesnt work trying to rescale to original values
 #m_H2b$coefficients[richness] <- scales::rescale(m_H2b$coefficients[richness], to=c(10,14))

(p_H2b_rich <- ggpredict(m_H2b, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    xlab("Richness")+
   # coord_cartesian(ylim = c(0.01,0.4), xlim= c(-2,2)) +
    theme_cowplot())


(p_H2b_even <- ggpredict(m_H2b, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
   # coord_cartesian(ylim = c(0.01,0.4), xlim = c(-2,2)) +
    theme_cowplot()+ 
    xlab("Evenness")+
    theme(legend.position = "none"))

(p_H2b_ground <- ggpredict(m_H2b, terms = c("bareground", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    scale_fill_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot()+
    xlab("Bare ground")+
   # coord_cartesian(ylim = c(0.01,0.4), xlim = c(-2,2)) +
    theme(legend.position = "none"))



grid.arrange(p_H2b_rich, p_H2b_even, p_H2b_ground, nrow=2)



# alternative with interaction terms (base r)

# spectral mean (richness, eveness, and bareground)
p_H2a_base <- allEffects(m_H2a)

print(p_H2a_base)
plot(p_H2a_base)

# spectral diverstiy (richness, eveness, and bareground)
p_H2b_base <- allEffects(m_H2b)

print(p_H2b_base)
plot(p_H2b_base)



# linear model for H2 2019

# spectral mean

collison_spec_plot_small_2019 <- collison_spec_plot_small %>% filter(year == 2019) #%>%
# attempt to normalize evenness
#mutate(evenness = (evenness- min(evenness))/(max(evenness)-min(evenness)))

# to scale all continous varibales
collison_spec_plot_small_2019$richness <- scale(collison_spec_plot_small_2019$richness)
collison_spec_plot_small_2019$evenness <- scale(collison_spec_plot_small_2019$evenness)
collison_spec_plot_small_2019$bareground <- scale(collison_spec_plot_small_2019$bareground)

str(collison_spec_plot_small_2019)

m_H2a <- lm(data = collison_spec_plot_small,
              spec_mean ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground))


summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 

# visulise fixed effect
(fe.effects <- plot_model(m_H2a, show.values = TRUE))


# attempting to visulize model output
ggpredict(data = m_H2a, c("type", "richness")) %>% plot()


# visuilization attempt
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


m_H2b <- lm(data = collison_spec_plot_small, 
              CV ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground))

summary(m_H2b)

plot(m_H2b)
qqnorm(resid(m_H2b))
qqline(resid(m_H2b)) 

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







# to scale all continous varibales
collison_spec_plot_small_corr$richness <- c(scale(collison_spec_plot_small_corr$richness))
collison_spec_plot_small_corr$evenness <- c(scale(collison_spec_plot_small_corr$evenness))
collison_spec_plot_small_corr$bareground <- c(scale(collison_spec_plot_small_corr$bareground))
collison_spec_plot_small_corr$year <- as.factor(collison_spec_plot_small_corr$year)

str(collison_spec_plot_small)

m_H2a <- lm(data = collison_spec_plot_small_corr,
            spec_mean ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground))


summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 


p_H2a_base <- allEffects(m_H2a)
print(p_H2a_base)

plot(p_H2a_base)

# predictions

ggpredict(m_H2a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

(p_H2a_rich <- ggpredict(m_H2a, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    coord_cartesian(ylim = c(0.015,0.6), xlim= c(-2,2)) +
    theme_cowplot()+
    theme(legend.position = "none"))

(p_H2a_even <- ggpredict(m_H2a, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    coord_cartesian(ylim = c(0.015,0.6), xlim= c(-2,2)) +
    theme_cowplot()+
    theme(legend.position = "none"))

(p_H2a_ground <- ggpredict(m_H2a, terms = c("bareground", "type"), type = "fe" ) %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    coord_cartesian(ylim = c(0.015,0.6), xlim= c(-2,2)) +
    theme_cowplot()+
    theme(legend.position = "none"))




# marginal effects for all 4 interaction terms
pr <- ggpredict(m_H2a, c( "richness", "type"))

# use plot() method, easier than own ggplot-code from scratch
plot(pr)

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


  ggpredict(m_H2a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE, show.title = F) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

(p_H2a_rich <- ggpredict(m_H2a, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

(p_H2a_even <- ggpredict(m_H2a, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot()+
    theme(legend.position = "none"))

(p_H2a_ground <- ggpredict(m_H2a, terms = c("bareground", "type"), type = "fe" ) %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot()+
    theme(legend.position = "none"))


grid.arrange(p_H2a_rich, p_H2a_even, p_H2a_ground, nrow = 1)

# do not need
# h3 spectral mean
# spectral mean

#collison_spec_plot_small_2019 <- collison_spec_plot_small %>% filter(year == 2019) #%>%
# to normalize evenness
#mutate(evenness = (evenness- min(evenness))/(max(evenness)-min(evenness)))

# to scale all continous varibales
collison_spec_plot_small$richness <- c(scale(collison_spec_plot_small$richness))
collison_spec_plot_small$evenness <- c(scale(collison_spec_plot_small$evenness))
collison_spec_plot_small$bareground <- c(scale(collison_spec_plot_small$bareground))
collison_spec_plot_small$year <- as.factor(collison_spec_plot_small$year)

str(collison_spec_plot_small)

m_H2a <- lm(data = collison_spec_plot_small,
            spec_mean ~ (type-1) + year + (type*richness) + (type*evenness) + (type*bareground))


summary(m_H2a)

plot(m_H2a)
qqnorm(resid(m_H2a))
qqline(resid(m_H2a)) 


p_H2a_base <- allEffects(m_H2a)
print(p_H2a_base)

plot(p_H2a_base)

# predictions

ggpredict(m_H2a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

(p_H2a_rich <- ggpredict(m_H2a, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    coord_cartesian(ylim = c(0.015,0.6), xlim= c(-2,2)) +
    theme_cowplot()+
    theme(legend.position = "none"))

(p_H2a_even <- ggpredict(m_H2a, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    coord_cartesian(ylim = c(0.015,0.6), xlim= c(-2,2)) +
    theme_cowplot()+
    theme(legend.position = "none"))

(p_H2a_ground <- ggpredict(m_H2a, terms = c("bareground", "type"), type = "fe" ) %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    coord_cartesian(ylim = c(0.015,0.6), xlim= c(-2,2)) +
    theme_cowplot()+
    theme(legend.position = "none"))




# marginal effects for all 4 interaction terms
pr <- ggpredict(m_H2a, c( "richness", "type"))

# use plot() method, easier than own ggplot-code from scratch
plot(pr)


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

  ggpredict(m_H2a, terms = c("type"), type = "fe") %>% 
  plot(rawdata = TRUE, show.title = F) +
  scale_color_manual(values = c("#ffa544", "#2b299b")) +
  theme_cowplot()

(p_H2a_rich <- ggpredict(m_H2a, terms = c("richness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

(p_H2a_even <- ggpredict(m_H2a, terms = c("evenness", "type"), type = "fe") %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())

(p_H2a_ground <- ggpredict(m_H2a, terms = c("bareground", "type"), type = "fe" ) %>% 
    plot(rawdata = TRUE, show.title = F) +
    scale_color_manual(values = c("#ffa544", "#2b299b")) +
    theme_cowplot())


grid.arrange(p_H2a_rich, p_H2a_even, p_H2a_ground, nrow = 1)


