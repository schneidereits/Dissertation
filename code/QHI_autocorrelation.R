# QHI 2018_2019 spatial autocorrelation
# shawn schneidereit edit
# 5.3.2020


# H4 spatial autocorrelation ----

# spatial variogram

QHI_spatial <- collison_spec_plot_small_2019 %>% 
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