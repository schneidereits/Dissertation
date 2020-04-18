# QHI 2018_2019 PCAs
# shawn schneidereit edit
# 5.3.2020


#  QHI PCA ----

# for all QHI measurements(2019)
pca <- spec_2019_small 

# detailed pca; adapted from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#pca-data-format
res.pca <- PCA(pca[,c(4,5)], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca results
print(res.pca)

# eigen values

eig.val <- get_eigenvalue(res.pca)
eig.val


# grouped by elipsise
(p_pca_QHI <- fviz_pca_ind(res.pca,
                              geom.ind = "point", # show points only (nbut not "text")
                              col.ind = "black", # color by groups
                              pointshape = 21,
                              fill.ind = spec_2019_small$type, # color by groups
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
pca_lowD <- spec_2019 %>%
  filter(wavelength %in% ISI_band_selection$wavelength) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance)) %>%
  # MIGHT WHAT TO IMPROVE BUT I KNOW THIS WORKS...
  group_by(type, plot_unique, year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))


# both with new band selection?
pca_lowD <- spec_2018_2019 %>%
  mutate(wavelength = round(wavelength, digits = 0)) %>%
  filter(wavelength %in% QHI_ISI_band_selection$wavelength) %>%
  group_by(year, type, plot, plot_unique, type_year, wavelength) %>%
  summarise(spec_mean = mean(reflectance),
            spec_SD = sd(reflectance),
            CV = sd(reflectance)/mean(reflectance)) %>%
  # MIGHT WHAT TO IMPROVE BUT I KNOW THIS WORKS...
  group_by(type, plot_unique, year, type_year) %>%
  summarise(CV = mean(CV),
            spec_mean = mean(spec_mean))

res.pca_lowD <- PCA(pca_lowD[,5:6], scale.unit = TRUE, ncp = 5, graph = TRUE)

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
(p_pca <- fviz_pca_ind(res.pca_lowD,
                          geom.ind = "point", # show points only (nbut not "text")
                          pointshape = 21,
                         fill.ind = spec_2018_2019_small$type_year, # color by groups
                          col.ind = "black", # color by groups
                         # palette = c("#ffa544", "#2b299b", "gray65"),
                          palette = c( "tomato", "#ffa544", "purple", "#2b299b", "gray65"),
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
(p_pca <- fviz_pca_ind(res.pca_lowD,
                          repel = TRUE, 
                          geom.ind = "point", # show points only (nbut not "text")
                          pointshape = 21,
                          fill.ind = spec_2018_2019_small$type, # color by groups
                          col.ind = spec_2018_2019_small$type, # color by groups
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
res.pca_QHI_2018_2019 <- PCA(spec_2018_2019_small[,c(5,6)], scale.unit = TRUE, ncp = 5, graph = TRUE)

# pca 
(p_pca <- fviz_pca_ind(res.pca_QHI_2018_2019,
                       geom.ind = "point", # show points only (nbut not "text")
                       pointshape = 21,
                       col.ind = "black",
                       fill.ind = QHI_2018_2019_small$type, # color by groups
                       palette = c("#ffa544", "#2b299b", "gray65"),
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
                                fill.ind = spec_2018_2019_small$type_year, # color by groups
                                color.ind = spec_2018_2019_small$type_year,
                                pointshape = 21,
                                palette = c( "tomato", "#ffa544", "purple", "#2b299b", "gray65"),
                                addEllipses = TRUE, # Concentration ellipses
                                # ellipse.type = "confidence",
                                ellipse.level = 0.90, # confidence level specification
                                mean.point = TRUE, # braycenter mean point
                                legend.title = "Groups",
                                axes.linetype = "dashed",
                                xlab = "PC1", ylab = "PC2", 
                                ggtheme = theme_cowplot()))

ggsave(p_pca_veg_year, path = "figures", filename = "QHI_2018-2019_pca.png", height = 10, width = 12)

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
res.pca_H2_2018_2019 <- PCA(pca_H2_2018_2019[,c(5, 8:15)], scale.unit = TRUE, ncp = 9, graph = TRUE)

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
t <- collison_spec_plot_small %>%
  # add site variable 
  unite(site, c(type,year))

var <- get_pca_var(t)
head(var$coord)

(p_pca <- fviz_pca_biplot(res.pca_H2_2018_2019,
                          geom.ind = "point", # show points only (nbut not "text")
                          fill.ind = t$site, # color by groups
                          color.ind = "black",
                          pointshape = 21, 
                          #  palette = c("#FF4500", "#FF8C00", "#FF7256", "#CD1076", "#FF4500", "#00CED1", "#8470FF", "#D15FEE", "#63B8FF"),
                          addEllipses = TRUE, # Concentration ellipses
                          # ellipse.type = "confidence",
                          repel = TRUE,
                          ellipse.level = 0.95, # confidence level specification
                          mean.point = TRUE, # braycenter mean point
                          # to color arrow by variable type
                          col.var = factor(c("diversity", "diversity",
                                             "environmenal", "environmenal", "environmenal", 
                                             "environmenal", "environmenal", 
                                             "spectral", "spectral")),
                         pallette = c("#00AFBB", "#00AFBB", "#FC4E07", "#FC4E07",
                                            "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800",  "#E7B800"),
                          # col.var = "cos2",
                          # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          # alternate color gradient gradient.cols = c("blue", "yellow", "red")
                          legend.title = list(fill = "Sites", color = "cos2"),
                          axes.linetype = "dashed",
                          xlab = "PC1", ylab = "PC2",
                         ggtheme = theme_cowplot())) +
  ggpubr::color_palette("Dark2")  +    # Variable colors
  ggpubr::fill_palette(c("#FF4500", "#FF8C00", "#D15FEE", "#63B8FF"))     # Indiviual fill color

(p_pca <- fviz_pca_ind(res.pca_QHI_2018_2019,
                       geom.ind = "point", # show points only (nbut not "text")
                       pointshape = 21,
                       col.ind = "black",
                       fill.ind = QHI_2018_2019_small$type, # color by groups
                       palette = c("#ffa544", "#2b299b", "gray65"),
                       addEllipses = TRUE, # Concentration ellipses
                       # ellipse.type = "confidence",
                       ellipse.level = 0.95, # confidence level specification
                       mean.point = TRUE, # braycenter mean point
                       legend.title = "Groups",
                       axes.linetype = "dashed",
                       xlab = "PC1", ylab = "PC2", 
                       ggtheme = theme_cowplot()))
