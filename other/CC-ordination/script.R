# Ordination tutorial
# 8.2.2020

library----
  
  # Install and load the following packages
#install.packages("vegan")
#install.packages("ape")
#install.packages("dplyr")

library(ape)
library(dplyr)
library(vegan)

# PCA

# Load the community dataset which we`ll use in the examples today
data(varespec)

# Open the dataset and look if you can find any patterns
View(varespec)
# It is probably very difficult to see any patterns by just looking at the data frame!

# With this command, you`ll perform a NMDS and plot the results
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

PCA <- rda(varespec, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")
# plotting both
plot(PCA, display = c("sites", "species"), type = "points")

# You can extract the species and site scores on the new PC for further analyses:
sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

# Check out the help file how to pimp your biplot further:
?biplot.rda

# You can even go beyond that, and use the ggbiplot package.
# You can install this package by running:
#library(devtools)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)

# First step is to calculate a distance matrix. 
# Here we use Bray-Curtis distance metric
dist <- vegdist(varespec,  method = "bray")

# principle cordinate analysis

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:3]) # 86%

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, varespec)

# Extract the plot scores from first two PCoA axes (if you need them):
PCOAaxes <- PCOA$vectors[,c(1,2)]

# Compare this result with the PCA plot
par(mfrow = c(1, 2)) 
biplot.pcoa(PCOA)
plot(PCA)

# reset plot window
par(mfrow = c(1, 1)) 

# non-metric multidimentional scaling

# First step is to calculate a distance matrix. See PCOA for more information about the distance measures
# Here we use bray-curtis distance, which is recommended for abundance data
dist <- vegdist(varespec,  method = "bray")

# Methodology of NMDS:
  
#Step 1: Perform NMDS with 1 to 10 dimensions
#Step 2: Check the stress vs dimension plot
#Step 3: Choose optimal number of dimensions
#Step 4: Perform final NMDS with that number of dimensions
#Step 5: Check for convergent solution and final stress

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)

# apparently to find we don´t see a data point for 1 dimension
metaMDS(dist)
metaMDS(dist, k = 1, trymax = 100, trace = F)

# Because the final result depends on the initial 
# random placement of the points 
# we`ll set a seed to make the results reproducible
set.seed(2)

?metaMDS()
# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE)
NMDS2

stressplot(NMDS1)


plot(NMDS1, type = "t")

# There are no species scores (same problem as we encountered with PCoA). We can work around this problem, by giving metaMDS the original community matrix as input and specifying the distance measure.
NMDS3 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

# Load the second dataset
data(varechem)

# The function envfit will add the environmental variables as vectors to the ordination plot
ef <- envfit(NMDS3, varechem, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value
# Plot the vectors of the significant correlations and interpret the plot
plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)

# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Group1", 12), rep("Group2", 12))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("red", 12), rep("blue", 12))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)

# challenge 1 (Dune data)

data(dune)

dune %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

PCA_d <- rda(dune, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA_d$CA$eig)/sum(PCA_d$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA_d$CA$eig)/sum(PCA_d$CA$eig))[1:2]) # 51%, this is Low?.
# Also try to do it for the first three axes
sum((as.vector(PCA_d$CA$eig)/sum(PCA_d$CA$eig))[1:3]) # 60%, still low?
sum((as.vector(PCA_d$CA$eig)/sum(PCA_d$CA$eig))[1:4]) # 68

# Now, we`ll plot our results with the plot function
plot(PCA_d)
plot(PCA_d, display = "sites", type = "points")
plot(PCA_d, display = "species", type = "text")
# plotting both
plot(PCA_d, display = c("sites", "species"), type = "points")

# You can extract the species and site scores on the new PC for further analyses:
sitePCA_d <- PCA_d$CA$u # Site scores
speciesPCA_d <- PCA_d$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA_d, choices = c(1,2), type = c("text", "points"), xlim = c(-2,3)) # biplot of axis 1 vs 2
biplot(PCA_d, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

# Check out the help file how to pimp your biplot further:
?biplot.rda

# You can even go beyond that, and use the ggbiplot package.
# You can install this package by running:
#library(devtools)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)

# First step is to calculate a distance matrix. 
# Here we use Bray-Curtis distance metric
dist_d <- vegdist(dune,  method = "bray")

# principle cordinate analysis

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA_d <- pcoa(dist_d)

# plot the eigenvalues and interpret
barplot(PCOA_d$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?
sum((as.vector(PCA_d$CA$eig)/sum(PCA_d$CA$eig))[1:2]) # 51%
sum((as.vector(PCA_d$CA$eig)/sum(PCA_d$CA$eig))[1:3]) # 60%

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA_d <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA_d)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA_d, dune)

# Extract the plot scores from first two PCoA axes (if you need them):
PCOAaxes_d <- PCOA$vectors[,c(1,2)]

# Compare this result with the PCA plot
par(mfrow = c(1, 2)) 
biplot.pcoa(PCOA_d)
plot(PCA_d)

# reset plot window
par(mfrow = c(1, 1)) 

# non-metric multidimentional scaling

# First step is to calculate a distance matrix. See PCOA for more information about the distance measures
# Here we use bray-curtis distance, which is recommended for abundance data
dist_d <- vegdist(dune,  method = "bray")

# Methodology of NMDS:

#Step 1: Perform NMDS with 1 to 10 dimensions
#Step 2: Check the stress vs dimension plot
#Step 3: Choose optimal number of dimensions
#Step 4: Perform final NMDS with that number of dimensions
#Step 5: Check for convergent solution and final stress

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist_d)

# apparently to find we don´t see a data point for 1 dimension
metaMDS(dist_d)
metaMDS(dist_d, k = 1, trymax = 100, trace = F)

# Because the final result depends on the initial 
# random placement of the points 
# we`ll set a seed to make the results reproducible
set.seed(2)

?metaMDS()
# Here, we perform the final analysis and check the result
NMDS1_d <- metaMDS(dist_d, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1_d

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2_d <- metaMDS(dune, k = 2, trymax = 100, trace = F, autotransform = FALSE)
NMDS2_d

stressplot(NMDS1_d)


plot(NMDS1_d, type = "t")

# There are no species scores (same problem as we encountered with PCoA). We can work around this problem, by giving metaMDS the original community matrix as input and specifying the distance measure.
NMDS3_d <- metaMDS(dune, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3_d)
plot(NMDS3_d, display = "sites", type = "n")
points(NMDS3_d, display = "sites", col = "red", cex = 1.25)
text(NMDS3_d, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS3_d, type = "n")
orditorp(NMDS3_d, display = "species", col = "red", air = 0.01)
orditorp(NMDS3_d, display = "sites", cex = 1.1, air = 0.01)

# Load the second dataset
data(varechem)

# The function envfit will add the environmental variables as vectors to the ordination plot
ef_d <- envfit(NMDS3_d, varechem, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value
# Plot the vectors of the significant correlations and interpret the plot
plot(NMDS3_d, type = "t", display = "sites")
plot(ef_d, p.max = 0.05)

# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Group1", 12), rep("Group2", 12))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("red", 12), rep("blue", 12))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3_d, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3_d$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3_d, display = "species", col = "red", air = 0.01)
orditorp(NMDS3_d, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)
