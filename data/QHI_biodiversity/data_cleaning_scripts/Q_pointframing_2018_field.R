# Pointframing results with 2018 included

# Qikiqtaruk Ecological Monitoring manuscript script
# Code for all modelling and data visualisation within the manuscript

# Written by Isla Myers-Smith, Anne Bjorkman, Haydn Thomas, Sandra Angers-Blondin and Gergana Daskalova
# E-mail: isla.myers-smith@ed.ac.uk
# 2018-01-30 

# Packages ----
library(MCMCglmm)
library(ggplot2)
library(plyr) # Load plyr before dplyr
library(gridExtra)
library(dplyr)
library(tidyr)
library(rjags)
library(R2jags)
library(stargazer)

# Defining functions used within the script ----

# Function to extract MCMCglmm model summary outputs ----
clean.MCMC <- function(x) {
  sols <- summary(x)$solutions  # pull out relevant info from model summary
  Gcovs <- summary(x)$Gcovariances
  Rcovs <- summary(x)$Rcovariances
  
  fixed <- data.frame(row.names(sols), sols, row.names = NULL)  # convert to dataframes with the row.names as the first col
  random <- data.frame(row.names(Gcovs), Gcovs, row.names = NULL)
  residual <- data.frame(row.names(Rcovs), Rcovs, row.names = NULL)
  
  names(fixed)[names(fixed) == "row.names.sols."] <- "variable"  # change the columns names to variable, so they all match
  names(random)[names(random) == "row.names.Gcovs."] <- "variable"
  names(residual)[names(residual) == "row.names.Rcovs."] <- "variable"
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  residual$effect <- "residual"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, residual))  # merge it all together
}

# Function for when models have no random effects
clean.MCMC.2 <- function(x) {
  sols <- summary(x)$solutions  # pull out relevant info from model summary
  Gcovs <- summary(x)$Gcovariances
  Rcovs <- summary(x)$Rcovariances
  
  fixed <- data.frame(row.names(sols), sols, row.names = NULL)  # convert to dataframes with the row.names as the first col
  residual <- data.frame(row.names(Rcovs), Rcovs, row.names = NULL)
  
  names(fixed)[names(fixed) == "row.names.sols."] <- "variable"  # change the columns names to variable, so they all match
  names(residual)[names(residual) == "row.names.Rcovs."] <- "variable"
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  residual$effect <- "residual"
  
  modelTerms <- as.data.frame(bind_rows(fixed, residual))  # merge it all together
}

getName.MCMC <- function(x) deparse(substitute(x))  # adding the model name

# Customised ggplot2 theme function ----
theme_QHI <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 16), 
          axis.title = element_text(size = 20),
          axis.text.x = element_text(angle = -45, hjust = -0.05),
          axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0),
          legend.text = element_text(size=16, face="italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9), 
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", fill = "transparent", size = 4, linetype="blank"))
}

# Loading data ----

# Vegetation cover and canopy height ----

HEcover <- read.csv("scripts/users/gdaskalova/QHI_cover_1999_2018_ITEX_updated.csv")
levels(HEcover$name) # Checking all spelling is correct

# Zero fill data
HEcover$cover <- as.numeric(as.character(HEcover$cover))

HEcover <- HEcover %>% 
  complete(plot, sub_name, year, name, fill = list(cover = 0)) %>% 
  group_by(name, year, sub_name, plot) %>% 
  summarise(cover = mean(cover))

# Total biomass - Cover
species_IDs <- as.data.frame(unique(HEcover$name))
HEcoverveg <- subset(HEcover, name !="XXXlitter" & name !="XXXlitter " & 
                       name !="XXXbareground" & name !="XXXbareground " & 
                       name !="XXXrock" & name !="XXXrock " & name !="XXXfeces" & 
                       name !="XXXfeces " & name !="XXXstandingwater" & 
                       name !="XXXstandingwater " & name !="XXXspider")

biomass_cover <- ddply(HEcoverveg, .(year, plot, sub_name), summarise,
                       Biomass = sum(cover))

HEcoverHE <- subset(HEcover, sub_name == "QHI:HE")
HEcoverKO <- subset(HEcover, sub_name == "QHI:KO")

HEcoverHE$name <- as.character(HEcoverHE$name)
HEcoverHE$name[HEcoverHE$name == "XXXcarex:QHI "] <- "Carex sp."

# Total biomass - Hits
abundance <- read.csv("scripts/users/gdaskalova/Herschel_ITEXdata_1999-2018_updated.csv")
abundance$unique_coords <- paste(abundance$X, abundance$Y, sep = "")

# Check no. points per plot as sometimes 90
n_hits <- ddply(abundance, .(YEAR, PLOT, SUBSITE), summarise,
                n_hits = length(unique(unique_coords)))

# Remove non-veg
abundance_veg <- subset(abundance, SPP !="XXXlitter" & SPP !="XXXlitter " & 
                          SPP !="XXXbareground" & SPP !="XXXbareground " & 
                          SPP !="XXXrock" & SPP !="XXXrock " & SPP !="XXXfeces" & 
                          SPP !="XXXfeces " & SPP !="XXXstandingwater" & 
                          SPP !="XXXstandingwater " & SPP !="XXXspider")

# Calculate biomass per plot
biomass_hits <- ddply(abundance_veg, .(YEAR, PLOT, SUBSITE), summarise,
                      Biomass = sum(Abundance))

biomass_hits[99, 4] <- 358

# Combine with number of points per plot
biomass_hits <- merge(biomass_hits, n_hits)
biomass_hits$Biomass <- biomass_hits$Biomass/biomass_hits$n_hits

# ** Bareground cover data ----

abundance$uniqueID <- paste(abundance$YEAR, abundance$SUBSITE, 
                            abundance$PLOT, abundance$X, abundance$Y, sep="")  # Assign unique ID to every point
bareground_IDs <- abundance[abundance$SPP=="XXXlitter" | abundance$SPP=="XXXlitter " | 
                              abundance$SPP=="XXXbareground" | abundance$SPP=="XXXbareground " | 
                              abundance$SPP=="XXXrock" | abundance$SPP =="XXXrock " | 
                              abundance$SPP =="XXXfeces" | abundance$SPP =="XXXfeces " | 
                              abundance$SPP =="XXXstandingwater" | abundance$SPP =="XXXstandingwater " | 
                              abundance$SPP =="XXXspider", "uniqueID"]  # Identify points with non-vegetation indicators
abundance_BGs <- abundance[abundance$uniqueID %in% bareground_IDs,]  # Extract only points that have non-veg indicators

Out=NULL  # Set up loop
for(i in unique(abundance_BGs$uniqueID)){  # For each point
  a <- subset(abundance_BGs, uniqueID==i)  # create dataframe of all entries for that point
  b <- a[a$SPP == "XXXlitter" | a$SPP == "XXXlitter " | a$SPP == "XXXbareground" | 
           a$SPP == "XXXbareground " | a$SPP == "XXXrock" | a$SPP == "XXXrock " | 
           a$SPP == "XXXfeces" | a$SPP == "XXXfeces " | a$SPP == "XXXstandingwater" | 
           a$SPP == "XXXstandingwater " | a$SPP == "XXXspider",]  # Identify how many entries are not vegetation
  c <- nrow(a) - nrow(b)  # Finnd out if any entries are vegetation (i.e. total rows - non-veg rows)
  Out <- rbind(Out, c(i, c))  # Extract point name and number vegetation entries
}

BGs <- as.data.frame(Out)  # Convert into data frame
BGs <- subset(BGs,V2=="0")  # Extract points for which there are only non-veg data (i.e.i.e. total rows - non-veg rows = 0)
BGs <- BGs[,1]  # Extract only first column (unique points)

bareground <- abundance_BGs <- abundance[abundance$uniqueID %in% BGs,]  # Create dataframe of bare ground points 
bareground <- ddply(bareground,.(YEAR, PLOT, SUBSITE), summarise,
                    Bareground = sum(Abundance))  # Count number of bare ground points per plot

# Create dummy dataframe with all plots because if plots have no bare ground they wont be included
bareground_full <- ddply(abundance,.(YEAR, SUBSITE, PLOT), summarise,
                         Bareground = 0)  

# Replace dummy bareground with real baregound
bareground_full$Bareground <- bareground$Bareground[match(paste(bareground_full$YEAR,
                                                                bareground_full$SUBSITE,
                                                                bareground_full$PLOT),
                                                          paste(bareground$YEAR,
                                                                bareground$SUBSITE,
                                                                bareground$PLOT))] 
bareground <- bareground_full  # Rename to original
bareground[is.na(bareground$Bareground),]$Bareground <- 0  # Replace NAs from match with zeros

# ** Canopy height ----

abundance$Height..cm. <- as.numeric(as.character(abundance$Height..cm.))
heights <- subset(abundance,!is.na(Height..cm.))

canopy <- ddply(heights,.(YEAR, SUBSITE, PLOT, unique_coords), summarise,
                MaxHeight = max(Height..cm.)) #Take max height at each unique point

# Average by plot
avg_heights <- ddply(canopy,.(YEAR, SUBSITE, PLOT), summarise,
                     Mean.C.H = mean(MaxHeight))

# ** Salix pulchra Height ----

salpuls <- subset(abundance, SPP=="SALPUL"|SPP == "Salix pulchra") #Take only heights for Salix pulchra

salpuls <- subset(salpuls,!is.na(Height..cm.))

meansp <- ddply(salpuls,.(YEAR), summarise,
                mean.height = mean(Height..cm.),
                sd = sd(Height..cm.))

avg_salpuls <- ddply(salpuls,.(YEAR, SUBSITE, PLOT), summarise,
                     Mean.C.H = mean(Height..cm.))

# ** Community composition and diversity measures ----

# Remove rows with no cover
diversity <- subset(HEcover, cover>0)
# Remove non veg
diversity <- subset(diversity, name !="XXXlitter" & name !="XXXlitter " & 
                      name !="XXXbareground" & name !="XXXbareground " & 
                      name !="XXXrock" & name !="XXXrock " & 
                      name !="XXXfeces" & name !="XXXfeces " & 
                      name !="XXXstandingwater" & name !="XXXstandingwater " & 
                      name !="XXXspider"& name !="Xxxspider"& name !="XXXspider ")
# Add unique plots
diversity$plot_unique <- paste(diversity$sub_name,diversity$plot,diversity$year,sep="")

# Convert to relative cover
plot_cover <- ddply(diversity,.(plot_unique), summarise,
                    total_cover = sum(cover))
diversity$total_cover <- plot_cover$total_cover[match(diversity$plot_unique, plot_cover$plot_unique)]
diversity$rel_cover <- diversity$cover/diversity$total_cover*100

# Modelling and data visualisation ----

# Defining parameter-expanded priors for MCMCglmm models

# For models with one random effect
prior1 <- list(R = list(V = 1, nu=0.002), G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))

# For models with two random effects
prior2 <- list(R = list(V = 1, nu=0.002), G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000), G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))

# Figure 6. Canopy height and radial growth ----

# HE canopy height model
HE_canopy_m <- MCMCglmm(Mean.C.H ~ I(YEAR - 1998), random = ~ YEAR + PLOT, family = "gaussian", data = avg_heights[avg_heights$SUBSITE == "HE",], pr=TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(HE_canopy_m)
#plot(HE_canopy_m$VCV)
#plot(HE_canopy_m$Sol)
autocorr(HE_canopy_m$VCV)

# Calculating model predictions
nyears <- 20
niter <- length(HE_canopy_m$Sol[,"(Intercept)"])
HE_canopy_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    HE_canopy_preds[i,j] <- HE_canopy_m$Sol[i,"(Intercept)"] + HE_canopy_m$Sol[i,"I(YEAR - 1998)"]*j
  }
}

HE_canopy_preds_df <- array(NA, dim = c(nyears, 3))

for (i in 1:nyears){
  HE_canopy_preds_df[i,] <- quantile(HE_canopy_preds[,i], c(0.025, 0.5, 0.975))
}

HE_canopy_preds_df <- cbind.data.frame(lower = HE_canopy_preds_df[,1], 
                                       mean = HE_canopy_preds_df[,2], 
                                       upper = HE_canopy_preds_df[,3], year = seq(1:20))

# KO canopy height model
KO_canopy_m <- MCMCglmm(Mean.C.H ~ I(YEAR - 1998), random = ~ YEAR + PLOT, family = "gaussian", data = avg_heights[avg_heights$SUBSITE=="KO",], pr=TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(KO_canopy_m)
#plot(KO_canopy_m$VCV)
#plot(KO_canopy_m$Sol)
autocorr(KO_canopy_m$VCV)

# Calculating model predictions
nyears <- 20
niter <- length(KO_canopy_m$Sol[,"(Intercept)"])
KO_canopy_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    KO_canopy_preds[i,j] <- KO_canopy_m$Sol[i,"(Intercept)"] + KO_canopy_m$Sol[i,"I(YEAR - 1998)"]*j
  }
}

KO_canopy_preds_df <- array(NA, dim = c(nyears, 3))

for (i in 1:nyears){
  KO_canopy_preds_df[i,] <- quantile(KO_canopy_preds[,i], c(0.025, 0.5, 0.975))
}

KO_canopy_preds_df <- cbind.data.frame(lower = KO_canopy_preds_df[,1], 
                                       mean = KO_canopy_preds_df[,2], 
                                       upper = KO_canopy_preds_df[,3], year = seq(1:20))

# HE + KO canopy height graph
(canopy.height <- ggplot() +
    geom_point(data = avg_heights, aes(x = YEAR, y = Mean.C.H, colour = factor(SUBSITE), 
                                       fill = factor(SUBSITE)), alpha = 0.8, size = 4) +
    scale_color_manual(values = c("#ffa544", "#2b299b"), name="", labels = c("Her.", "Kom.")) +
    scale_fill_manual(values = c("#ffa544","#2b299b")) +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017, 2018)) +
    geom_line(data = HE_canopy_preds_df, aes (x = year + 1998, y = mean), colour = "#ffa544") +
    geom_ribbon(data = HE_canopy_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper),
                fill = "#ffa544", alpha = 0.2) +
    geom_line(data = KO_canopy_preds_df, aes (x = year + 1998, y = mean), colour = "#2b299b") +
    geom_ribbon(data = KO_canopy_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper),
                fill = "#2b299b", alpha = 0.2) +
    guides(fill = FALSE) +
    theme_QHI() +
    theme(legend.position = c(0.1, 0.95), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) +
    labs(x = " ", y = "Mean canopy height (cm)\n", title = "(a) Mean canopy height\n"))

ggsave(canopy.height, filename = "~/Desktop/canopy_height.png", height = 10, width = 20)

# Salix pulchra model
salpuls <- subset(abundance, SPP == "SALPUL"|SPP == "Salix pulchra")
salpuls <- subset(salpuls, !is.na(Height..cm.))
avg_salpuls <- ddply(salpuls, .(YEAR, SUBSITE, PLOT), summarise,
                     Mean.C.H = mean(Height..cm.))

Salix_canopy_m <- MCMCglmm(Height..cm. ~ I(YEAR - 1998), random = ~ YEAR + PLOT, family = "gaussian", data = salpuls, pr=TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(Salix_canopy_m)
#plot(Salix_canopy_m$VCV)
#plot(Salix_canopy_m$Sol)
autocorr(Salix_canopy_m$VCV)

# Calculating model predictions
nyears <- 20
niter <- length(Salix_canopy_m$Sol[,"(Intercept)"])
Salix_canopy_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    Salix_canopy_preds[i,j] <- Salix_canopy_m$Sol[i,"(Intercept)"] + Salix_canopy_m$Sol[i,"I(YEAR - 1998)"]*j
  }
}

Salix_canopy_preds_df <- array(NA, dim = c(nyears, 3))

for (i in 1:nyears){
  Salix_canopy_preds_df[i,] <- quantile(Salix_canopy_preds[,i], c(0.025, 0.5, 0.975))
}

Salix_canopy_preds_df <- cbind.data.frame(lower = Salix_canopy_preds_df[,1], 
                                          mean = Salix_canopy_preds_df[,2], 
                                          upper = Salix_canopy_preds_df[,3], year = seq(1:20))

# Salix pulchra graph
(salix <- ggplot() +
    geom_point(data = avg_salpuls, aes(x = YEAR, y = Mean.C.H), alpha = 0.8, colour = "#008c5f", size = 4) +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017, 2018)) +
    geom_ribbon(data = Salix_canopy_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#008c5f", alpha = 0.2) +
    geom_line(data = Salix_canopy_preds_df, aes(x = year + 1998, y = mean), colour = "#008c5f") +
    theme_QHI() +
    labs(x = "", y = "Height (cm)\n", 
         title = expression(paste("(b) ", italic("Salix pulchra"), " canopy height")),
         subtitle = ""))

# Figure 7. Vegetation cover community composition changes ----

# Biomass (Vegetation cover index) model for HE
biomass_HE_m <- MCMCglmm(Biomass ~ I(YEAR-1998), random = ~ YEAR + PLOT, data = biomass_hits[biomass_hits$SUBSITE == "HE",], family="gaussian", pr=TRUE, nitt = 100000, burnin = 20000, prior=prior2)
summary(biomass_HE_m)
#plot(biomass_HE_m$VCV)
#plot(biomass_HE_m$Sol)
autocorr(biomass_HE_m$VCV)

# Calculate model predictions
nyears <- 20
niter <- length(biomass_HE_m$Sol[,"(Intercept)"])

biomass_HE_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    biomass_HE_preds[i,j] <- biomass_HE_m$Sol[i,"(Intercept)"] + biomass_HE_m$Sol[i,"I(YEAR - 1998)"]*j
  }
}

biomass_HE_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  biomass_HE_preds_df[i,] <- quantile(biomass_HE_preds[,i], c(0.025, 0.5, 0.975))
}

biomass_HE_preds_df <- cbind.data.frame(lower = biomass_HE_preds_df[,1], 
                                        mean = biomass_HE_preds_df[,2], upper = biomass_HE_preds_df[,3], year = seq(1:20))

# Biomass (Vegetation cover index) model for KO
biomass_KO_m <- MCMCglmm(Biomass ~ I(YEAR - 1998), random = ~ YEAR + PLOT, data = biomass_hits[biomass_hits$SUBSITE == "KO",], family = "gaussian", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(biomass_KO_m)
#plot(biomass_KO_m$VCV)
#plot(biomass_KO_m$Sol)
autocorr(biomass_KO_m$VCV)

# Calculate model predictions
nyears <- 20
niter <- length(biomass_KO_m$Sol[,"(Intercept)"])

biomass_KO_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    biomass_KO_preds[i,j] <- biomass_KO_m$Sol[i,"(Intercept)"] + biomass_KO_m$Sol[i,"I(YEAR - 1998)"]*j
  }
}

biomass_KO_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  biomass_KO_preds_df[i,] <- quantile(biomass_KO_preds[,i], c(0.025, 0.5, 0.975))
}

biomass_KO_preds_df <- cbind.data.frame(lower = biomass_KO_preds_df[,1], 
                                        mean = biomass_KO_preds_df[,2], upper = biomass_KO_preds_df[,3], year = seq(1:20))

# Biomass (Vegetation cover index) graph
(veg.cover <- ggplot() +
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
    labs(x = "", y = "Vegetation cover index\n", title = "(a) Vegetation cover\n"))

ggsave(veg.cover, filename = "~/Desktop/veg_cover.png", height = 7, width = 7)

# Bare ground HE model

# Creating a column for the trial hits and misses

bareground$failures <- 100 - bareground$Bareground
bareground_HE_m <- MCMCglmm(cbind(Bareground, failures) ~ I(YEAR - 1998), random = ~YEAR + PLOT, data = bareground[bareground$SUBSITE == "HE",], family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(bareground_HE_m)
#plot(bareground_HE_m$VCV)
#plot(bareground_HE_m$Sol)
autocorr(bareground_HE_m$VCV)

# Calculate model predictions
nyears <- 20
niter <- length(bareground_HE_m$Sol[,"(Intercept)"])

bareground_HE_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    bareground_HE_preds[i,j] <- 100*plogis(bareground_HE_m$Sol[i,"(Intercept)"] + bareground_HE_m$Sol[i,"I(YEAR - 1998)"]*j)
  }
}

bareground_HE_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  bareground_HE_preds_df[i,] <- quantile(bareground_HE_preds[,i], c(0.025, 0.5, 0.975))
}

bareground_HE_preds_df <- cbind.data.frame(lower = bareground_HE_preds_df[,1], 
                                           mean = bareground_HE_preds_df[,2], upper = bareground_HE_preds_df[,3], year = seq(1:20))

# Bare ground KO model
bareground_KO_m <- MCMCglmm(cbind(Bareground, failures) ~ I(YEAR - 1998), random = ~YEAR + PLOT, data = bareground[bareground$SUBSITE == "KO",], family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(bareground_KO_m)
#plot(bareground_KO_m$VCV)
#plot(bareground_KO_m$Sol)
autocorr(bareground_KO_m$VCV)

# Calculate model predictions
nyears <- 20
niter <- length(bareground_KO_m$Sol[,"(Intercept)"])

bareground_KO_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    bareground_KO_preds[i,j] <- 100*plogis(bareground_KO_m$Sol[i,"(Intercept)"] + bareground_KO_m$Sol[i,"I(YEAR - 1998)"]*j)
  }
}

bareground_KO_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  bareground_KO_preds_df[i,] <- quantile(bareground_KO_preds[,i], c(0.025, 0.5, 0.975))
}

bareground_KO_preds_df <- cbind.data.frame(lower = bareground_KO_preds_df[,1], 
                                           mean = bareground_KO_preds_df[,2], upper = bareground_KO_preds_df[,3], year = seq(1:20))

# Bare ground change plot
(bare.ground <- ggplot() +
    geom_point(data = bareground, aes(x = YEAR, y = Bareground, colour = factor(SUBSITE)), alpha = 0.8, size = 4) +
    scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
    scale_fill_manual(values = c("#ffa544","#2b299b")) +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017, 2018)) +
    geom_ribbon(data = bareground_HE_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#ffa544", alpha = 0.2) +
    geom_line(data = bareground_HE_preds_df, aes(x = year + 1998, y = mean), colour = "#ffa544") +
    geom_ribbon(data = bareground_KO_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#2b299b", alpha = 0.2) +
    geom_line(data = bareground_KO_preds_df, aes(x = year + 1998, y = mean), colour = "#2b299b") +
    theme_QHI() +
    theme(legend.position = c(0.9, 0.95), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) +
    labs(x = "", y = "Bare ground (% cover per plot)\n", title = "(b) Bare ground\n"))

ggsave(bare.ground, filename = "~/Desktop/bare_ground.png", height = 7, width = 7)

# Species richness model

#Site alpha diversity
alpha_site <- ddply(diversity,.(sub_name, year), summarise,
                    richness = length(unique(name)))
alpha_site$plot_unique <- paste(alpha_site$sub_name, alpha_site$plot, sep = "")

# Richness HE model
richness_HE_m <- MCMCglmm(richness ~ I(year - 1998), data = alpha_site[alpha_site$sub_name == "QHI:HE",], family = "gaussian", pr = TRUE, nitt = 100000, burnin = 20000)
summary(richness_HE_m)
#plot(richness_HE_m$VCV)
#plot(richness_HE_m$Sol)
autocorr(richness_HE_m$VCV)

# Calculate model predictions
nyears <- 20
niter <- length(richness_HE_m$Sol[,"(Intercept)"])

richness_HE_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    richness_HE_preds[i,j] <- richness_HE_m$Sol[i,"(Intercept)"] + richness_HE_m$Sol[i,"I(year - 1998)"]*j
  }
}

richness_HE_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  richness_HE_preds_df[i,] <- quantile(richness_HE_preds[,i], c(0.025, 0.5, 0.975))
}

richness_HE_preds_df <- cbind.data.frame(lower = richness_HE_preds_df[,1], 
                                         mean = richness_HE_preds_df[,2], upper = richness_HE_preds_df[,3], year = seq(1:20))

# Richness KO model
richness_KO_m <- MCMCglmm(richness ~ I(year - 1998), data = alpha_site[alpha_site$sub_name == "QHI:KO",], family = "gaussian", pr = TRUE, nitt = 100000, burnin = 20000)
summary(richness_KO_m)
#plot(richness_KO_m$VCV)
#plot(richness_KO_m$Sol)
autocorr(richness_KO_m$VCV)

# Calculate model predictions
nyears <- 20
niter <- length(richness_KO_m$Sol[,"(Intercept)"])

richness_KO_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    richness_KO_preds[i,j] <- richness_KO_m$Sol[i,"(Intercept)"] + richness_KO_m$Sol[i,"I(year - 1998)"]*j
  }
}

richness_KO_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  richness_KO_preds_df[i,] <- quantile(richness_KO_preds[,i], c(0.025, 0.5, 0.975))
}

richness_KO_preds_df <- cbind.data.frame(lower = richness_KO_preds_df[,1], 
                                         mean = richness_KO_preds_df[,2], upper = richness_KO_preds_df[,3], year = seq(1:20))

# Richness plot
(richness.plot <- ggplot() +
    geom_point(data = alpha_site, aes(x = year, y = richness, colour = factor(sub_name)), 
               alpha = 0.8, size = 4, position = position_jitter(height = 0.3, width = 0.3)) +
    scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
    scale_fill_manual(values = c("#ffa544","#2b299b", labels = c("Her.", "Kom."))) +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017, 2018)) +
    geom_ribbon(data = richness_HE_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#ffa544", alpha = 0.2) +
    geom_line(data = richness_HE_preds_df, aes(x = year + 1998, y = mean), colour = "#ffa544") +
    geom_ribbon(data = richness_KO_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#2b299b", alpha = 0.2) +
    geom_line(data = richness_KO_preds_df, aes(x = year + 1998, y = mean), colour = "#2b299b") +
    theme_QHI() +
    coord_cartesian(ylim = c(20, 45), xlim = c(1999, 2017)) +
    theme(legend.position = c(0.1, 0.95), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) +
    labs(x = "", y = "Species richness\n", title = "(c) Species richness\n"))

# Evenness calculation
diversity2 <- ddply(diversity,.(sub_name, plot, year, name), summarise,
                    plnp = mean(rel_cover/100 * (log(rel_cover/100))),
                    p2 = mean((rel_cover/100)^2))
diversity2 <- subset(diversity2, !is.na(plnp))
Indices <- ddply(diversity2,.(sub_name, year, plot), summarise,
                 lnS = log(length(unique(name))),
                 Shannon = sum(plnp)*-1,
                 Simpson = 1-(sum(p2)),
                 Evenness = Shannon / lnS)

#Add back in unique plots
Indices$PLOT2 <- paste(Indices$sub_name, Indices$plot, sep="")

Indices_HE <- subset(Indices, sub_name=="QHI:HE")
Indices_KO <- subset(Indices, sub_name=="QHI:KO")

# Evenness HE model
# Creating a column for the trial hits and misses
Indices_HE$failures <- round(100 - 100*Indices_HE$Evenness)

evenness_HE_m <- MCMCglmm(cbind(round(100*Evenness), failures) ~ I(year - 1998), random = ~year + plot, data = Indices_HE, family = "multinomial2", nitt = 100000, burnin = 20000, pr = TRUE, prior = prior2)
summary(evenness_HE_m)
#plot(evenness_HE_m$VCV)
#plot(evenness_HE_m$Sol)
autocorr(evenness_HE_m$VCV)

# Calculate model predictions
nyears <- 19
niter <- length(evenness_HE_m$Sol[,"(Intercept)"])

evenness_HE_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    evenness_HE_preds[i,j] <- 100*plogis(evenness_HE_m$Sol[i,"(Intercept)"] + evenness_HE_m$Sol[i,"I(year - 1998)"]*j)/100
  }
}

evenness_HE_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  evenness_HE_preds_df[i,] <- quantile(evenness_HE_preds[,i], c(0.025, 0.5, 0.975))
}

evenness_HE_preds_df <- cbind.data.frame(lower = evenness_HE_preds_df[,1], 
                                         mean = evenness_HE_preds_df[,2], upper = evenness_HE_preds_df[,3], year = seq(1:19))

# Evenness KO model
# Creating a column for the trial hits and misses
Indices_KO$failures <- round(100 - 100*Indices_KO$Evenness)

evenness_KO_m <- MCMCglmm(cbind(round(100*Evenness), failures) ~ I(year - 1998), random = ~ year + plot, data = Indices_KO, family = "multinomial2", nitt = 100000, burnin = 20000, pr = TRUE, prior = prior2)
summary(evenness_KO_m)
#plot(evenness_KO_m$VCV)
#plot(evenness_KO_m$Sol)
autocorr(evenness_KO_m$VCV)

# Calculate model predictions
nyears <- 19
niter <- length(evenness_KO_m$Sol[,"(Intercept)"])

evenness_KO_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    evenness_KO_preds[i,j] <- 100*plogis(evenness_KO_m$Sol[i,"(Intercept)"] + evenness_KO_m$Sol[i,"I(year - 1998)"]*j)/100
  }
}

evenness_KO_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  evenness_KO_preds_df[i,] <- quantile(evenness_KO_preds[,i], c(0.025, 0.5, 0.975))
}

evenness_KO_preds_df <- cbind.data.frame(lower = evenness_KO_preds_df[,1], 
                                         mean = evenness_KO_preds_df[,2], upper = evenness_KO_preds_df[,3], year = seq(1:19))

# Evenness plot
(evenness.plot <- ggplot() +
    geom_point(data = Indices, aes(x = year, y = Evenness, colour = factor(sub_name)), alpha = 0.8, size = 4) +
    scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
    scale_fill_manual(values = c("#ffa544","#2b299b")) +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017)) +
    geom_ribbon(data = evenness_HE_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#ffa544", alpha = 0.2) +
    geom_line(data = evenness_HE_preds_df, aes(x = year + 1998, y = mean), colour = "#ffa544") +
    geom_ribbon(data = evenness_KO_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), 
                fill = "#2b299b", alpha = 0.2) +
    geom_line(data = evenness_KO_preds_df, aes(x = year + 1998, y = mean), colour = "#2b299b") +
    theme_QHI() +
    coord_cartesian(ylim = c(0.40, 1), xlim = c(1999, 2017)) +
    theme(legend.position = c(0.1, 0.95), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) +
    labs(x = "", y = "Evenness\n", title = "(d) Evenness\n"))

# Herschel type model for E. vaginatum
HE_plots_ev <- subset(HEcoverHE, name == "Eriophorum vaginatum")

# Creating a column for the trial hits and misses
HE_plots_ev$failures <- round(100 - HE_plots_ev$cover)

HE_plot_evag <- MCMCglmm(cbind(round(cover), failures) ~ I(year - 1998), random = ~ year + plot, data = HE_plots_ev, family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(HE_plot_evag)  
#plot(HE_plot_evag$VCV)  
#plot(HE_plot_evag$Sol)
autocorr(HE_plot_evag$VCV)

# Calculating model predictions
nyears <- 20
niter <- length(HE_plot_evag$Sol[,"(Intercept)"])

HE_plot_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    HE_plot_preds[i,j] <- 100*plogis(HE_plot_evag$Sol[i,"(Intercept)"] + HE_plot_evag$Sol[i,"I(year - 1998)"]*j)
  }
}

HE_plot_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  HE_plot_preds_df[i,] <- quantile(HE_plot_preds[,i], c(0.025, 0.5, 0.975))
}

HE_plot_preds_df <- cbind.data.frame(lower = HE_plot_preds_df[,1], 
                                     mean = HE_plot_preds_df[,2], upper = HE_plot_preds_df[,3], year = seq(1:20))

# Herschel type model for S. pulchra
HE_plots_sp <- subset(HEcoverHE, name == "Salix pulchra")

# Creating a column for the trial hits and misses
HE_plots_sp$failures <- round(100 - HE_plots_sp$cover)

HE_plot_salpul <- MCMCglmm(cbind(round(cover), failures) ~ I(year - 1998), random = ~ year + plot, data = HE_plots_sp, family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(HE_plot_salpul)  
#plot(HE_plot_salpul$VCV)  
#plot(HE_plot_salpul$Sol)
autocorr(HE_plot_salpul$VCV)

# Calculating model predictions
nyears <- 20
niter <- length(HE_plot_salpul$Sol[,"(Intercept)"])

HE_plot_preds2 <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    HE_plot_preds2[i,j] <- 100*plogis(HE_plot_salpul$Sol[i,"(Intercept)"] + HE_plot_salpul$Sol[i,"I(year - 1998)"]*j)
  }
}

HE_plot_preds_df2 <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  HE_plot_preds_df2[i,] <- quantile(HE_plot_preds2[,i], c(0.025, 0.5, 0.975))
}

HE_plot_preds_df2 <- cbind.data.frame(lower = HE_plot_preds_df2[,1], 
                                      mean = HE_plot_preds_df2[,2], upper = HE_plot_preds_df2[,3], year = seq(1:20))

# Herschel type graph
(herschel <- ggplot() +
    geom_point(data = subset(HEcoverHE, name == "Eriophorum vaginatum" | name == "Salix pulchra"), 
               aes(x = year, y = cover, colour = factor(name)), alpha = 0.8, size = 4) +
    scale_color_manual(values = c("#fc8d62","#008c5f"), name = "", labels = c("Eriophorum vaginatum", "Salix pulchra")) +
    scale_fill_manual(values = c("#fc8d62","#008c5f"), name = "", labels = c("Eriophorum vaginatum", "Salix pulchra")) +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017, 2018)) +
    geom_ribbon(data = HE_plot_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), fill = "#fc8d62", alpha = 0.2) +
    geom_line(data = HE_plot_preds_df, aes(x = year + 1998, y = mean), colour = "#fc8d62") +
    geom_ribbon(data = HE_plot_preds_df2, aes(x = year + 1998, ymin = lower, ymax = upper), fill = "#008c5f", alpha = 0.2) +
    geom_line(data = HE_plot_preds_df2, aes(x = year + 1998, y = mean), colour = "#008c5f") +
    theme_QHI() +
    theme(legend.position = c(0.28, 0.95), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) +
    labs(x = "", y = "Species cover per plot\n", title = "(e) Herschel plots\n"))

# Komakuk type model

# Arctagrostis latifolia
KO_plots_al <- subset(HEcoverKO,name == "Arctagrostis latifolia")

# Creating a column for the trial hits and misses
KO_plots_al$failures <- round(100 - KO_plots_al$cover)

# Binomial model
KO_plot_arclat <- MCMCglmm(cbind(round(cover), failures) ~ I(year - 1998), random = ~ year + plot, data = KO_plots_al, family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(KO_plot_arclat)
#plot(KO_plot_arclat$VCV)
#plot(KO_plot_arclat$Sol)
autocorr(KO_plot_arclat$VCV)

# Calculating model predictions - binomial
nyears <- 20
niter <- length(KO_plot_arclat$Sol[,"(Intercept)"])

KO_plot_preds <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    KO_plot_preds[i,j] <- 100*plogis(KO_plot_arclat$Sol[i,"(Intercept)"] + KO_plot_arclat$Sol[i,"I(year - 1998)"]*j)
  }
}

KO_plot_preds_df <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  KO_plot_preds_df[i,] <- quantile(KO_plot_preds[,i], c(0.025, 0.5, 0.975))
}

KO_plot_preds_df <- cbind.data.frame(lower = KO_plot_preds_df[,1],
                                     mean = KO_plot_preds_df[,2], upper = KO_plot_preds_df[,3], year = seq(1:20))

# Komakuk type model for Alopecurus alpinus
KO_plots_aa <- subset(HEcoverKO, name == "Alopecurus alpinus")

# Creating a column for the trial hits and misses
KO_plots_aa$failures <- round(100 - KO_plots_aa$cover)

# Binomial model
KO_plot_aloalp <- MCMCglmm(cbind(round(cover), failures) ~ I(year - 1998), random = ~ year + plot, data = KO_plots_aa, family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(KO_plot_aloalp)
#plot(KO_plot_aloalp$VCV)
#plot(KO_plot_aloalp$Sol)
autocorr(KO_plot_aloalp$VCV)

# Calculating model predictions - binomial
nyears <- 20
niter <- length(KO_plot_aloalp$Sol[,"(Intercept)"])

KO_plot_preds2 <- array(NA, dim = c(niter,nyears))

for (i in 1:niter){
  for (j in 1:nyears){
    KO_plot_preds2[i,j] <- 100*plogis(KO_plot_aloalp$Sol[i,"(Intercept)"] + KO_plot_aloalp$Sol[i,"I(year - 1998)"]*j)
  }
}

KO_plot_preds_df2 <- array(NA, dim = c(nyears,3))

for (i in 1:nyears){
  KO_plot_preds_df2[i,] <- quantile(KO_plot_preds2[,i], c(0.025, 0.5, 0.975))
}

KO_plot_preds_df2 <- cbind.data.frame(lower = KO_plot_preds_df2[,1],
                                      mean = KO_plot_preds_df2[,2], upper = KO_plot_preds_df2[,3], year = seq(1:20))

# Komakuk type graph
(komakuk <- ggplot() +
    geom_point(data = subset(HEcoverKO, name == "Arctagrostis latifolia" | name == "Alopecurus alpinus"), 
               aes(x = year, y = cover, colour = factor(name)), alpha = 0.8, size = 4) +
    scale_color_manual(values = c("#ffcd44","#1b74d3"), name = "", labels = c("Alopecurus alpinus", "Arctagrostis latifolia")) +
    scale_fill_manual(values = c("#ffcd44","#1b74d3"), name = "") +
    scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2017)) +
    geom_ribbon(data = KO_plot_preds_df, aes(x = year + 1998, ymin = lower, ymax = upper), fill = "#1b74d3", alpha = 0.2) +
    geom_line(data = KO_plot_preds_df, aes(x = year + 1998, y = mean), colour = "#1b74d3") +
    geom_ribbon(data = KO_plot_preds_df2, aes(x = year + 1998, ymin = lower, ymax = upper), fill = "#ffcd44", alpha = 0.2) +
    geom_line(data = KO_plot_preds_df2, aes(x = year + 1998, y = mean), colour = "#ffcd44") +
    theme_QHI() +
    theme(legend.position = c(0.25, 0.95), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) +
    labs(x = "", y = "Species cover per plot\n", title = "(f) Komakuk plots\n") +
    coord_cartesian(ylim = c(0, 100), xlim = c(1999, 2017)))

#png(file="~/Desktop/Figure8_com_comp.png", width = 1000, height = 1500)
grid.arrange(veg.cover, bare.ground, richness.plot, evenness.plot, herschel, komakuk, ncol=2)
dev.off()

# All species model Herschel Vegetation Type

# Creating a column for the trial hits and misses
HEcoverHE$failures <- round(100 - HEcoverHE$cover)

# Linear model
HE_plot_all_linear <- MCMCglmm(cover ~ I(year - 1998) * name - 1, random = ~ year + plot, data = HEcoverHE, family = "gaussian", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(HE_plot_all_linear)

# Binomial model
HE_plot_all <- MCMCglmm(cbind(round(cover), failures) ~ I(year - 1998) * name, random = ~ year + plot, data = HEcoverHE, family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(HE_plot_all)
#plot(HE_plot_all$VCV)
#plot(HE_plot_all$Sol)

# All species model Komakuk Vegetation Type

# Creating a column for the trial hits and misses
HEcoverKO$failures <- round(100 - HEcoverKO$cover)

# Linear model
KO_plot_all_linear <- MCMCglmm(cover ~ I(year - 1998) * name -1, random = ~ year + plot, data = HEcoverKO, 
                               family = "gaussian", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(KO_plot_all_linear)

# Binomial model
KO_plot_all <- MCMCglmm(cbind(round(cover), failures) ~ I(year - 1998) * name, random = ~ year + plot, data = HEcoverKO, family = "multinomial2", pr = TRUE, nitt = 100000, burnin = 20000, prior = prior2)
summary(KO_plot_all)
#plot(HE_plot_all$VCV)
#plot(HE_plot_all$Sol)

# Figure 8. Species accumulation curves ----
SAC.acc <- read.csv("~/Desktop/SACaccumulated2018.csv")

SAC.acc2017 <- SAC.acc %>% filter(year == "2017")

(SAC.plot <- ggplot() +
   geom_point(data = SAC.acc2017, aes(x = distance.all, y = accumulated, 
                                  colour = factor(veg.type)), alpha = 0.8, size = 6) +
   geom_smooth(data = SAC.acc, aes(x = distance.all, y = accumulated, colour = factor(veg.type), 
                                   fill = factor(veg.type)), alpha = 0.2, show.legend = FALSE) +
   scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
   scale_fill_manual(values = c("#ffa544","#2b299b"), name = "", labels = c("Her.", "Kom.")) +
   theme_QHI() +
   theme(legend.position = c(0.1, 0.9), 
         axis.line.x = element_line(color="black", size = 0.5),
         axis.line.y = element_line(color="black", size = 0.5),
         axis.text = element_text(size = 30),
         axis.title = element_text(size = 36),
         legend.text = element_text(size = 26)) +
   labs(x = "Distance (m)", y = "Number of species\n", title = "2017\n") +
   guides(fill = FALSE))

SAC.acc2018 <- SAC.acc %>% filter(year == "2018")

(SAC.plot2018 <- ggplot() +
    geom_point(data = SAC.acc2018, aes(x = distance.all, y = accumulated, 
                                       colour = factor(veg.type)), alpha = 0.8, size = 6) +
    geom_smooth(data = SAC.acc, aes(x = distance.all, y = accumulated, colour = factor(veg.type), 
                                    fill = factor(veg.type)), alpha = 0.2, show.legend = FALSE) +
    scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
    scale_fill_manual(values = c("#ffa544","#2b299b"), name = "", labels = c("Her.", "Kom.")) +
    theme_QHI() +
    theme(legend.position = c(0.1, 0.9), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 36),
          legend.text = element_text(size = 26)) +
    labs(x = "Distance (m)", y = "Number of species\n", title = "2018\n") +
    guides(fill = FALSE))

panel <- grid.arrange(SAC.plot, SAC.plot2018, ncol = 2)
ggsave(panel, filename = "~/Desktop/SACpanel.png", height = 10, width = 20)


# Species accumulation models ----

# Herschel
# The best convergence of the three! And autocorrelation is within the limits!
HE_SAC_m <- MCMCglmm(accumulated ~ log(distance.all), family = "gaussian", 
                     data = SAC.acc[SAC.acc$veg.type == "HE",], pr=TRUE, nitt = 100000, burnin = 20000)
summary(HE_SAC_m)
#plot(HE_SAC_m$VCV)
#plot(HE_SAC_m$Sol)
autocorr(HE_SAC_m$VCV)

# Calculating model predictions
ndist <- 90
niter <- length(HE_SAC_m$Sol[,"(Intercept)"])
HE_SAC_preds <- array(NA, dim = c(niter,ndist))

for (i in 1:niter){
  for (j in 1:ndist){
    HE_SAC_preds[i,j] <- HE_SAC_m$Sol[i,"(Intercept)"] + HE_SAC_m$Sol[i,"log(distance.all)"]*log(j)
  }
}

HE_SAC_preds_df <- array(NA, dim = c(ndist, 3))

for (i in 1:ndist){
  HE_SAC_preds_df[i,] <- quantile(HE_SAC_preds[,i], c(0.025, 0.5, 0.975))
}

HE_SAC_preds_df <- cbind.data.frame(lower = HE_SAC_preds_df[,1], 
                                    mean = HE_SAC_preds_df[,2], 
                                    upper = HE_SAC_preds_df[,3], 
                                    distance = seq(1:90))

# Adding on a predictin for 0.125 meters and 0 species at 0 meters
new_df <- array(NA, dim = c(2,4))
new_df[1,1] <- 0
new_df[1,2] <- 0
new_df[1,3] <- 0
new_df[1,4] <- 0
new_df[2,2] <- mean(HE_SAC_m$Sol[,"(Intercept)"] + HE_SAC_m$Sol[,"log(distance.all)"]*log(0.125))
new_df[2,4] <- 0.125
new_df[2,1] <- HPDinterval(HE_SAC_m$Sol[,"(Intercept)"] + HE_SAC_m$Sol[,"log(distance.all)"]*log(0.125))[,1]
new_df[2,3] <- HPDinterval(HE_SAC_m$Sol[,"(Intercept)"] + HE_SAC_m$Sol[,"log(distance.all)"]*log(0.125))[,2]
colnames(new_df) <- c("lower", "mean", "upper", "distance")
HE_SAC_preds_df <- rbind(HE_SAC_preds_df, new_df)

# Komakuk type
KO_SAC_m <- MCMCglmm(accumulated ~ log(distance.all), family = "gaussian", 
                     data = SAC.acc[SAC.acc$veg.type == "KO",], pr=TRUE, nitt = 100000, burnin = 20000)
summary(KO_SAC_m)
#plot(KO_SAC_m$VCV)
#plot(KO_SAC_m$Sol)
autocorr(KO_SAC_m$VCV)

# Calculating model predictions
ndist <- 105
niter <- length(KO_SAC_m$Sol[,"(Intercept)"])
KO_SAC_preds <- array(NA, dim = c(niter,ndist))

for (i in 1:niter){
  for (j in 1:ndist){
    KO_SAC_preds[i,j] <- KO_SAC_m$Sol[i,"(Intercept)"] + KO_SAC_m$Sol[i,"log(distance.all)"]*log(j)
  }
}

KO_SAC_preds_df <- array(NA, dim = c(ndist, 3))

for (i in 1:ndist){
  KO_SAC_preds_df[i,] <- quantile(KO_SAC_preds[,i], c(0.025, 0.5, 0.975))
}

KO_SAC_preds_df <- cbind.data.frame(lower = KO_SAC_preds_df[,1], 
                                    mean = KO_SAC_preds_df[,2], 
                                    upper = KO_SAC_preds_df[,3], 
                                    distance = seq(1:105))

# Adding on a predictin for 0.125 meters and 0 species at 0 meters
new_df <- array(NA, dim = c(2,4))
new_df[1,1] <- 0
new_df[1,2] <- 0
new_df[1,3] <- 0
new_df[1,4] <- 0
new_df[2,2] <- mean(KO_SAC_m$Sol[,"(Intercept)"] + KO_SAC_m$Sol[,"log(distance.all)"]*log(0.125))
new_df[2,4] <- 0.125
new_df[2,1] <- HPDinterval(KO_SAC_m$Sol[,"(Intercept)"] + KO_SAC_m$Sol[,"log(distance.all)"]*log(0.125))[,1]
new_df[2,3] <- HPDinterval(KO_SAC_m$Sol[,"(Intercept)"] + KO_SAC_m$Sol[,"log(distance.all)"]*log(0.125))[,2]
KO_SAC_preds_df <- rbind(KO_SAC_preds_df, new_df)

# Species pool figures with model predictions ----
(SAC.plot2 <- ggplot() +
   geom_point(data = SAC.acc, aes(x = distance.all, y = accumulated, 
                                  colour = factor(veg.type)), alpha = 0.8, size = 8) +
   geom_ribbon(data = HE_SAC_preds_df, aes(x = distance, ymin = lower, ymax = upper), 
               fill = "#ffa544", alpha = 0.2) +
   geom_line(data = HE_SAC_preds_df, aes(x = distance, y = mean), colour = "#ffa544") +
   geom_ribbon(data = KO_SAC_preds_df, aes(x = distance, ymin = lower, ymax = upper), 
               fill = "#2b299b", alpha = 0.2) +
   geom_line(data = KO_SAC_preds_df, aes(x = distance, y = mean), colour = "#2b299b") +
   scale_color_manual(values = c("#ffa544", "#2b299b"), name = "", labels = c("Her.", "Kom.")) +
   scale_fill_manual(values = c("#ffa544","#2b299b"), name = "", labels = c("Her.", "Kom.")) +
   theme_QHI() +
   theme(legend.position = c(0.1, 0.9), 
         axis.line.x = element_line(color="black", size = 0.5),
         axis.line.y = element_line(color="black", size = 0.5),
         axis.text = element_text(size = 30),
         axis.title = element_text(size = 36),
         legend.text = element_text(size = 26)) +
   labs(x = "Distance (m)", y = "Number of species\n") +
   guides(fill = FALSE))

png(file="~/Desktop/Figure10_SAC2.png", width = 750, height = 750)
grid.arrange(SAC.plot2, ncol = 1)
dev.off()