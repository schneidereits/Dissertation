# cc tutorial data clustering
# 8.2.2020

#install.packages("recluster")
#install.packages("phytools")
#install.packages("maps")
#install.packages("vegan")

# Loading libraries
library(recluster)
library(maps)
library(phytools)
library(stats)
library(cluster)
library(readr)

# Import ----

# Loading the dataframes we'll be working with:
# First load the data-frame containing all species for which NeoTropTree has at least one record in Bolivia.
spp <- read.csv("other/CC-Data-clustering-tutorial/spp_bol.csv")
head(spp)  # View the first few columns
dim(spp)  # How many rows and columns are there?
sites <- read_csv("other/CC-Data-clustering-tutorial/sites_bolivia.csv")
View(sites)
# Load the sppxarea matrix
sppxsites <- read_csv("other/CC-Data-clustering-tutorial/sppxsites_bol.csv")
head(sppxsites)
dim(sppxsites)

# presence/absence matrix ----

# Making the species by site matrix (presence and abscence). We'll call it `commat`.

sites_sub <- unique(sppxsites$AreaID)  # Making a vector with sites in our dataset
spp_sub <- unique(sppxsites$SppID)  # Making a vector with species in our dataset

# First we'll create an empty matrix with our sites in the rows and our species in the columns. The loop function will place a `1` on a given cell when the species is present in an area and will fill out the remaining cells with a `0`.

spp_commat <- matrix(0, length(sites_sub), length(spp_sub))
for (i in 1:nrow(spp_commat)){
  temp_sites <- sppxsites[which(sppxsites$AreaID == sites_sub[i]),]
  spp_commat[i, which(spp_sub%in%temp_sites$SppID)] <- 1
  print(i)
}

# Now let's name our rows and columns with the codes for the sites and the codes for the species.
rownames(spp_commat) <- as.character(sites$AreaCode[match(sites_sub, sites$AreaID)])
colnames(spp_commat) <- as.character(spp$Species.code[match(spp_sub, spp$SppID)])
dim(spp_commat)

# Check if the loop function worked alright and did its job
spp_commat[1:6,1:6]

# removing noise aka unique species
spp_commat_trim <- spp_commat[,which(!colSums(spp_commat) == 1)]
dim(spp_commat_trim)

# We removed 275 species from our dataset. We'll check if this makes much of a difference later on.

# the clustering beginns ----

help("vegdist")
help("recluster.dist")

# Picking a metric is difficult, but calculating it is actually simple. Simply use the recluster.dist command or the vegdist command in order to estimate such distances.
simpson_dist <- recluster.dist(spp_commat_trim, dist="simpson")
jaccard_dist <- recluster.dist(spp_commat_trim, dist="jaccard")
sorensen_dist <- recluster.dist(spp_commat_trim, dist="sorensen")
euclidian_dist <- vegdist(spp_commat_trim, method="euclidean")

# linkage methods

help(hclust)
bol_singlelink <- recluster.cons(spp_commat_trim, tr = 1, p = 0.5, dist = "simpson", method = "single")
bol_singlelink_tmp <- bol_singlelink$cons  # Selecting the consensus tree (we'll discuss it later)
plot(bol_singlelink_tmp, direction = "downwards", cex = 0.5)  # You can change the "direction" argument to your liking.

# the write.tree function will let you save your cluster in TRE format so you can open it with Figtree and visualise it better.
write.tree(bol_singlelink_tmp, "bol_singlelink_tmp.tre")

