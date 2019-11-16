# 16.11.2019
# Geographic data in R


# install ----
#install.packages("sf")
#install.packages("raster")
#install.packages("spData")
# devtools::install_github("Nowosad/spDataLarge")
#install.packages("devtools")
# library ----
library(sf)
library(raster)
library(spData)
library(spDataLarge)   # load larger geographic data

# An introduction to simple features ----
vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

names(world)
plot(world)

summary(world["lifeExp"]) #The commonly used summary() function, 
                          #for example, provides a useful overview of the
                          #variables within the world object.

world_mini = world[1:2, 1:3]
world_mini
library(sp)
world_sp = as(world, Class = "Spatial")
world_sf = st_as_sf(world_sp, "sf")

# Basic map making ----

plot(world[3:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# 2.2.4 Base plot arguments ----