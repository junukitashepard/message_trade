##########################
# Generate node data set #
##########################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')

output <-   paste0(wd, "output/")
temp <-     paste0(wd, "temp/")

# User inputs
node.interval <- 2 # degrees
##############################
# Set up nodes
long <- seq(from = -180, to = 180, by = node.interval)
lat <- seq(from = -90, to = 90, by = node.interval)
node.df <- expand.grid(long, lat)

# Import map
data(wrld_simpl)
nodes.map <- SpatialPoints(node.df,
                           proj4string=CRS(proj4string(wrld_simpl)))
# Remove land cover
land.cover <- !is.na(over(nodes.map, wrld_simpl)$FIPS)

# Include indicator in node.df
node.df$land_cover <- land.cover

# Map on global projection
plot(wrld_simpl)
points(nodes.map, col = 1+land.cover)

# Save nodes as csv in temp folder
names(node.df) <- c('long', 'lat', 'land_cover')

write.csv(node.df, file.path(temp, "uniform_nodes.csv"))

