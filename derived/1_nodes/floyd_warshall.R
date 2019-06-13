###############################
# Run Floyd-Washall Algorithm #
###############################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('maptools')

output <-   paste0(wd, "output/")
temp <-     paste0(wd, "temp/")
raw <-      paste0(wd, "raw/")

# Import files
nodes <- readRDS(file.path(temp, 'node_combinations.rds'))

# Import map
data(wrld_simpl)
node.map <- unique(nodes[c('long1', 'lat1')])
nodes.map <- SpatialPoints(node.map,
                           proj4string=CRS(proj4string(wrld_simpl)))
plot(wrld_simpl)
points(nodes.map)