##################################################################### 
# Identify port-port combination with shortest distance per ij pair #
#####################################################################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')

input <-    paste0(wd, "output/derived/nodes")
output <-   paste0(wd, "output/derived/nodes")
temp <-     paste0(wd, "temp/")
raw <-      paste0(wd, "raw/")

# User input
node.interval <- 5 # degrees
#######################################################
# Import route file
spath <- readRDS(file.path(input, 'shortest_paths.rds'))

# Omit same country trade
spath <- subset(spath, port1.country != port2.country)

# Calculate distance between each step
spath <- dplyr::arrange(spath, route_id, step_id)

spath <- dplyr::group_by(spath, route_id) %>%
         dplyr::mutate(l1.long = lag(node.long),
                       l1.lat = lag(node.lat))

spath$distance <- geosphere::distHaversine(spath[c('node.long', 'node.lat')],
                                           spath[c('l1.long', 'l1.lat')])

# Subset if path is too long (this means shortest route can't be calculated)
spath <- dplyr::group_by(spath, route_id) %>%
         dplyr::mutate(min.dist = min(distance, na.rm = T))

max.dist <- geosphere::distHaversine(c(50, 0),
                                    c(50+node.interval,0))
max.dist <- sqrt(max.dist^2 + max.dist^2)

spath <- subset(spath, min.dist <= max.dist & !is.na(min.dist))
hist(spath$min.dist)

# Collapse distance to port-port level
spath <- dplyr::group_by(spath, port1, port2, port1.country, port2.country,
                         port1.long, port2.long, port1.lat, port2.lat,
                         route_id) %>%
         dplyr::summarise(distance = sum(distance, na.rm = T))
isid('spath', c('route_id'))

# Identify shortest route by country-country 
spath <- dplyr::group_by(spath, port1.country, port2.country) %>%
         dplyr::mutate(min.distance = min(distance, na.rm = T))

spath.out <- subset(spath, distance == min.distance)
spath.out <- spath.out[c('port1', 'port2', 
                         'port1.country', 'port2.country',
                         'port1.long', 'port2.long',
                         'port1.lat', 'port2.lat',
                         'route_id', 'distance')]
spath.out <- unique(spath.out)

# Remove duplicates (should be 16)
spath.out <- dplyr::group_by(spath.out, port1.country, port2.country) %>%
             dplyr::mutate(count = n(),
                           rown = row_number())
spath.out <- subset(spath.out, count == 1 | (count == 2 & rown == 1))
spath.out$count <- spath.out$rown <- NULL

isid('spath.out', c('port1.country', 'port2.country'))

# Save file
saveRDS(spath.out, file.path(output, 'ij_ports.rds'))