#################################################################
# Generate dataframe of distances for each combination of nodes #
#################################################################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('stringr')
library('maptools')
library('geosphere')

raw <-      paste0(wd, "raw")
output <-   paste0(wd, "output")
temp <-     paste0(wd, "temp")

#sink(paste0(temp, "/calculate_distance.txt"))

# User input
node.interval <- 5 # degrees
##################
# Import file
nodes <- read.csv(file.path(temp, 'ports_nodes.csv'), stringsAsFactors = F)
data(wrld_simpl)

# Fill df of all combinations of nodes
longlat.df <- data.frame(long1 = character(0),
                         lat1 = character(0),
                         type1 = character(0),
                         country1 = character(0),
                         long2 = character(0),
                         lat2 = character(0),
                         type2 = character(0),
                         country2 = character(0))

# For each node, calculate distance to four closes nodes
# The distance should not be greater than the user-defined degree distance (e.g. 5d), remove for now
# The edge should not fall on land (divide distance into 100 and make sure none fall on land)
pri <- nrow(nodes)
for (i in 1:pri) {
  
  # Illustrate progress
  print(paste0('Row ', i, " which is a ", nodes$type[i]))
  
  assign('df', nodes)
  
  base.lat <- df$lat[i]
  base.long <- df$long[i]
  base.country <- df$country[i]
  base.type <- df$type[i]
  
  df <- df[(i+1):nrow(df), ]
  df$diff.lat <- abs(df$lat - base.lat)
  df$diff.long <- abs(df$long - base.long)
  df$diff.avg <- (df$diff.lat + df$diff.long)/2
  
  # Top four closest nodes
  df <- arrange(df, diff.avg)
  df.close <- df[1:4,]
  #df.close <- subset(df.close, diff.avg <= node.interval)
  
  # Divide edges into 100 and make sure none are over land
  base.latlong <- cbind(base.lat, base.long)
  
  land.list <- list()
  if (nrow(df.close) > 0) {
    for (j in 1:nrow(df.close)) {
      
      close.lat <- df.close$lat[j]
      close.long <- df.close$long[j]
      
      fill.lat <- seq(from = base.lat, to = close.lat, length.out = 10)
      fill.long <- seq(from = base.long, to = close.long, length.out = 10)
      close.longlat <- expand.grid(fill.long, fill.lat)
      names(close.longlat) <- c('long', 'lat')
      
      nodes.map <- SpatialPoints(close.longlat,
                                 proj4string=CRS(proj4string(wrld_simpl)))
      land.cover <- !is.na(over(nodes.map, wrld_simpl)$FIPS)
      
      if (any(land.cover) == T) {
        land.list <- c(land.list, j)
      }
    }
  
  land.list <- as.numeric(land.list)
  print(paste0("Number of closest sea nodes: ", (nrow(df.close) - length(land.list))))
  
  # Append node combinations to longlat.df
  out.df <- df.close[c('long', 'lat', 'type', 'country')]
  names(out.df) <- c('long2', 'lat2', 'type2', 'country2')
  out.df$long1 <- base.long
  out.df$lat1 <- base.lat
  out.df$type1 <- base.type
  out.df$country1 <- base.country
  out.df <- out.df[c('long1', 'lat1', 'type1', 'country1',
                     'long2', 'lat2', 'type2', 'country2')]
  
  # Remove observations that are landcover
  if (length(land.list) != 0) { 
    out.df <- out.df[-c(land.list),]
    }
      
  longlat.df <- rbind(longlat.df, out.df)
  }
}


# Calculate distance
longlat.df$distance <- distHaversine(longlat.df[, 1:2], 
                                     longlat.df[, 5:6])

# Write files
saveRDS(longlat.df, file.path(temp, "node_combinations.rds"))

sink()



  
  
  