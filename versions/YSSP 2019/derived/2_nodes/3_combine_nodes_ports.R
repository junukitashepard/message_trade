#############################################
# Combine uniform nodes and major sea ports #
#############################################
# Import files
major.ports <- read.csv(file.path(major.ports.csv), stringsAsFactors = F)
uniform.nodes <- read.csv(file.path(temp, 'uniform_nodes.csv'), stringsAsFactors = F)

# Keep only sea nodes
uniform.nodes <- subset(uniform.nodes, land_cover == FALSE)
uniform.nodes$type <- 'SEA.NODE'
uniform.nodes$country <- NA

# Keep relevant variables
major.ports <- major.ports[c('country', 'long', 'lat')]
  major.ports$type <- 'SEAPORT.NODE'
  
uniform.nodes <- uniform.nodes[c('country', 'type', 'long', 'lat')]

# Combine files
all.nodes <- rbind(major.ports, uniform.nodes)
all.nodes <-  subset(all.nodes, !is.na(long) & !is.na(lat))

# Map out the ports and nodes
data(wrld_simpl)
all.nodes.map <- unique(all.nodes[c('long', 'lat')])
nodes.map <- SpatialPoints(all.nodes.map,
                           proj4string=CRS(proj4string(wrld_simpl)))

land.cover <- !is.na(over(nodes.map, wrld_simpl)$FIPS) # Some ports are inland waterways
all.nodes$type[land.cover == T] <- 'INLANDPORT.NODE'

# Assign colors
all.nodes$col[all.nodes$type == 'SEA.NODE'] <- 'gray45'
all.nodes$col[all.nodes$type == 'SEAPORT.NODE'] <- 'dodgerblue4'
all.nodes$col[all.nodes$type == 'INLANDPORT.NODE'] <- 'forestgreen'

plot(wrld_simpl)
points(all.nodes.map, col = all.nodes$col)

# Assign node names
all.nodes <- dplyr::group_by(all.nodes, long, lat) %>% dplyr::mutate(count = n())
all.nodes <- subset(all.nodes, count == 1)
all.nodes$count <- NULL

all.nodes$node.name <- seq(length = nrow(unique(all.nodes[c('long', 'lat')])))

# Write file
write.csv(all.nodes, file.path(temp, 'ports_nodes.csv'))