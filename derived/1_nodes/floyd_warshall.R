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
library('e1071')
library('tidyr')

output <-   paste0(wd, "output/")
temp <-     paste0(wd, "temp/")
raw <-      paste0(wd, "raw/")
################################
# Import files
nodes <- readRDS(file.path(temp, 'node_combinations.rds'))

# Import map
data(wrld_simpl)
node.map <- unique(nodes[c('long1', 'lat1')])
nodes.map <- SpatialPoints(node.map,
                           proj4string=CRS(proj4string(wrld_simpl)))
plot(wrld_simpl)
points(nodes.map)

# Get return path on nodes
nodes.return <- nodes
names(nodes.return) <- c('long2', 'lat2', 'type2', 'country2', 
                         'long1', 'lat1', 'type1', 'country1', 'distance')

nodes <- nodes[c('long1', 'lat1', 'type1', 'country1',
                 'long2', 'lat2', 'type2', 'country2',
                 'distance')]
nodes.return <- nodes.return[c('long1', 'lat1', 'type1', 'country1',
                               'long2', 'lat2', 'type2', 'country2',
                               'distance')]

nodes <- unique(rbind(nodes, nodes.return))

# Remove duplicates from nodes
nodes <- dplyr::group_by(nodes, long1, lat1, long2, lat2) %>%
  dplyr::mutate(count = n())
nodes <- subset(nodes, count == 1)
nodes$count <- NULL
isid('nodes', c('long1', 'lat1', 'long2', 'lat2'))

# Function, FW_calculate: Calculate Floyd-Warshall shortest path
FW_calculate <- function(infile, node1, node2) {
  
  environment(assert) <- environment(isid) <- environment()
  
  assign('node.edges', get(infile))
  node.edges <- unique(node.edges[c(node1, node2, 'distance')])
  isid('node.edges', c(node1, node2))
  
  # Assign unique node names
  assign('unique.n1', unique(node.edges[c('long1', 'lat1')]))
  assign('unique.n2', unique(node.edges[c('long2', 'lat2')]))
  names(unique.n1) <- names(unique.n2) <- c('long', 'lat')
  
  assign('node.names', unique(full_join(unique.n1, unique.n2, by = c('long', 'lat'))))
  isid('node.names', c('lat', 'long'))
  
  node.names$node.name <- as.character(seq(length = nrow(node.names)))
  
  saveRDS(node.names, file.path(temp, 'node_names.rds'))
  
  node.edges <- inner_join(node.edges, node.names, by = c('long1' = 'long', 'lat1' = 'lat'))
  node.edges <- inner_join(node.edges, node.names, by = c('long2' = 'long', 'lat2' = 'lat'))
  
  names(node.edges) <- c(node1, node2,
                         'distance', 'node.name1', 'node.name2')
  
  # Generate matrix for shortest path calculation
  print("Generate matrix for shortest path calculation")
  assign('path.list', node.edges[c('node.name1', 'node.name2', 'distance')])
  
  assign('unique.node', as.character(unique(node.names$node.name)))
  unique.node <- sort(unique.node, decreasing = FALSE)
  
  print("Fill in matrix with distances (NA where missing)")
  assign('j', as.data.frame(unique.node))
  j$unique.node <- as.character(j$unique.node)
  j$idx <- dplyr::row_number(j)
  
  assign('k', left_join(node.edges, j, by = c('node.name1' = 'unique.node')))
  k <- left_join(k, j, by = c('node.name2' = 'unique.node'))
  assert('!is.na(k$idx.x) & !is.na(k$idx.y)')
  k <- dplyr::arrange(k, idx.x, idx.y)

  assign('unames', as.character(unique(k$idx.x)))
  assign('path.mat', matrix(nrow = length(unames),
                            ncol = length(unames)))
  
  saveRDS(k, file.path(temp, 'k.rds')) # save node information
  
  rownames(path.mat) <- sort(unames)
  colnames(path.mat) <- sort(unames)
  assert('rownames(path.mat) == colnames(path.mat)')
  
  k <- k[c('idx.x', 'idx.y', 'distance')]
  k$idx.x <- as.character(k$idx.x); k$idx.y <- as.character(k$idx.y)
  k$distance <- as.numeric(k$distance)
  
  for (i in 1:nrow(k)) {
    path.mat[k$idx.x[i], k$idx.y[i]] <- k$distance[i]
  }
  
  diag(path.mat) <- 0
  
  saveRDS(path.mat, file.path(temp, 'path_matrix.rds'))
  
  # Generate shortest paths
  print("Generate shortest paths")
  assign('shortest.path', allShortestPaths(path.mat))
  
  # Save file
  saveRDS(shortest.path, file.path(temp, 'shortest_path.rds'))
  }

# Run program
FW_calculate(infile = 'nodes',
             node1 = c('long1', 'lat1'),
             node2 = c('long2', 'lat2'))