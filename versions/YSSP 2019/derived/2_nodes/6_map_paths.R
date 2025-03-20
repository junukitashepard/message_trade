######################
# Map shortest paths #
######################
# Import shortest path rds
spath <- readRDS(file.path(temp, 'shortest_path.rds'))

# Import path matrix
pmat <- readRDS(file.path(temp, 'path_matrix.rds'))

# Import node identifiers (idx.x)
k <- readRDS(file.path(temp, 'k.rds'))
k$idx.x <- as.character(k$idx.x)
k$idx.y <- as.character(k$idx.y)

k[, c('long1', 'lat1', 'long2', 'lat2')] <- 
  lapply(k[, c('long1', 'lat1', 'long2', 'lat2')], function(x) round(x, 1))

# Import node information
nodes <- readRDS(file.path(temp, 'node_combinations.rds'))
  n1 <- nodes[c('long1', 'lat1', 'type1', 'country1')]
  n2 <- nodes[c('long2', 'lat2', 'type2', 'country2')]
  names(n1) <- names(n2) <- c('long', 'lat', 'type', 'country')
  n.base <- unique(rbind(n1, n2))
  n.all <- subset(n.base, type == 'SEAPORT.NODE') # Keep sea ports
  n.all$lat <- round(n.all$lat, 1)
  n.all$long <- round(n.all$long, 1)

node.names <- readRDS(file.path(temp, 'node_names.rds'))

# Build port dataframe
ports <- unique(k[c('idx.x', 'long1', 'lat1')])
  ports <- inner_join(ports, n.all, by = c('long1' = 'long', 'lat1' = 'lat'))
  #ports <- subset(ports, type == 'SEAPORT.NODE')
  
df <- expand.grid(ports$idx.x, ports$idx.x)
df[] <- lapply(df[], function(x) as.character(x))
names(df) <- c('port1', 'port2')
df <- subset(df, port1 != port2)

# Function: fill in routes
fill_routes <- function() {
  
  assign('path4map', data.frame(port1 = numeric(), port2 = numeric(), node = numeric()), envir = parent.frame())
  
  for (i in 1:nrow(df)) {
    print(paste0("Insert ", i, " of ", nrow(df)))
    
    assign('sp', extractPath(spath, which(rownames(pmat) == df$port1[i]),
                                    which(rownames(pmat) == df$port2[i])))
    assign('sp.route', data.frame(port1 = numeric(length(sp)), 
                                  port2 = numeric(length(sp)),
                                  node = numeric(length(sp))))
  
    r <- 1
    for (n in 1:length(sp)) {
      sp.route$port1[r] <- df$port1[i]
      sp.route$port2[r] <-  df$port2[i]
      sp.route$node[r] <- rownames(pmat)[sp[n]]
      r <- r+1
    }
  
    assign('path4map', rbind(path4map, sp.route), envir = parent.frame())
  }
  path4map$node <- as.character(path4map$node)
  
  # Link to node names and information
  path4map <- left_join(path4map, ports, by = c('port1' = 'idx.x'))
  path4map <- left_join(path4map, ports, by = c('port2' = 'idx.x'))
  path4map <- left_join(path4map, unique(k[c('idx.x', 'long1', 'lat1')]), by = c('node' = 'idx.x'))

  names(path4map) <- c('port1', 'port2', 'node',
                       'port1.long', 'port1.lat', 'port1.type', 'port1.country',
                       'port2.long', 'port2.lat', 'port2.type', 'port2.country',
                       'node.long', 'node.lat')
  
  path4map <- dplyr::group_by(path4map, port1, port2) %>%
              dplyr::mutate(route_id = group_indices(),
                            step_id = row_number())
  
  assign('path4map', path4map, envir = parent.frame())
  write.csv(path4map, file.path(output, 'derived/nodes/shortest_paths.csv'))
  saveRDS(path4map, file.path(output, 'derived/nodes/shortest_paths.rds'))
}

# Run program
fill_routes()