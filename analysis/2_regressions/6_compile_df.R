#######################################################
# Compile data frame of variable cost impacts by node #
#######################################################
rm(list = ls())
wd.data <- "H:/data/"
wd <- 'H:/message_trade/analysis/2_regressions/'
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')
library('ggplot2')

raw <-      paste0(wd.data, "raw")
input <-    paste0(wd.data, "output/analysis/regress/")
output <-   paste0(wd.data, "output/analysis/regress/")
temp <-     paste0(wd.data, "temp/")

source(paste0(wd, '4_regress.R'))
#######################################################
# Import file
trade <- readRDS(file.path(input, "regdf.rds"))
paths <- read.csv(file.path(wd.data, 'output/derived/nodes/regional_paths.csv'), stringsAsFactors = F)[c('year', 'energy', 'msg_region1', 'msg_region2', 'port1', 'port2')]
ijports <- readRDS(file.path(wd.data, 'output/derived/nodes/ij_ports.rds'))[c('port1', 'port2', 'distance')]

region_list <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'RUS', 'SAS', 'WEU')
energy_list <- energy_list <- c('oil', 'coal', 'foil', 'LNG')

# Link port information to distance
paths <- unique(subset(paths, msg_region1 %in% region_list & msg_region2 %in% region_list))
paths <- group_by(paths, year, energy, msg_region1, msg_region2) %>% mutate(count = row_number())
paths <- subset(paths, count == 1) # drops 1 observation
paths$count <- NULL
isid('paths', c('year', 'energy', 'msg_region1', 'msg_region2'))

ijports$port1 <- as.numeric(ijports$port1)
ijports$port2 <- as.numeric(ijports$port2)

paths <- left_join(paths, ijports, by = c('port1', 'port2'))
paths$distance <- paths$distance/1000 # in 1000 km

# Get mean paths for when distance is missing
mean_paths <- group_by(paths, msg_region1, msg_region2) %>%
              summarise(mean_path = mean(distance, na.rm = T))
assert('!is.na(mean_paths$mean_path)')

# Function: compile dataframe for exporters
make_regdf <- function(varlist, subset_p = NULL) {
  assign('base_df', expand.grid(region_list, energy_list))
  names(base_df) <- c('node_loc', 'technology')
  base_df$node_loc <- paste0('R14_', base_df$node_loc)
  base_df$technology <- paste0(base_df$technology, '_exp')
  
  for (v in varlist) {
    print(paste0('##############################'))
    print(paste0('# Compiling for: ', v, ' #'))
    print(paste0('##############################'))
    
    var_df <- data.frame()
    
    for (r in c('all', region_list)) {
      print(paste0("Running regression for exporter = ", r))
      assign(paste0('X'), run_reg(variable = v, energy_list = energy_list, exporters = r))
      X <- as.data.frame(X)
      
      X <- X[2:nrow(X), c('Estimate', 'Std. Error', 'Pr(>|t|)')]
      names(X) <- c(paste0(v, '_eff'), paste0(v, '_se'), 'p')
      
      if (!is.null(subset_p)) {
        X <- subset(X, p < subset_p)
        if (nrow(X) == 0) {
          next()
        }}
      X$p <- NULL
      
      X$technology <- paste0(rownames(X), "_exp")
      X$node_loc <- paste0('R14_', r)
      var_df <- rbind(var_df, X)
    }
    
    base_df <- full_join(base_df, var_df, by = c('node_loc', 'technology'))
  }
  return(base_df)
}

# Run program
var_cost_df <- 
make_regdf(varlist = c('distance', 'sanction_imposition'))

# Visualize results
plot_hist <- function(df, variable, vartitle, unit) {
  
  assign('plot', ggplot(aes(x = get(variable)), data = get(df)))
  plot <- plot + 
          geom_histogram(colour = 'darkblue', fill = 'blue', alpha = 0.3) +
          labs(title = paste0('Effect of ', vartitle, ' on variable cost'),
               subtitle = paste0('Unit: ', unit),
               x = vartitle,
               y = 'count')
  return(plot)
}

plot_hist(df = 'var_cost_df', variable = 'distance_eff', vartitle = 'distance', unit = '$/GWa/1000km')
plot_hist(df = 'var_cost_df', variable = 'sanction_imposition_eff', vartitle = 'sanction imposition', unit = '$/GWa/sanction')

# Put paths onto regression results #
#####################################
paths$technology <- paste0(paths$energy, '_exp_', tolower(paths$msg_region2))
paths$technology.link <- paste0(paths$energy, '_exp')
paths$node_loc <- paste0('R14_', paths$msg_region1)

# Post-process: add mean distance where missing (note: not included in regression!)
paths <- left_join(paths, mean_paths, by = c('msg_region1', 'msg_region2'))
paths$distance[is.na(paths$distance)] <- paths$mean_path[is.na(paths$distance)]

paths <- paths[c('year', 'energy', 'node_loc', 'distance', 'technology', 'technology.link')]
assert('!is.na(paths$distance)')

# Combine paths wtih regression results
var_cost_df <- subset(var_cost_df, node_loc == 'R14_all')
paths <- left_join(paths, var_cost_df[c('technology', 'distance_eff')], 
                   by = c('technology.link' = 'technology'))

paths$var_cost <- paths$distance * paths$distance_eff

check <- subset(paths, is.na(var_cost))

# Plot
paths.plotdf <- subset(paths, var_cost != 0)
hist <- ggplot(aes(x = var_cost), data = paths.plotdf) + 
        geom_histogram(colour = 'darkorange', fill = 'orange', alpha = 0.4) +
        labs(title = "Variable cost estimated by distance only",
             subtitle = "Unit: $/GWa, Data ID: node_loc, technology, year",
             x = "Variable cost",
             y = 'Count') + 
        theme(text = element_text(size = 24))

# Save for MESSAGE parameterization
saveRDS(paths, file.path(output, 'var_cost_from_reg.rds'))




