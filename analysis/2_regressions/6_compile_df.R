#######################################################
# Compile data frame of variable cost impacts by node #
#######################################################
# Import file
trade <- readRDS(file.path(input, "analysis/regress/regdf.rds"))
paths <- read.csv(file.path(input, 'derived/nodes/regional_paths.csv'), stringsAsFactors = F)[c('year', 'energy', 'msg_region1', 'msg_region2', 'port1', 'port2')]
ijports <- readRDS(file.path(input, 'derived/nodes/ij_ports.rds'))[c('port1', 'port2', 'distance')]

# Link port information to distance
paths <- unique(subset(paths, msg_region1 %in% region.list.trade & msg_region2 %in% region.list.trade))
paths <- dplyr::group_by(paths, year, energy, msg_region1, msg_region2) %>% 
         dplyr::mutate(count = row_number())
paths <- subset(paths, count == 1) # drops 1 observation
paths$count <- NULL
isid('paths', c('year', 'energy', 'msg_region1', 'msg_region2'))

ijports$port1 <- as.numeric(ijports$port1)
ijports$port2 <- as.numeric(ijports$port2)

paths <- left_join(paths, ijports, by = c('port1', 'port2'))
paths$distance <- paths$distance/1000 # in 1000 km

# Get mean paths for when distance is missing
mean_paths <- dplyr::group_by(paths, msg_region1, msg_region2) %>%
              dplyr::summarise(mean_path = mean(distance, na.rm = T))
assert('!is.na(mean_paths$mean_path)')

# Function: compile dataframe for exporters
make_regdf <- function(varlist, subset_p = NULL) {
  assign('base_df', expand.grid(region.list.trade, energy.types.BACI))
  names(base_df) <- c('node_loc', 'technology')
  base_df$node_loc <- paste0(region.number, '_', base_df$node_loc)
  base_df$technology <- paste0(base_df$technology, '_exp')
  
  for (v in varlist) {
    print(paste0('##############################'))
    print(paste0('# Compiling for: ', v, ' #'))
    print(paste0('##############################'))
    
    var_df <- data.frame()
    
    for (r in c('all', region.list.trade)) {
      print(paste0("Running regression for exporter = ", r))
      assign(paste0('X'), run_reg(variable.rr = v, energy_list.rr = energy.types.BACI, exporters = r))
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
      X$node_loc <- paste0(region.number, '_', r)
      var_df <- rbind(var_df, X)
    }
    
    base_df <- full_join(base_df, var_df, by = c('node_loc', 'technology'))
  }
  return(base_df)
}

# Run program
var_cost_df <- make_regdf(varlist = c('distance', 'agree_fta', 'agree_pta', 'sanction_imposition', 'sanction_threat'))

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

plot_hist(df = 'var_cost_df', variable = 'distance_eff', vartitle = 'distance', unit = '$M/GWa/1000km')
# plot_hist(df = 'var_cost_df', variable = 'agree_fta_eff', vartitle = 'Free Trade Agreements', unit = '$M/GWa')
# plot_hist(df = 'var_cost_df', variable = 'agree_pta_eff', vartitle = 'Free Trade Agreements', unit = '$M/GWa')
# plot_hist(df = 'var_cost_df', variable = 'sanction_imposition_eff', vartitle = 'sanction', unit = '$M/GWa')

# Put paths onto regression results #
#####################################
paths$technology <- paste0(paths$energy, '_exp_', tolower(paths$msg_region2))
paths$technology.link <- paste0(paths$energy, '_exp')
paths$node_loc <- paste0(region.number, '_', paths$msg_region1)

# Post-process: add mean distance where missing (note: not included in regression!)
paths <- left_join(paths, mean_paths, by = c('msg_region1', 'msg_region2'))
paths$distance[is.na(paths$distance)] <- paths$mean_path[is.na(paths$distance)]

paths <- paths[c('year', 'energy', 'node_loc', 'distance', 'technology', 'technology.link')]
assert('!is.na(paths$distance)')

# Save regression results for scenario creation #
#################################################
scenario_input <- var_cost_df
scenario_input$distance_eff <- scenario_input$distance_se <- NULL
saveRDS(scenario_input, file.path(output, 'analysis/regress/scenario_effect.rds'))

# Combine paths wtih regression results #
#########################################
var_cost_df <- subset(var_cost_df, node_loc == paste0(region.number, '_all'))
paths <- left_join(paths, var_cost_df[c('technology', 'distance_eff')], 
                   by = c('technology.link' = 'technology'))

paths$var_cost <- paths$distance * paths$distance_eff

check <- subset(paths, is.na(var_cost))

# Plot
paths <- subset(paths, var_cost != 0)
hist <- ggplot(aes(x = var_cost), data = paths) + 
        geom_histogram(colour = 'darkorange', fill = 'orange', alpha = 0.4) +
        labs(title = "Variable cost estimated by distance only",
             subtitle = "Unit: $m/GWa, Data ID: node_loc, technology, year",
             x = "Variable cost",
             y = 'Count') + 
        theme(text = element_text(size = 24))

# Save for MESSAGE parameterization
saveRDS(paths, file.path(output, 'analysis/regress/var_cost_from_reg.rds'))



