####################################
# Build parameters: var_cost #
####################################
# You must run 2_regress files before compiling parameter!
input_reg <- paste0(wd, "output/analysis/regress")

# Import regression file
paths <- readRDS(file.path(input_reg, 'var_cost_from_reg.rds'))
isid('paths', c('node_loc', 'technology', 'year'))

mean_var_cost <- group_by(paths, energy, node_loc, technology) %>%
                 summarise(mean_var_cost = mean(var_cost, na.rm = T))

# Set up in MESSAGEix format #
##############################
paths_msg <- expand.grid(year_act, unique(paste0('R14_', toupper(regions))), unique(paths$technology))
  names(paths_msg) <- c('year_act', 'node_loc', 'technology')
  paths_msg[, 2:3] <- lapply(paths_msg[, 2:3], function(x) as.character(x))
  paths_msg <- subset(paths_msg, tolower(substr(node_loc, 5, 7)) != 
                        substr(technology, nchar(technology) - 2, nchar(technology)))
  
paths_msg <- left_join(paths_msg, paths[c('node_loc', 'technology', 'year', 'var_cost')], 
                       by = c('node_loc', 'technology', 'year_act' = 'year'))

# Fill in with mean where missing (particularly future values)
paths_msg <- left_join(paths_msg, mean_var_cost, by = c('node_loc', 'technology'))
paths_msg$var_cost[is.na(paths_msg$var_cost)] <- paths_msg$mean_var_cost[is.na(paths_msg$var_cost)]

# For landlocked regions, only allow pipeline access to be normal price, otherwise very high
pipelines <- read.csv(file.path(wd, "raw/UserInputs/pipeline_connections.csv"), stringsAsFactors = F)
names(pipelines) <- c('node1', 'node2')
pipelines2 <- pipelines[c('node2', 'node1')]
  names(pipelines2) <- c('node1', 'node2')
pipelines <- unique(rbind(pipelines, pipelines2))
pipelines$partners <- paste0(pipelines$node1, " ", pipelines$node2)
pipelines$pipeline <- 1

paths_msg$partners <- paste0(paths_msg$node_loc, " R14_", 
                             toupper(substr(paths_msg$technology, nchar(paths_msg$technology) - 2, nchar(paths_msg$technology))))

paths_msg <- left_join(paths_msg, pipelines[c('partners', 'pipeline')], by = c('partners'))

paths_msg$var_cost[paths_msg$pipeline == 1] <- 0
paths_msg$var_cost[is.na(paths_msg$var_cost)] <- 2*max(paths_msg$var_cost, na.rm = T) # make it very high to ship to landlocked regions
paths_msg$pipeline <- paths_msg$partners <- NULL

# Truncate at zero
paths_msg$var_cost[paths_msg$var_cost < 0] <- 0
#paths_msg$var_cost <- paths_msg$var_cost/10

# Put in MESSAGE format
parout <- expand.grid(year_act, year_vtg)
names(parout) <- c('year_act', 'year_vtg')

parout <- inner_join(parout, paths_msg, by = c('year_act'))

parout$value <- parout$var_cost
parout$mode <- mode
parout$unit <- unit
parout$time <- time

parout <- parout[c('node_loc', 'technology', 'year_vtg',
                         'year_act', 'mode', 'time', 'value', 'unit')]

parout <- subset(parout, year_act - year_vtg <= tech_lifetime & year_act - year_vtg >=0)

# Save by technology
for (t in export_technologies) {
  print(paste0('Saving parameter [', t, ']'))
  assign('df', subset(parout, grepl(t, technology)))
  
  if (t == 'oil_exp') {assign('df', subset(parout, substr(technology, 1, 3) == 'oil'))} # so we don't include foil or loil
  
  saveRDS(df, file.path(output, paste0('var_cost/', t, '.rds')))
  write.csv(df, file.path(output, paste0('var_cost/', t, '.csv')))
}