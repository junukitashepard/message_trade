####################################
# Build parameters: var_cost #
####################################
# You must run 2_regress files before compiling parameter!
input_reg <- paste0(wd, "output/analysis/regress")

# Import regression file
paths <- readRDS(file.path(input_reg, 'var_cost_from_reg.rds'))
isid('paths', c('node_loc', 'technology', 'year'))

# Set up in MESSAGEix format #
##############################
all_technologies <- expand.grid(export_technologies, region.list)
all_technologies$technology <- paste0(all_technologies$Var1, "_", all_technologies$Var2)
all_technologies <- all_technologies$technology

paths_msg <- expand.grid(year_act, unique(paste0(region.number, '_', toupper(region.list))), unique(all_technologies))
  names(paths_msg) <- c('year_act', 'node_loc', 'technology')
  paths_msg[, 2:3] <- lapply(paths_msg[, 2:3], function(x) as.character(x))
  paths_msg <- subset(paths_msg, tolower(substr(node_loc, 5, 7)) != 
                        substr(technology, nchar(technology) - 2, nchar(technology)))
  
paths_msg <- left_join(paths_msg, paths[c('node_loc', 'technology', 'year', 'var_cost')], 
                       by = c('node_loc', 'technology', 'year_act' = 'year'))

# Make energy types (e.g. loil) follow foil variable costs
foil_costs <- data.frame()
for (e in energy.types.trade.foil) {
  fcdf <- subset(paths_msg, grepl('foil_exp', technology))
  fcdf$technology <- stringr::str_replace(fcdf$technology, 'foil_', paste0(e, '_'))
  fcdf$foil_cost <- fcdf$var_cost
  fcdf$var_cost <- fcdf$mean_var_cost <- NULL
  foil_costs <- rbind(as.data.frame(foil_costs), as.data.frame(fcdf))
}
if (nrow(foil_costs) > 0) {
  paths_msg <- left_join(paths_msg, foil_costs, by = c('year_act', 'node_loc', 'technology'))
  paths_msg$var_cost[is.na(paths_msg$var_cost) & !is.na(paths_msg$foil_cost)] <- paths_msg$foil_cost[is.na(paths_msg$var_cost) & !is.na(paths_msg$foil_cost)]
  paths_msg$foil_cost <- NULL
}

# Make energy types (e.g. hydrogen) follow LNG variable costs
LNG_costs <- data.frame()
for (e in energy.types.trade.LNG) {
  lndf <- subset(paths_msg, grepl('LNG_exp', technology))
  lndf$technology <- stringr::str_replace(lndf$technology, 'LNG_', paste0(e, '_'))
  lndf$LNG_cost <- lndf$var_cost
  lndf$var_cost <- lndf$mean_var_cost <- NULL
  LNG_costs <- rbind(as.data.frame(LNG_costs), as.data.frame(lndf))
}
if (nrow(LNG_costs) > 0) {
  paths_msg <- left_join(paths_msg, LNG_costs, by = c('year_act', 'node_loc', 'technology'))
  paths_msg$var_cost[is.na(paths_msg$var_cost) & !is.na(paths_msg$LNG_cost)] <- paths_msg$LNG_cost[is.na(paths_msg$var_cost) & !is.na(paths_msg$LNG_cost)]
  paths_msg$LNG_cost <- NULL
}

# For landlocked regions, only allow land access to be normal price, otherwise very high
landacc <- read.csv(file.path(wd, "raw/UserInputs/pipeline_connections.csv"), stringsAsFactors = F)
names(landacc) <- c('node1', 'node2')
landacc2 <- landacc[c('node2', 'node1')]
  names(landacc2) <- c('node1', 'node2')
landacc <- unique(rbind(landacc, landacc2))
landacc$partners <- paste0(landacc$node1, " ", landacc$node2)
landacc$landacc <- 1

paths_msg$partners <- paste0(paths_msg$node_loc, " ", region.number, "_", 
                             toupper(substr(paths_msg$technology, nchar(paths_msg$technology) - 2, 
                                            nchar(paths_msg$technology))))

paths_msg <- left_join(paths_msg, landacc[c('partners', 'landacc')], by = c('partners'))

paths_msg$var_cost[paths_msg$landacc == 1] <- mean(paths_msg$var_cost[paths_msg$var_cost > 0], na.rm  = T)

# Fill in with mean where missing (particularly future values)
mean_var_cost <- dplyr::group_by(paths_msg, node_loc, technology) %>%
                 dplyr::summarise(mean_var_cost = mean(var_cost, na.rm = T))
mean_var_cost$mean_var_cost[is.nan(mean_var_cost$mean_var_cost)] <- NA
paths_msg$var_cost[is.nan(paths_msg$var_cost)] <- NA

paths_msg <- left_join(paths_msg, mean_var_cost, by = c('node_loc', 'technology'))
paths_msg$var_cost[is.na(paths_msg$var_cost)] <- paths_msg$mean_var_cost[is.na(paths_msg$var_cost)]

paths_msg$var_cost[is.na(paths_msg$var_cost)] <- 2*max(paths_msg$var_cost, na.rm = T) # make it very high to ship to landlocked regions
paths_msg$partners <- NULL

# Truncate at zero
paths_msg$var_cost[paths_msg$var_cost < 0] <- 0

# Put in MESSAGE format #
#########################
parout <- paths_msg
parout$year_vtg <- parout$year_act

parout$value <- parout$var_cost

parout$mode <- mode
parout$unit <- unit
parout$time <- time

parout <- parout[c('node_loc', 'technology', 'year_vtg',
                         'year_act', 'mode', 'time', 'value', 'unit')]

# Save across technologies
saveRDS(parout, file.path(output, 'analysis/msg_parameters/var_cost/var_cost_base.rds'))

# Save by technology
for (t in export_technologies) {
  print(paste0('Saving parameter [', t, ']'))
  assign('df', subset(parout, grepl(t, technology)))
  
  if (t == 'oil_exp') {assign('df', subset(parout, substr(technology, 1, 3) == 'oil'))} # so we don't include foil or loil
  
  saveRDS(df, file.path(output, paste0('analysis/msg_parameters/var_cost/', t, '.rds')))
  write.csv(df, file.path(output, paste0('analysis/msg_parameters/var_cost/', t, '.csv')))
  write.csv(df, file.path(output, paste0('analysis/msg_parameters/SCENARIOS/baseline_no_tariff/var_cost/', t, '.csv')))
  
}