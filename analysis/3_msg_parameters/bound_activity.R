###################################################
# Scale parameters based on trade volume (in GWa) #
# bound_activity_lo and bound_activity_up
###################################################
# Function: build bound_activity_'x' parameters
build_bound_activity <- function(lo_or_up, energy) {
  
  environment(scale_exp_parameter) <- environment(expand_imp_parameter) <- environment()
  
  print(paste0('Energy = ', energy))
  
  assign('varlist', c('node_loc', 'technology', 'year_act', 'mode', 'time', 'value', 'unit'))
  assign('parname', paste0('bound_activity_', lo_or_up))
  assign('tra.energy', energy)
  
  # EXPORTS
  assign('msg.technology', paste0(energy, '_exp'))
  assign('exports', scale_exp_parameter(parname = parname, 
                                        msg.technology = msg.technology, 
                                        tra.energy = tra.energy, 
                                        varlist = varlist))
  
  # Set future bounds on activity
  exp_tec <- paste0(msg.technology, '_', regions)
  
  df <- expand.grid(exp_tec, paste0('R14_', toupper(regions)))
  names(df) <- c('technology', 'node_loc')
  future_activity <- data.frame()
  for (y in year_act_base) {
    indf <- df
    indf$year_act <- y
    indf$mode <- 'M1'
    indf$time <- 'year'
    indf$unit <- 'GWa'
    future_activity <- rbind(future_activity, indf)
  }
  
  future_activity <- subset(future_activity, year_act > 2015)
  future_activity$value <- 2000
  
  # IMPORTS
  assign('msg.technology', paste0(energy, '_imp'))
  assign('imports', expand_imp_parameter(parname = parname,
                                         msg.technology = msg.technology,
                                         tra.energy = tra.energy,
                                         varlist = varlist))
  
  
  # CHECK LATER: Arbitrary number to add to upper bound
  if (lo_or_up == 'up') {
    exports <- full_join(exports, future_activity, by = c('node_loc', 'year_act', 'technology', 'mode', 'time', 'unit'))
      exports$value <- exports$value.x
      exports$value[is.na(exports$value)] <- exports$value.y[is.na(exports$value)]
    imports <- full_join(imports, future_activity, by = c('node_loc', 'year_act', 'technology', 'mode', 'time', 'unit'))
      imports$value <- imports$value.x
      imports$value[is.na(imports$value)] <- imports$value.y[is.na(imports$value)]
    
    exports <- exports[c('technology', 'mode', 'time', 'unit', 'year_act', 'node_loc', 'value')]
    imports <- imports[c('technology', 'mode', 'time', 'unit', 'year_act', 'node_loc', 'value')]
    
  } else {
    exports$value <- 0
    imports$value <- 0
  }
  
  exports <- subset(exports, !is.na(value))
  imports <- subset(imports, !is.na(value) & year_act %in% year_act_base)
  
  saveRDS(exports, file.path(output, paste0('bound_activity_', lo_or_up, '/', energy, '_exp.rds')))
  saveRDS(imports, file.path(output, paste0('bound_activity_', lo_or_up, '/', energy, '_imp.rds')))
  
  write.csv(exports, file.path(output, paste0('bound_activity_', lo_or_up, '/', energy, '_exp.csv')))
  write.csv(imports, file.path(output, paste0('bound_activity_', lo_or_up, '/', energy, '_imp.csv')))
}

# Run program
for (e in energy_list) {
  print(paste0('## Building parameters for = ', e, ' ##'))
  
  build_bound_activity(lo_or_up = 'lo', energy = e)
  build_bound_activity(lo_or_up = 'up', energy = e)
}
