###################################################
# Scale parameters based on trade volume (in GWa) #
# Initial activity lo and up
###################################################
# Function: build bound_activity_'x' parameters
build_initial_activity <- function(lo_or_up, energy) {
  
  environment(scale_exp_parameter) <- environment(expand_imp_parameter) <- environment()
  
  print(paste0('Energy = ', energy))
  
  assign('varlist', c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit'))
  assign('parname', paste0('initial_activity_', lo_or_up))
  assign('tra.energy', energy)
  
  # EXPORTS
  assign('msg.technology', paste0(energy, '_exp'))
  assign('exports', scale_exp_parameter(parname = parname, 
                                        msg.technology = msg.technology, 
                                        tra.energy = tra.energy, 
                                        varlist = varlist))
  exports$mode <- 'M1'
  
  # Set future bounds on activity
  exp_tec <- paste0(msg.technology, '_', region.list)
  
  df <- expand.grid(exp_tec, paste0(region.number, '_', toupper(region.list)))
  names(df) <- c('technology', 'node_loc')
  future_activity <- data.frame()
  for (y in year_act) {
    indf <- df
    indf$year_act <- y
    indf$time <- 'year'
    indf$unit <- 'GWa'
    future_activity <- rbind(future_activity, indf)
  }

  exports <- subset(exports, year_act < 2020)
  
  future_activity <- subset(future_activity, year_act > 2015)
  future_activity <- subset(future_activity, substr(node_loc, 5, 7) != toupper(gsub(paste0(msg.technology, '_'), '', technology)))
  
  link_to_future <- subset(exports, year_act == 2015)
  link_to_future <- link_to_future[c('node_loc', 'technology', 'value')]

  future_activity <- suppressWarnings(left_join(future_activity, link_to_future, by = c('node_loc', 'technology')))
  future_activity$value[is.na(future_activity$value)] <- 0
  
  exports <- full_join(exports, future_activity, by = c('node_loc', 'year_act', 'technology', 'time', 'unit'))
  exports$value <- exports$value.x
  exports$value[is.na(exports$value)] <- exports$value.y[is.na(exports$value)]
  exports <- exports[varlist]
  
  exports <- subset(exports, !is.na(value))
  
  exports.total <- group_by(exports, node_loc, year_act, time, unit) %>% summarize(value = sum(value))
  exports.total$technology <- msg.technology
  exports.total <- exports.total[names(exports)]
  
  #exports <- rbind(as.data.frame(exports), as.data.frame(exports.total))
  
  saveRDS(exports, file.path(output, paste0('analysis/msg_parameters/initial_activity_', lo_or_up, '/', energy, '_exp.rds')))
  write.csv(exports, file.path(output, paste0('analysis/msg_parameters/initial_activity_', lo_or_up, '/', energy, '_exp.csv')))
}

# Run programs
# Exports
for (e in energy.types) {
  build_initial_activity('lo', e)
  build_initial_activity('up', e)
}

# Imports
for (l in c('lo', 'up')) {
  assign('value.in', get(paste0('value_', l)))
  build_activity(paste0('initial_activity_', l), l, value.in, imports.only = TRUE)
}