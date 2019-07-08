###################################################
# Scale parameters based on trade volume (in GWa) #
# bound_activity_lo and bound_activity_up
###################################################
# Function: build bound_activity_'x' parameters
build_bound_activity <- function(lo_or_up, energy) {
  
  environment(scale_exp_parameter) <- environment(expand_imp_parameter) <- environment()
  
  assign('varlist', c('node_loc', 'technology', 'year_act', 'mode', 'time', 'value', 'unit'))
  assign('parname', paste0('bound_activity_', lo_or_up))
  assign('tra.energy', energy)
  
  # EXPORTS
  assign('msg.technology', paste0(energy, '_exp'))
  assign('exports', scale_exp_parameter(parname = parname, 
                                        msg.technology = msg.technology, 
                                        tra.energy = tra.energy, 
                                        varlist = varlist))
  # IMPORTS
  assign('msg.technology', paste0(energy, '_imp'))
  assign('imports', expand_imp_parameter(parname = parname,
                                         msg.technology = msg.technology,
                                         tra.energy = tra.energy,
                                         varlist = varlist))
  
  # CHECK LATER: Arbitrary number to add to upper bound
  if (lo_or_up == 'up') {
    exports$value <- exports$value + 5
    imports$value <- imports$value + 5
  }
  
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
