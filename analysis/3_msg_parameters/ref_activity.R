###################################################
# Scale parameters based on trade volume (in GWa) #
# ref_activity
###################################################
# Function: build historical_activity parameter
build_ref_activity <- function(energy) {
  
  environment(scale_exp_parameter) <- environment(expand_imp_parameter) <- environment()
  
  assign('varlist', c('node_loc', 'technology', 'year_act', 'mode', 'time', 'value', 'unit'))
  assign('parname', 'ref_activity')
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
  
  # Subset to keep only non-missing value
  exports <- subset(exports, !is.na(value))
  imports <- subset(imports, !is.na(value) & year_act %in% year_act_base)
  
  saveRDS(exports, file.path(output, paste0('ref_activity/', energy, '_exp.rds')))
  saveRDS(imports, file.path(output, paste0('ref_activity/', energy, '_imp.rds')))
  write.csv(exports, file.path(output, paste0('ref_activity/', energy, '_exp.csv')))
  write.csv(imports, file.path(output, paste0('ref_activity/', energy, '_imp.csv')))
}

# Run program
for (e in energy_list) {
  build_ref_activity(e)
}

clean_up()