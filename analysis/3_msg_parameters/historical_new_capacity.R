###################################################
# Scale parameters based on trade volume (in GWa) #
# historical_new_capacity
# (only for export technologies)
###################################################
# Function: build historical_new_capacity parameter
build_historical_new_capacity <- function(energy) {
  
  print(paste0('Building parameter [historical_new_capacity]'))
  print(paste0('Technology = ', energy, '_exp'))
  
  environment(scale_exp_parameter) <- environment()
  
  assign('varlist', c('node_loc', 'technology', 'year_vtg', 'value', 'unit'))
  assign('parname', 'historical_new_capacity')
  assign('tra.energy', energy)
  
  # EXPORTS
  assign('msg.technology', paste0(energy, '_exp'))
  assign('exports', scale_exp_parameter(parname = parname, 
                                        msg.technology = msg.technology, 
                                        tra.energy = tra.energy, 
                                        varlist = varlist))
  
  
  # Subset to keep only non-missing value
  exports <- subset(exports, !is.na(value))
  
  saveRDS(exports, file.path(output, paste0('historical_new_capacity/', energy, '_exp.rds')))
  write.csv(exports, file.path(output, paste0('historical_new_capacity/', energy, '_exp.csv')))
  
}

# Run program
for (e in energy_list) {
  build_historical_new_capacity(e)
}

clean_up()