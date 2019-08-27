#####################################
# Build parameters: emission_factor #
#####################################
# Build parameter for each trade technology
for (t in c(import_technologies)) {
  
  # Set up output file       
  print(paste0('TRADE TECH. = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in regions) {
    
    print(paste0('DEST. = ', r_to))
    
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
      
    assign('technology.in', t)
      
    assign('node_loc.in', paste0('R14_', toupper(r_to)))
    assign('varlist', c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')) # re-assign without year_vtg
      
    assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                       node_loc = node_loc.in,
                                       year_act = year_act,
                                       time = time, mode = mode,
                                       value = value, unit = unit))
    parout$year_vtg <- parout$year_act
    parsave <- rbind(parsave, parout)
  }
  
  parsave <- unique(parsave)
  
  saveRDS(parsave, file.path(output, paste0('var_cost/', t, '.rds')))
  write.csv(parsave, file.path(output, paste0('var_cost/', t, '.csv')))
  write.csv(parsave, file.path(output, paste0('SCENARIOS/baseline_no_tariff/var_cost/', t, '.csv')))
}

# Clean up
clean_up()
