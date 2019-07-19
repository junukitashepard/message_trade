########################################
# Build parameters: technical lifetime #
# (only for exports)
########################################
# Build parameter for each trade technology
for (t in export_technologies) {
  
  # Set up output file       
  print(paste0('Trade technology = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in regions) {
    
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
    # EXPORTS
    if (grepl('exp', t)) {
      for (r_from in regions) {
        
        assign('technology.in', paste0(t, '_', r_to))
        
        assign('node_loc.in', paste0('R14_', toupper(r_from)))
        
        assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                         node_loc = node_loc.in,
                                         year_vtg = year_vtg,
                                         value = value, unit = unit))
        
        parin <- rbind(parin, parout)
      }
    }
    parsave <- rbind(parsave, parin)
  }
  
  parsave <- unique(parsave)
  
  saveRDS(parsave, file.path(output, paste0('technical_lifetime/', t, '.rds')))
  write.csv(parsave, file.path(output, paste0('technical_lifetime/', t, '.csv')))
}

clean_up()