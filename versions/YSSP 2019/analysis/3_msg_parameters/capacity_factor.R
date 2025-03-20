#####################################
# Build parameters: capacity_factor #
#####################################
# Build parameter for each trade technology
for (t in c(export_technologies, import_technologies)) {
  
  # Set up output file       
  print(paste0('TRADE TECH. = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in region.list) {
    
    print(paste0('DEST. = ', r_to))
    
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
    # EXPORTS
    if (grepl('exp', t)) {
      for (r_from in region.list[region.list != r_to]) {
        
        print(paste0('FROM = ', r_from))
        
        assign('technology.in', paste0(t, '_', r_to))
        
        assign('node_loc.in', paste0(region.number, '_', toupper(r_from)))
        
        assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                         node_loc = node_loc.in,
                                         year_act = year_act, year_vtg = year_vtg,
                                         time = time, 
                                         value = value, unit = unit))
        # parout <- subset(parout, year_act - year_vtg < tech_lifetime & year_act - year_vtg >=0)
        # parout <- subset(parout, year_act >= 1990)
        parout$year_vtg <- parout$year_act
        parout <- unique(parout)
        parout <- subset(parout, year_act >= 1990)
        parin <- rbind(parin, parout)
      }
    }
    
    # IMPORTS
    if (grepl('imp', t)) {
      
      assign('technology.in', t)
      
      assign('node_loc.in', paste0(region.number, '_', toupper(r_to)))
      assign('varlist', c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')) # re-assign without year_vtg
      
      assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                       node_loc = node_loc.in,
                                       year_act = year_act,
                                       time = time, 
                                       value = value, unit = unit))
      parout$year_vtg <- parout$year_act
      parout <- unique(parout)
      parout <- subset(parout, year_act >= 1990)
      parin <- rbind(parin, parout)
    }
    
    
    parsave <- rbind(parsave, parin)
  }
  
  parsave <- unique(parsave)
  
  saveRDS(parsave, file.path(output, paste0('analysis/msg_parameters/capacity_factor/', t, '.rds')))
  write.csv(parsave, file.path(output, paste0('analysis/msg_parameters/capacity_factor/', t, '.csv')))
}

# Clean up
clean_up()
