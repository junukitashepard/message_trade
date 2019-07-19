#####################################
# Build parameters: emission_factor #
#####################################
# Build parameter for each trade technology
for (t in c(export_technologies, import_technologies)) {
  
  # Set up output file       
  print(paste0('TRADE TECH. = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in regions) {
    
    print(paste0('DEST. = ', r_to))
    
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
    # EXPORTS
    if (grepl('exp', t)) {
      for (r_from in regions[regions != r_to]) {
        
        print(paste0('FROM = ', r_from))
        
        assign('technology.in', paste0(t, '_', r_to))
        
        assign('node_loc.in', paste0('R14_', toupper(r_from)))
        
        assign('value', emit_lt$emission_factor[emit_lt$node_loc == node_loc.in & emit_lt$technology == t])
        
        assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                         node_loc = node_loc.in,
                                         year_act = year_act, year_vtg = year_vtg,
                                         time = time, mode = mode, emission = emission, 
                                         value = value, unit = unit))
        parout <- subset(parout, year_act - year_vtg <= 40 & year_act - year_vtg >=0)
        parout <- subset(parout, year_act >= 1990)
        
        parin <- rbind(parin, parout)
      }
    }
    
    # IMPORTS
    if (grepl('imp', t)) {
      
      assign('technology.in', t)
      
      assign('node_loc.in', paste0('R14_', toupper(r_to)))
      assign('varlist', c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')) # re-assign without year_vtg
      
      assign('value', emit_lt$emission_factor[emit_lt$node_loc == node_loc.in & 
                                                emit_lt$technology == gsub('imp', 'exp', t)])
      
      value <- value * -1 # must be exactly negative of corresponding export technology
      
      assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                       node_loc = node_loc.in,
                                       year_act = year_act,
                                       time = time, mode = mode, emission = emission,
                                       value = value, unit = unit))
      parout$year_vtg <- parout$year_act
      parin <- rbind(parin, parout)
    }
    
    
    parsave <- rbind(parsave, parin)
  }
  
  parsave <- unique(parsave)
  
  saveRDS(parsave, file.path(output, paste0('emission_factor/', t, '.rds')))
  write.csv(parsave, file.path(output, paste0('emission_factor/', t, '.csv')))
}

# Clean up
clean_up()

check <- readRDS(file.path(output, 'emission_factor/oil_exp.rds'))