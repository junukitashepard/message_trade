####################################
# Build parameters: input #
####################################
# Build parameter for each trade technology
for (t in c(export_technologies, import_technologies)) {
  
  # Import trade technology information
  source(paste0(repo, 'analysis/3_msg_parameters/structure/input/input_', t, '.R'))
  
  # Set up output file       
  print(paste0('TRADE TECH. = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in regions) {
    
    print(paste0('DEST. = ', r_to))
    
    # EXPORTS
    if (grepl('exp', t)) {
      assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
      assign('technology.in', paste0(t, '_', r_to))
      
      for (r_from in regions[regions != r_to]) {
      
        print(paste0('FROM = ', r_from))
        
        assign('node_loc.in', paste0('R14_', toupper(r_from)))
        assign('node_origin.in', paste0('R14_', toupper(r_from)))
  
        assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                node_loc = node_loc.in, node_origin = node_origin.in,
                                year_act = year_act, year_vtg = year_vtg,
                                mode = mode, time = time, time_origin = time_origin,
                                commodity = commodity, level = level,
                                value = value, unit = unit))
        parout <- subset(parout, year_act - year_vtg <= 40)
        parout <- subset(parout, year_act >= 1990)
        
        parin <- rbind(parin, parout)
      }
    }
    
    # IMPORTS
    if (grepl('imp', t)) {
      assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
      
      assign('technology.in', t)
        
      assign('node_loc.in', paste0('R14_', toupper(r_to)))
      assign('node_origin.in', node_origin)
      assign('commodity.in', paste0(commodity, '_', r_to))
      
      assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                        node_loc = node_loc.in, node_origin = node_origin.in,
                                        year_act = year_act,
                                        mode = mode, time = time, time_origin = time_origin,
                                        commodity = commodity.in, level = level,
                                        value = value, unit = unit))
      parout <- unique(parout)
      parout$year_vtg <- parout$year_act
        
        parin <- rbind(parin, parout)
    }
   parsave <- rbind(parsave, parin)
  }
  saveRDS(parsave, file.path(output, paste0('input/', t, '.rds')))
  write.csv(parsave, file.path(output, paste0('input/', t, '.csv')))
}

clean_up()