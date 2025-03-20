##############################
# Build parameters: inv_cost #
# (only for exports)
##############################
# Build parameter for each trade technology
for (t in export_technologies) {
  
  # Set up output file       
  print(paste0('Trade technology = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in region.list) {
    
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
    # EXPORTS
    if (grepl('exp', t)) {
      for (r_from in region.list[region.list != r_to]) {
        
        assign('technology.in', paste0(t, '_', r_to))
        
        assign('node_loc.in', paste0(region.number, '_', toupper(r_from)))
        
        assign('value', costs$inv_cost[costs$node_loc == node_loc.in & costs$technology == t])
        
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
  
  saveRDS(parsave, file.path(output, paste0('analysis/msg_parameters/inv_cost/', t, '.rds')))
  write.csv(parsave, file.path(output, paste0('analysis/msg_parameters/inv_cost/', t, '.csv')))
  
}

clean_up()
