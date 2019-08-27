######################################################
# Function to build "activity" parameters in MESSAGE #
######################################################

build_activity <- function(parname, lo_or_up, in.value, imports.too = TRUE, imports.only = FALSE) {
  
  assign('value', in.value)
  
  if (imports.only == TRUE) {tec_list <- c(import_technologies)}
  if (imports.only == FALSE) {tec_list <- c(export_technologies, import_technologies)}
  
  for (t in tec_list) {
    
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
          
          assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                           node_loc = node_loc.in,
                                           year_act = year_act,
                                           time = time, 
                                           value = value, unit = unit))
          parin <- rbind(parin, parout)
        }
      }
      
      # IMPORTS
      if (imports.too == TRUE) {
        if (grepl('imp', t)) {
          
          assign('technology.in', t)
          
          assign('node_loc.in', paste0('R14_', toupper(r_to)))
          
          assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                           node_loc = node_loc.in,
                                           year_act = year_act,
                                           time = time, 
                                           value = value, unit = unit))
          parin <- rbind(parin, parout)
        }
      }
        
        
        parsave <- rbind(parsave, parin)
      }
      
    parsave <- unique(parsave)
    
    saveRDS(parsave, file.path(output, paste0(parname, '/', t, '.rds')))
    write.csv(parsave, file.path(output, paste0(parname, '/', t, '.csv')))
    
  }
}