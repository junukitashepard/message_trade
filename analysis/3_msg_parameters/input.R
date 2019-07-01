####################################
# Build parameters: input #
####################################
rm(list = ls())

setwd('H:/message_trade/analysis/3_msg_parameters')

output <- 'H:/data/output/analysis/msg_parameters/input/'

# Import functions
source('1_functions.R')

# Set import and export technologies
export_technologies <- c('oil_exp', 'coal_exp')
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Build parameter for each trade technology
for (t in export_technologies) {
  
  # Import trade technology information
  source(paste0('structure/input_', t, '.R'))
  
  # Set up output file       
  print(paste0('TRADE TECH. = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in regions) {
    
    print(paste0('DEST. = ', r_to))
    
    assign('technology.in', paste0(t, '_', r_to))
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
    for (r_from in regions[regions != r_to]) {
      
      print(paste0('FROM = ', r_from))
      
      assign('node_loc.in', paste0('R14_', toupper(r_from)))
      assign('node_origin.in', node_loc.in)
  
      assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                              node_loc = node_loc.in, node_origin = node_origin.in,
                              year_act = year_act, year_vtg = year_vtg,
                              mode = mode, time = time, time_origin = time_origin,
                              commodity = commodity, level = level,
                              value = value, unit = unit))
      parin <- rbind(parin, parout)
    }
  
  parsave <- rbind(parsave, parin)
  }
  
  saveRDS(parsave, file.path(output, paste0(t, '.rds')))
}
