########################################
# Build parameters: technical lifetime #
# (only for exports)
########################################
rm(list = ls())
wd <- 'H:/message_trade/analysis/3_msg_parameters'
setwd(wd)

output <- 'H:/data/output/analysis/msg_parameters/technical_lifetime/'

# Import functions
source('build_parameters.R')

# Set import and export technologies
export_technologies <- c('oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp')
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Set columns (this can be dataframe if it varies by node/year)
parname <- 'technical_lifetime'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
value <- 40
unit <- 'y'
year_vtg <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))

# Build parameter for each trade technology
for (t in export_technologies) {
  
  # Set up output file       
  print(paste0('Trade technology = ', t))
  
  assign('parsave', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
  
  for (r_to in regions) {
    
    assign('parin', as.data.frame(matrix(ncol = length(varlist), nrow = 0)))
    
    # EXPORTS
    if (grepl('exp', t)) {
      for (r_from in regions[regions != r_to]) {
        
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
  
  saveRDS(parsave, file.path(output, paste0(t, '.rds')))
}