###########################################################
# Build parameters: Growth and Initial activity lo and up #
############################################################
rm(list = ls())

setwd('H:/message_trade/analysis/3_msg_parameters')

output <- 'H:/data/output/analysis/msg_parameters/'

# Import functions
source('build_parameters.R')

# Set import and export technologies
export_technologies <- c('oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp')
import_technologies <- c('oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp')
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Build parameter for each trade technology
build_activity <- function(parname, lo_or_up, in.value) {
  
  assign('value', in.value)
  
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
          
          assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                           node_loc = node_loc.in,
                                           year_act = year_act,
                                           time = time, 
                                           value = value, unit = unit))
          parin <- rbind(parin, parout)
        }
      }
      
      # IMPORTS
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
      
      
      parsave <- rbind(parsave, parin)
    }
    
    parsave <- unique(parsave)
    
    saveRDS(parsave, file.path(output, paste0(parname, '/', t, '.rds')))
  }
}

# Run programs: growth_activity
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
unit <- '%'
time <- 'year'

build_activity('growth_activity_lo', 'lo', -0.05)
build_activity('growth_activity_up', 'up', 0.02)

# Run programs: initial_activity
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
unit <- 'GWa'
time <- 'year'

build_activity('initial_activity_lo', 'lo', 2)
build_activity('initial_activity_up', 'up', 2)
