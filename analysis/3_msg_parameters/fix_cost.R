##############################
# Build parameters: fix_cost #
##############################
rm(list = ls())
wd <- 'H:/message_trade/analysis/3_msg_parameters'
setwd(wd)

output <- 'H:/data/output/analysis/msg_parameters/fix_cost/'

# Import functions
source('build_parameters.R')

# Import cost spreadsheet
costs <- read.csv(file.path(wd, "costs/costs_input.csv"), stringsAsFactors = F)
names(costs) <- c('node_loc', 'technology', 'fix_cost', 'inv_cost')

# Set import and export technologies
export_technologies <- c('oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp')
import_technologies <- c('oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp')
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Set columns (this can be dataframe if it varies by node/year)
parname <- 'fix_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'value', 'unit')
unit <- 'USD/GWa'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act


# Build parameter for each trade technology
for (t in c(export_technologies, import_technologies)) {
  
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
        
        assign('value', costs$fix_cost[costs$node_loc == node_loc.in & costs$technology == t])
        
        assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                         node_loc = node_loc.in,
                                         year_act = year_act, year_vtg = year_vtg,
                                         value = value, unit = unit))
        parout <- subset(parout, year_act - year_vtg <= 40)
        parout <- subset(parout, year_act >= 1990)
        
        parin <- rbind(parin, parout)
      }
    }
    
    # IMPORTS
    if (grepl('imp', t)) {
      
      assign('technology.in', t)
      
      assign('node_loc.in', paste0('R14_', toupper(r_to)))
      
      assign('value', costs$fix_cost[costs$node_loc == node_loc.in & costs$technology == t])
      
      assign('varlist', c('node_loc', 'technology', 'year_act', 'value', 'unit')) # re-assign without year_vtg
      
      assign('parout', build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                                       node_loc = node_loc.in,
                                       year_act = year_act,
                                       value = value, unit = unit))
      parout$year_vtg <- parout$year_act
      parin <- rbind(parin, parout)
    }
    
    
    parsave <- rbind(parsave, parin)
  }
  
  parsave <- unique(parsave)
  
  saveRDS(parsave, file.path(output, paste0(t, '.rds')))
}