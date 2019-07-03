###################################################
# Scale parameters based on trade volume (in GWa) #
# historical_activity
###################################################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')

input <-    paste0(wd, 'output/derived/')
output <-   paste0(wd, "output/analysis/msg_parameters/")

source(paste0(repo, 'analysis/3_msg_parameters/scale_msg_parameter/functions.R'))
source(paste0(repo, 'analysis/3_msg_parameters/build_parameters.R'))

# Import regionally aggregated trade data
trade.df <- read.csv(file.path(input, 'trade/regional_trade.csv'), stringsAsFactors = F)

# Energy commodities
energy_list <- c('oil', 'coal', 'loil', 'foil', 'LNG')

# Function: build historical_activity parameter
build_historical_activity <- function(energy) {
  
  environment(scale_exp_parameter) <- environment(expand_imp_parameter) <- environment()
  
  assign('varlist', c('node_loc', 'technology', 'year_act', 'mode', 'time', 'value', 'unit'))
  assign('parname', 'historical_activity')
  assign('tra.energy', energy)
  
  # EXPORTS
  assign('msg.technology', paste0(energy, '_exp'))
  assign('exports', scale_exp_parameter(parname = parname, 
                                        msg.technology = msg.technology, 
                                        tra.energy = tra.energy, 
                                        varlist = varlist))
  # IMPORTS
  assign('msg.technology', paste0(energy, '_imp'))
  assign('imports', expand_imp_parameter(parname = parname,
                                         msg.technology = msg.technology,
                                         tra.energy = tra.energy,
                                         varlist = varlist))
  
  # Subset to keep only non-missing value
  exports <- subset(exports, !is.na(value))
  imports <- subset(imports, !is.na(value))
  
  saveRDS(exports, file.path(output, paste0('historical_activity/', energy, '_exp.rds')))
  saveRDS(imports, file.path(output, paste0('historical_activity/', energy, '_imp.rds')))
}

# Run program
for (e in energy_list) {
  build_historical_activity(e)
}