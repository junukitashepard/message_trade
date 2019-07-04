####################################################
# Build parameters: level_cost_activity_soft_lo/up #
####################################################
rm(list = ls())

setwd('H:/message_trade/analysis/3_msg_parameters')

output <- 'H:/data/output/analysis/msg_parameters/'

# Import functions
source('build_parameters.R')
source('activity_parameters/build_activity.R')

# Set import and export technologies
export_technologies <- c('oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp')
import_technologies <- c('oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp')
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Run programs: level_cost_soft_activity
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(2020, 2055, by = 5), seq(2060, 2110, by = 10)) # future only
unit <- '???'
time <- 'year'

build_activity('level_cost_activity_soft_lo', 'lo', 0.5, imports.too = FALSE)
build_activity('level_cost_activity_soft_up', 'up', 0.5, imports.too = FALSE)