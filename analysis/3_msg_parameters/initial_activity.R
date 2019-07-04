#################################################################
# Build parameters: initial_activity_lo and initial_activity_up #
#################################################################
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

# Run programs: initial_activity
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1995, 2055, by = 5), seq(2060, 2110, by = 10))
unit <- 'GWa'
time <- 'year'

build_activity('initial_activity_lo', 'lo', 2)
build_activity('initial_activity_up', 'up', 2)