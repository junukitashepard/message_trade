########################
# Build all parameters #
########################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')

input <-   paste0(wd, "output/derived/")
output <-   paste0(wd, "output/analysis/msg_parameters/")

# Import functions 
###################
source(paste0(repo, 'analysis/3_msg_parameters/scale_msg_parameter/functions.R'))
source(paste0(repo, 'analysis/3_msg_parameters/build_parameters.R'))
source(paste0(repo, 'analysis/3_msg_parameters/activity_parameters/build_activity.R'))

# Define parameters of interest and energy commodities
#######################################################
# List of parameters
parameter_list <- c('bound_activity_lo', 'bound_activity_up',
                    'capacity_factor', 'fix_cost',
                    'growth_activity_lo', 'growth_activity_up',
                    'historical_activity', 'historical_new_capacity',
                    'initial_activity_lo', 'initial_activity_up',
                    'input', 'inv_cost',
                    'level_cost_activity_soft_lo', 'level_cost_activity_soft_up',
                    'output', 'soft_activity_lo', 'soft_activity_up', 'technical_lifetime')

# List of energy commodities
energy_list <- c('oil', 'coal', 'loil', 'foil', 'LNG')

# List of technologies
export_technologies <- c('oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp')
import_technologies <- c('oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp')

# List of regions
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Import data files 
####################
# Import regionally aggregated trade data
trade.df <- read.csv(file.path(input, 'trade/regional_trade.csv'), stringsAsFactors = F)

# Import cost spreadsheet
costs <- read.csv(file.path(repo, "analysis/3_msg_parameters/costs/costs_input.csv"), stringsAsFactors = F)
names(costs) <- c('node_loc', 'technology', 'fix_cost', 'inv_cost')

# Import emission factor and lifetime
emit_lt <- read.csv(file.path(repo, "analysis/3_msg_parameters/emission_lifetime/emission_lifetime.csv"), stringsAsFactors = F)
names(emit_lt) <- c('node_loc', 'technology', 'emission_factor', 'lifetime')

# Build parameters!
####################
# bound_activity_lo, bound_activity_up
source(paste0(repo, 'analysis/3_msg_parameters/bound_activity.R'))

# capacity_factor
parname <- 'capacity_factor'
varlist <-  c('node_loc', 'technology', 'year_vtg', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
value <- 1
unit <- '%'
time <- 'year'
source(paste0(repo, 'analysis/3_msg_parameters/capacity_factor.R'))

# fix_cost
parname <- 'fix_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'value', 'unit')
unit <- 'USD/GWa'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
source(paste0(repo, 'analysis/3_msg_parameters/fix_cost.R'))

# growth_activity_lo, growth_activity_up
parname <- 'growth_activity'
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
unit <- '%'
time <- 'year'
value_lo = -0.05
value_up = 0.02
source(paste0(repo, 'analysis/3_msg_parameters/growth_activity.R'))

# historical_activity
source(paste0(repo, 'analysis/3_msg_parameters/historical_activity.R'))

# historical_new_capacity
source(paste0(repo, 'analysis/3_msg_parameters/historical_new_capacity.R'))

# initial_activity_lo, initial_activity_up
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1995, 2055, by = 5), seq(2060, 2110, by = 10))
unit <- 'GWa'
time <- 'year'
value_lo = 2
value_up = 2
source(paste0(repo, 'analysis/3_msg_parameters/initial_activity.R'))

# input
source(paste0(repo, 'analysis/3_msg_parameters/input.R'))

# inv_cost
parname <- 'inv_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
unit <- 'USD/GWa'
year_vtg <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/inv_cost.R'))

# level_cost_activity_soft_lo, level_cost_activity_soft_up
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(2020, 2055, by = 5), seq(2060, 2110, by = 10)) # future only
unit <- '???'
time <- 'year'
value_up <- 0.5
value_lo <- 0.5
source(paste0(repo, 'analysis/3_msg_parameters/level_cost_activity_soft.R'))

# output
source(paste0(repo, 'analysis/3_msg_parameters/output.R'))

# soft_activity
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(2020, 2055, by = 5), seq(2060, 2110, by = 10)) # future only
unit <- '???'
time <- 'year'
value_lo <- 0.05
value_up <- 0.05
source(paste0(repo, 'analysis/3_msg_parameters/soft_activity.R'))

# technical_lifetime
parname <- 'technical_lifetime'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
value <- 40
unit <- 'y'
year_vtg <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/technical_lifetime.R'))

# emission_factor
parname <- 'emission_factor'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'emission', 'value', 'unit')
mode <- 'M1'
unit <- 'kg/kWa'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
time <- 'year'
emission <- 'CO2'
source(paste0(repo, 'analysis/3_msg_parameters/emission_factor.R'))

