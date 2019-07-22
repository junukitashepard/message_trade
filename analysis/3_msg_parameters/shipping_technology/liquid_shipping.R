############################################
# Build parameters for shipping technology #
############################################
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

#sink(paste0(repo, 'analysis/3_msg_parameters/make_parameters.txt'))

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

# Technical lifetime
tech_lifetime = 20

# List of energy commodities
liquid_list <- c('oil', 'loil', 'foil', 'LNG')
solid_list <- c('coal')

# List of regions
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Import data files 
####################
# Import regionally aggregated trade data
trade.df <- read.csv(file.path(input, 'trade/regional_trade.csv'), stringsAsFactors = F)

# Function to fix vintage years
fix_vintage <- function(param) {
  
  assign('df', param)
  
  df$diff <- df$year_act - df$year_vtg
  df <- subset(df, diff > 0 & diff < tech_lifetime)
  df$diff <- NULL
  return(df)
}

# Build parameters!
####################
# capacity_factor
parname <- 'capacity_factor'
varlist <-  c('node_loc', 'technology', 'year_vtg', 'year_act', 'time', 'value', 'unit')
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
value <- 1
unit <- '%'
time <- 'year'

capacity_factor <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = paste0('R14_', toupper(regions)),
                  year_act = year_act, year_vtg = year_vtg,
                  time = time, 
                  value = value, unit = unit) %>%
  fix_vintage()

# fix_cost
parname <- 'fix_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'value', 'unit')
unit <- 'USD/ton-km-y'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
value <- 0
  
fix_cost <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = paste0('R14_', toupper(regions)),
                  year_act = year_act, year_vtg = year_vtg,
                  value = value, unit = unit) %>%
  fix_vintage()

# inv_cost
parname <- 'inv_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
unit <- 'USD/ton-km-y'
year_vtg <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
value <- 10

inv_cost <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = paste0('R14_', toupper(regions)),
                  year_vtg = year_vtg,
                  value = value, unit = unit)

# input
parname <- 'input'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'node_origin', 'commodity', 'level', 'time', 'time_origin', 'value', 'unit')
value <- 1
unit <- 'ton-km-y'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
mode <- 'M1'
time <- 'year'
time_origin <- 'year'
level <- 'shipping'
commodity <- 'shipping-capacity'

input <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = 'R14_GLB', node_origin = paste0('R14_', toupper(regions)),
                  year_act = year_act, year_vtg = year_vtg,
                  mode = mode, time = time, time_origin = time_origin,
                  commodity = commodity, level = level,
                  value = value, unit = unit) %>%
  fix_vintage()


# technical_lifetime
parname <- 'technical_lifetime'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
value <- tech_lifetime
unit <- 'y'
year_vtg <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))

technical_lifetime <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = paste0('R14_', toupper(regions)),
                  year_vtg = year_vtg,
                  value = value, unit = unit)

# emission_factor
parname <- 'emission_factor'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'emission', 'value', 'unit')
mode <- 'M1'
unit <- 'USD/ton-km-y'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
time <- 'year'
emission <- 'CO2'
value <- 60

emission_factor <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = paste0('R14_', toupper(regions)),
                  year_vtg = year_vtg, year_act = year_act,
                  mode = mode, emission = emission, 
                  value = value, unit = unit) %>%
  fix_vintage()

# var_cost (exports)
parname <- 'var_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'time', 'value', 'unit')
mode <- 'M1'
unit <- 'USD/ton-km-y'
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- year_act
time <- 'year'
value <- 0
mode <- 'M1'

var_cost <- 
  build_parameter(parname = parname, varlist = varlist, technology = 'liquid_shipping',
                  node_loc = paste0('R14_', toupper(regions)),
                  year_vtg = year_vtg, year_act = year_act, 
                  mode = mode, time = time,
                  value = value, unit = unit) %>%
  fix_vintage()

# historical_activity
year_act_base <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/historical_activity.R'))

# historical_new_capacity
source(paste0(repo, 'analysis/3_msg_parameters/historical_new_capacity.R'))


# relation_activity (only change exports)
source(paste0(repo, 'analysis/3_msg_parameters/relation_activity.R'))
sink()