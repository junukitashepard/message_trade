###############
# Pre-process #
###############
rm(list = ls())
repo <- "H:/message_trade/"
wd <- "H:/data/"

library('e1071')
library('geosphere')
library('ggplot2')
library('jsfunctions')
library('magrittr')
library('maptools')
library('openxlsx')
library('plyr')
library('reticulate')
library('RMySQL')
library('stringr')
library('tidyr')
library('zoo')
library('dplyr')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output")
output <-   paste0(wd, "output")
temp <-     paste0(wd, "temp")

setwd(repo)
config <- reticulate::import('config')

# Nodes and sea distance #
##########################
# Degrees of separation between uniform sea nodes
node.interval <- 5 

# CSV of major sea and inland ports
major.ports.csv <- 'H:/message_trade/user_inputs/default/major_ports.csv'

# MESSAGE parameterization #
############################
# Temporal specification
MESSAGE.years <- config$MESSAGE_years
MESSAGE.model.horizon.start <- config$MESSAGE_model_horizon_start # When does the model horizon start?

# Regional specification 
region.number <- config$region_number
regional.specification.csv <- config$regional_specification_csv
region.list <- config$regions_list
region.list.trade <- config$region_list_trade

# List of UNCTAD economies
economy_list <- c('Africa', 'Asia', 'Europe', 'Latin America and the Caribbean', 'Northern America', 'Oceania')

# List of technologies
export_technologies <- config$export_technologies
import_technologies <- config$import_technologies

# Energy commodities modeled
energy.types <-config$energy_types
energy.types.BACI <- config$energy_types_BACI # Energy types found in BACI
energy.types.oil.history <- config$energy_types_oil_history # Energy types for which historical activity can be proxied by oil
energy.types.trade.foil <- config$energy_types_trade_foil # Energy types for which trade variable costs behave like fuel oil
energy.types.trade.LNG <- config$energy_types_trade_LNG # Energy types for which trade variable costs behave like LNG

# Parameters
parameter.list <- config$parameter_list

# Technical lifetime
MESSAGE.technical.lifetime = config$MESSAGE_technical_lifetime

# Capacity factor
MESSAGE.capacity.factor = config$MESSAGE_capacity_factor

# Shipping parameters
shipping_liquid_list <- config$shipping_liquid_list
shipping_solid_list <- config$shipping_solid_list
shipping_LNG_list <- config$shipping_LNG_list
shipping_lh2_list <- config$shipping_lh2_list

shipping_technical_lifetime <- config$shipping_technical_lifetime

shipping_parameter_list <- c('capacity_factor', 'emission_factor', 'fix_cost', 
                             'input', 'output', 'inv_cost',
                             'technical_lifetime',
                             'var_cost')
###################
# RUN ALL SCRIPTS #
###################
# Derived
source(paste0(repo, 'derived/1_trade/run_all.R'))
source(paste0(repo, 'derived/2_nodes/run_all.R'))

# Analysis
source(paste0(repo, 'analysis/2_regressions/run_all.R'))
source(paste0(repo, 'analysis/3_msg_parameters/run_all.R'))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/build_shipping_parameters.R'))

