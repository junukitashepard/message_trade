######################################
# Pre-proces, after running baseline #
######################################
rm(list = ls())
repo <- "H:/message_trade/"
wd <- "H:/data/"

library('cluster')
library('e1071')
library('gdxrrw')
library('geosphere')
library('ggplot2')
library('jsfunctions')
library('magrittr')
library('maptools')
library('NbClust')
library('openxlsx')
library('plyr')
library('reticulate')
library('RMySQL')
library('stringr')
library('tidyr')
library('dplyr')

raw <-       paste0(wd, 'raw/')
reg.input <- paste0(wd, "output/analysis/regress/")
input <-     paste0(wd, "output/analysis/msg_parameters/")
output <-    paste0(wd, "output/analysis/msg_parameters/SCENARIOS/")
msg_dir <-   "C:/ProgramData/Anaconda3/Lib/site-packages/message_ix/model/output"

setwd(repo)
config <- reticulate::import('config')

# Scenario inputs #
###################
baseline_no_tariff_version = 15

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

###################
# RUN ALL SCRIPTS #
###################
# Build scenarios
source(paste0(repo, 'analysis/4_run_message/build_scenarios/tariffs.R'))
source(paste0(repo, 'analysis/4_run_message/build_scenarios/emission_taxes.R'))
source(paste0(repo, 'analysis/4_run_message/build_scenarios/build_scenario.R'))
