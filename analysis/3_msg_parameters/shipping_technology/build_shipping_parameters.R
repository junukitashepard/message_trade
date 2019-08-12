####################################################
# Build parameters for shipping technology: solids #
####################################################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('zoo')

raw <-      paste0(wd, "raw/")
temp <-     paste0(wd, 'temp/')
input <-    paste0(wd, "output/derived/")
output <-   paste0(wd, "output/analysis/msg_parameters/")

# Import functions 
###################
source(paste0(repo, "analysis/3_msg_parameters/shipping_technology/shipping_parameter_base.R"))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/relation_activity.R'))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/relation_upper.R'))

# Define parameters of interest and energy commodities
#######################################################
# List of parameters
parameter_list <- c('capacity_factor', 'emission_factor', 'fix_cost', 
                    'input', 'output', 'inv_cost',
                    'technical_lifetime',
                    'var_cost')

# List of energy commodities
liquid_list <- c('oil', 'loil', 'foil')
solid_list <- c('coal')
LNG_list <- c('LNG')

# Technical lifetime
tech_lifetime <- 25

# Compile shipping parameter (function)
########################################
compile_shipping_parameters <- function(type_of_shipping, shipping_fuel, shipping_fuel.msg, 
                                        fix_cost.value, inv_cost.value, var_cost.value, 
                                        input.value, technical_lifetime.value, emission_factor.value,
                                        technology_trend = 'BAU') {
  
  par.capacity_factor <- build_capacity_factor(paste0(type_of_shipping, '_', shipping_fuel), 
                                               no.vintage = F)
  par.fix_cost <- build_fix_cost(paste0(type_of_shipping, '_', shipping_fuel), 
                                 fix_cost.value, 
                                 no.vintage = F)
  
  par.inv_cost <- build_inv_cost(paste0(type_of_shipping, '_', shipping_fuel), 
                                 inv_cost.value, cost_scenario = technology_trend)
  
  par.input <- build_input(paste0(type_of_shipping, '_', shipping_fuel), 
                           shipping_fuel.msg, 
                           input.value, 
                           no.vintage = F)
  
  par.output <- build_output(paste0(type_of_shipping, '_', shipping_fuel), 
                             no.vintage = F)
  
  par.technical_lifetime <- build_technical_lifetime(paste0(type_of_shipping, '_', shipping_fuel), 
                                                     technical_lifetime.value)
  
  par.emission_factor <- build_emission_factor(paste0(type_of_shipping, '_', shipping_fuel), 
                                               emission_factor.value, 
                                               no.vintage = F)
  
  par.var_cost <- build_var_cost(paste0(type_of_shipping, '_', shipping_fuel), 
                                 var_cost.value, 
                                 no.vintage = F)

  for (p in parameter_list[!(parameter_list %in% 'inv_cost')]) {
    assign('df', get(paste0('par.', p)))
    write.csv(df, file.path(output, paste0(p, "/", type_of_shipping, '_', shipping_fuel, '.csv')))
  }
  
  # Investment cost parameter will be stored in SCENARIOS if it is not baseline
  if (technology_trend == 'BAU') {
    write.csv(par.inv_cost, file.path(output, paste0("inv_cost/", type_of_shipping, '_', shipping_fuel, '.csv')))
  } else if (technology_trend == 'CLT') {
    write.csv(par.inv_cost, file.path(output, paste0("SCENARIOS/shipping_CLT/inv_cost/", type_of_shipping, '_', shipping_fuel, '.csv')))
  }
}

# Build parameters #
####################
runall_shipping_parameters <- function(tech_scenario) {
  
  # Liquid shipping technologies
  ##############################
  # liquid_shipping (Diesel)
  compile_shipping_parameters(type_of_shipping = 'liquid_shipping',
                              shipping_fuel = 'diesel', shipping_fuel.msg = 'fueloil',
                              fix_cost.value = 0, inv_cost.value = 5.86e-8, var_cost.value = 0,
                              input.value = 0.009, technical_lifetime.value = 25, emission_factor.value = 0.021,
                              technology_trend = tech_scenario)
  
  # liquid_shipping (LNG)
  compile_shipping_parameters(type_of_shipping = 'liquid_shipping',
                              shipping_fuel = 'LNG', shipping_fuel.msg = 'LNG',
                              fix_cost.value = 0, inv_cost.value = 7.40e-8, var_cost.value = 0,
                              input.value = 0.004, technical_lifetime.value = 25, emission_factor.value = 0.009,
                              technology_trend = tech_scenario)
  
  # liquid_shipping (Electricity)
  compile_shipping_parameters(type_of_shipping = 'liquid_shipping',
                              shipping_fuel = 'elec', shipping_fuel.msg = 'electr',
                              fix_cost.value = 0, inv_cost.value = 1.48e-3, var_cost.value = 0,
                              input.value = 0.003, technical_lifetime.value = 25, emission_factor.value = 0,
                              technology_trend = tech_scenario)
  
  # Solid shipping technologies
  #############################
  # Solid shipping (diesel)
  compile_shipping_parameters(type_of_shipping = 'solid_shipping',
                              shipping_fuel = 'diesel', shipping_fuel.msg = 'fueloil',
                              fix_cost.value = 0, inv_cost.value = 7.95e-8, var_cost.value = 0,
                              input.value = 0.01, technical_lifetime.value = 25, emission_factor.value = 0.022,
                              technology_trend = tech_scenario)
  
  # Solid shipping (LNG)
  compile_shipping_parameters(type_of_shipping = 'solid_shipping',
                              shipping_fuel = 'LNG', shipping_fuel.msg = 'LNG',
                              fix_cost.value = 0, inv_cost.value = 1.1e-7, var_cost.value = 0,
                              input.value = .004, technical_lifetime.value = 25, emission_factor.value = .008,
                              technology_trend = tech_scenario)
  
  # Solid shipping (Electricity)
  compile_shipping_parameters(type_of_shipping = 'solid_shipping',
                              shipping_fuel = 'elec', shipping_fuel.msg = 'electr',
                              fix_cost.value = 0, inv_cost.value = 2.2e-3, var_cost.value = 0,
                              input.value = .003, technical_lifetime.value = 25, emission_factor.value = 0,
                              technology_trend = tech_scenario)
  
  # LNG shipping technologies
  ###########################
  # LNG shipping (diesel)
  compile_shipping_parameters(type_of_shipping = 'LNG_shipping',
                              shipping_fuel = 'diesel', shipping_fuel.msg = 'fueloil',
                              fix_cost.value = 0, inv_cost.value = 1.3e-7, var_cost.value = 0,
                              input.value = 0.024, technical_lifetime.value = 25, emission_factor.value = 0.052,
                              technology_trend = tech_scenario)
  
  # LNG shipping (LNG)
  compile_shipping_parameters(type_of_shipping = 'LNG_shipping',
                              shipping_fuel = 'LNG', shipping_fuel.msg = 'LNG',
                              fix_cost.value = 0, inv_cost.value = 1.54e-7, var_cost.value = 0,
                              input.value = 0.007, technical_lifetime.value = 25, emission_factor.value = 0.014,
                              technology_trend = tech_scenario)
  
  # LNG shipping (Electricity)
  compile_shipping_parameters(type_of_shipping = 'LNG_shipping',
                              shipping_fuel = 'elec', shipping_fuel.msg = 'electr',
                              fix_cost.value = 0, inv_cost.value = 3.08e-3, var_cost.value = 0,
                              input.value = 0.003, technical_lifetime.value = 25, emission_factor.value = 0,
                              technology_trend = tech_scenario)
}

runall_shipping_parameters(tech_scenario = 'BAU')
runall_shipping_parameters(tech_scenario = 'CLT')

# Relation parameters
######################
# Build relation_activity and relation_upper (function)
build_relation_parameters <- function(type_of_shipping, shipped_energy, year_list) {
  
  year_act <- year_rel <- year_list
  
  par.relation_activity <- build_relation_activity(type_of_shipping, shipped_energy, year_act = year_act)
  par.relation_upper <- build_relation_upper(relation = paste0('lim_', type_of_shipping), value = 0, year_rel = year_rel)
  
  write.csv(par.relation_activity, file.path(output, paste0('relation_activity/', type_of_shipping, '.csv')))
  write.csv(par.relation_upper, file.path(output, paste0('relation_upper/', type_of_shipping, '.csv')))
}

# Run function
build_relation_parameters('liquid_shipping', liquid_list, c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10)))
build_relation_parameters('solid_shipping', solid_list, c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10)))
build_relation_parameters('LNG_shipping', LNG_list, c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10)))

# Historical parameters (only for diesel shipping)
###################################################
msg_years <- c(seq(1970, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/historical_activity.R'))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/historical_new_capacity.R'))

build_historical_parameters <- function(shipping_type) {
  
  # historical_activity
  reformat_activity(shipping = shipping_type)
  
  # historical_new_capacity
  reformat_capacity(shipping = shipping_type)
}

build_historical_parameters('liquid_shipping_diesel')
build_historical_parameters('LNG_shipping_diesel')
build_historical_parameters('solid_shipping_diesel')


