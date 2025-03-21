####################################################
# Build parameters for shipping technology: solids #
####################################################
# Import functions 
###################
source(paste0(repo, "analysis/3_msg_parameters/shipping_technology/shipping_parameter_base.R"))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/relation_activity.R'))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/relation_upper.R'))


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

  for (p in shipping_parameter_list[!(shipping_parameter_list %in% 'inv_cost')]) {
    assign('df', get(paste0('par.', p)))
    write.csv(df, file.path(output, paste0('analysis/msg_parameters/', p, "/", type_of_shipping, '_', shipping_fuel, '.csv')))
  }
  
  # Investment cost parameter will be stored in SCENARIOS if it is not baseline
  if (technology_trend == 'BAU') {
    write.csv(par.inv_cost, file.path(output, paste0("analysis/msg_parameters/inv_cost/", type_of_shipping, '_', shipping_fuel, '.csv')))
  } else if (technology_trend == 'CLT') {
    write.csv(par.inv_cost, file.path(output, paste0("analysis/msg_parameters/SCENARIOS/shipping_CLT/inv_cost/", type_of_shipping, '_', shipping_fuel, '.csv')))
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
                              fix_cost.value = 0, inv_cost.value = 7.4e-7, var_cost.value = 0,
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
                              fix_cost.value = 0, inv_cost.value = 1.1e-6, var_cost.value = 0,
                              input.value = .003, technical_lifetime.value = 25, emission_factor.value = 0,
                              technology_trend = tech_scenario)
  
  # LNG shipping technologies
  ###########################
  # LNG shipping (diesel)
  compile_shipping_parameters(type_of_shipping = 'LNG_shipping',
                              shipping_fuel = 'diesel', shipping_fuel.msg = 'fueloil',
                              fix_cost.value = 0, inv_cost.value = 1.38e-7, var_cost.value = 0,
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
                              fix_cost.value = 0, inv_cost.value = 1.54e-6, var_cost.value = 0,
                              input.value = 0.003, technical_lifetime.value = 25, emission_factor.value = 0,
                              technology_trend = tech_scenario)
  
  # Liquid hydrogen shipping technologies
  #######################################
  # LH2 shipping (diesel)
  compile_shipping_parameters(type_of_shipping = 'lh2_shipping',
                              shipping_fuel = 'diesel', shipping_fuel.msg = 'fueloil',
                              fix_cost.value = 0, inv_cost.value = 6.6e-6, var_cost.value = 0,
                              input.value = 0.024, technical_lifetime.value = 25, emission_factor.value = 0.052,
                              technology_trend = tech_scenario)
  
  # LH2 shipping (LNG)
  compile_shipping_parameters(type_of_shipping = 'lh2_shipping',
                              shipping_fuel = 'LNG', shipping_fuel.msg = 'LNG',
                              fix_cost.value = 0, inv_cost.value = 7.71e-6, var_cost.value = 0,
                              input.value = 0.007, technical_lifetime.value = 25, emission_factor.value = 0.014,
                              technology_trend = tech_scenario)
  
  # LH2 shipping (Electricity)
  compile_shipping_parameters(type_of_shipping = 'lh2_shipping',
                              shipping_fuel = 'elec', shipping_fuel.msg = 'electr',
                              fix_cost.value = 0, inv_cost.value = 7.71e-5, var_cost.value = 0,
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
  
  write.csv(par.relation_activity, file.path(output, paste0('analysis/msg_parameters/relation_activity/', type_of_shipping, '.csv')))
  write.csv(par.relation_upper, file.path(output, paste0('analysis/msg_parameters/relation_upper/', type_of_shipping, '.csv')))
}

# Run function
build_relation_parameters('liquid_shipping', shipping_liquid_list, MESSAGE.years)
build_relation_parameters('solid_shipping', shipping_solid_list, MESSAGE.years)
build_relation_parameters('LNG_shipping', shipping_LNG_list, MESSAGE.years)
build_relation_parameters('lh2_shipping', shipping_lh2_list, MESSAGE.years)

# Historical parameters (only for diesel shipping)
###################################################
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
# No historical LH2 shipping



