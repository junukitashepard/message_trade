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
library('zoo')

raw <-      paste0(wd, "raw/")
temp <-     paste0(wd, 'temp/')
input <-    paste0(wd, "output/derived/")
output <-   paste0(wd, "output/analysis/msg_parameters/")

# Import functions 
###################
source(paste0(repo, "analysis/3_msg_parameters/shipping_technology/1_shipping_parameter_base.R"))

# Define parameters of interest and energy commodities
#######################################################
# List of parameters
parameter_list <- c('capacity_factor', 'emission_factor', 'fix_cost', 
                    'input', 'output', 'inv_cost',
                    'technical_lifetime',
                    'var_cost')

# List of energy commodities
liquid_list <- c('oil', 'loil', 'foil')

# Lifetime of ship
tech_lifetime <- 25

# liquid_shipping (Diesel)
##########################
ls_diesel.capacity_factor <- build_capacity_factor('liquid_shipping_diesel', no.vintage = F)
ls_diesel.fix_cost <- build_fix_cost('liquid_shipping_diesel', 0, no.vintage = F)
ls_diesel.inv_cost <- build_inv_cost('liquid_shipping_diesel', 6.24)
ls_diesel.input <- build_input('liquid_shipping_diesel', c('fueloil'), 0.009, no.vintage = F)
ls_diesel.output <- build_output('liquid_shipping_diesel', no.vintage = F)
ls_diesel.technical_lifetime <- build_technical_lifetime('liquid_shipping_diesel', tech_lifetime)
ls_diesel.emission_factor <- build_emission_factor('liquid_shipping_diesel', 0.02, no.vintage = F)
ls_diesel.var_cost <- build_var_cost('liquid_shipping_diesel', 0, no.vintage = F)

# liquid_shipping (LNG)
##########################
ls_LNG.capacity_factor <- build_capacity_factor('liquid_shipping_LNG', no.vintage = F)
ls_LNG.fix_cost <- build_fix_cost('liquid_shipping_LNG', 0, no.vintage = F)
ls_LNG.inv_cost <- build_inv_cost('liquid_shipping_LNG', 7.05)
ls_LNG.input <- build_input('liquid_shipping_LNG', c('LNG'), 0.004, no.vintage = F)
ls_LNG.output <- build_output('liquid_shipping_LNG', no.vintage = F)
ls_LNG.technical_lifetime <- build_technical_lifetime('liquid_shipping_LNG', tech_lifetime)
ls_LNG.emission_factor <- build_emission_factor('liquid_shipping_LNG', 0.009, no.vintage = F)
ls_LNG.var_cost <- build_var_cost('liquid_shipping_LNG', 0, no.vintage = F)

# liquid_shipping (Electricity)
##########################
ls_elec.capacity_factor <- build_capacity_factor('liquid_shipping_elec', no.vintage = F)
ls_elec.fix_cost <- build_fix_cost('liquid_shipping_elec', 0, no.vintage = F)
ls_elec.inv_cost <- build_inv_cost('liquid_shipping_elec', 8.69)
  ls_elec.inv_cost$value[ls_elec.inv_cost$year_vtg < 2040] <- 15 # make prohibitively expensive before 2040
ls_elec.input <- build_input('liquid_shipping_elec', c('electr'), 0.003, no.vintage = F)
ls_elec.output <- build_output('liquid_shipping_elec', no.vintage = F)
ls_elec.technical_lifetime <- build_technical_lifetime('liquid_shipping_elec', tech_lifetime)
ls_elec.emission_factor <- build_emission_factor('liquid_shipping_elec', 0, no.vintage = F)
ls_elec.var_cost <- build_var_cost('liquid_shipping_elec', 0, no.vintage = F)

# Historical and relation parameters
#####################################
# historical_activity (assume only diesel shipping)
shipping_type <- 'liquid_shipping_diesel'
varname <- 'ls_diesel.historical_activity'
mode <- 'M1'
time <- 'year'
unit <- 'bton-km-y'
msg_years <- c(seq(1970, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/historical_activity.R'))

# historical_new_capacity (assume only diesel shipping)
shipping_type <- 'liquid_shipping_diesel'
varname <- 'ls_diesel.historical_new_capacity'
unit <- 'bton-km'
msg_years <- c(seq(1970, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/historical_new_capacity.R'))

# relation_activity (shipping technology constraint)
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/relation_activity.R'))
ls.relation_activity <- build_relation_activity('liquid_shipping', liquid_list)

# relation_upper (set to 0 for shipping technology constraint)
year_rel <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
source(paste0(repo, 'analysis/3_msg_parameters/shipping_technology/relation_upper.R'))
ls.relation_upper <- 
  build_relation_upper(relation = 'lim_liquid_shipping', value = 0)

# Export parameters
###################
for (p in parameter_list) {
  
  for (t in c('diesel', 'LNG', 'elec')) {
    
    assign('df', get(paste0('ls_', t, '.', p)))
    write.csv(df, file.path(output, paste0(p, "/", 'liquid_shipping_', t, '.csv')))
  }
}

write.csv(ls_diesel.historical_activity, file.path(output, 'historical_activity/liquid_shipping_diesel.csv'))
write.csv(ls_diesel.historical_new_capacity, file.path(output, 'historical_new_capacity/liquid_shipping_diesel.csv'))

write.csv(ls.relation_activity, file.path(output, 'relation_activity/liquid_shipping.csv'))
write.csv(ls.relation_upper, file.path(output, 'relation_upper/liquid_shipping.csv'))
