########################
# Build all parameters #
########################
# Import functions 
###################
source(paste0(repo, 'analysis/3_msg_parameters/scale_msg_parameter/functions.R'))
source(paste0(repo, 'analysis/3_msg_parameters/build_parameters.R'))
source(paste0(repo, 'analysis/3_msg_parameters/activity_parameters/build_activity.R'))

# Import data files 
####################
# Import regionally aggregated trade data
trade.df <- read.csv(file.path(input, 'derived/trade/regional_trade.csv'), stringsAsFactors = F)
trade.df$year[trade.df$year == 2014] <- 2015 # Use 2014 data as proxy for 2015 activity

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
year_act <- MESSAGE.years
year_vtg <- MESSAGE.years
value <- MESSAGE.capacity.factor
unit <- '%'
time <- 'year'
source(paste0(repo, 'analysis/3_msg_parameters/capacity_factor.R'))

# fix_cost
parname <- 'fix_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'value', 'unit')
unit <- 'USD/GWa'
year_act <- MESSAGE.years
year_vtg <- MESSAGE.years
source(paste0(repo, 'analysis/3_msg_parameters/fix_cost.R'))

# growth_activity_lo, growth_activity_up
parname <- 'growth_activity'
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- MESSAGE.years
unit <- '%'
time <- 'year'
value_lo <- -0.2
value_up <- 0.2
source(paste0(repo, 'analysis/3_msg_parameters/growth_activity.R'))

# historical_activity
year_act_base <- MESSAGE.years
source(paste0(repo, 'analysis/3_msg_parameters/historical_activity.R'))

# historical_new_capacity
source(paste0(repo, 'analysis/3_msg_parameters/historical_new_capacity.R'))

# initial_activity_lo, initial_activity_up
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- MESSAGE.years
value_lo = 0
value_up = 2
time <- 'year'
unit <- 'GWa'
source(paste0(repo, 'analysis/3_msg_parameters/initial_activity.R'))

# input
source(paste0(repo, 'analysis/3_msg_parameters/input.R'))

# inv_cost
parname <- 'inv_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
unit <- 'USD/GWa'
year_vtg <- MESSAGE.years
source(paste0(repo, 'analysis/3_msg_parameters/inv_cost.R'))

# level_cost_activity_soft_lo, level_cost_activity_soft_up
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- MESSAGE.years[MESSAGE.years >= MESSAGE.model.horizon.start] # future only
unit <- '???'
time <- 'year'
value_up <- 0.5
value_lo <- -0.5
source(paste0(repo, 'analysis/3_msg_parameters/level_cost_activity_soft.R'))

# output
source(paste0(repo, 'analysis/3_msg_parameters/output.R'))

# ref_new_capacity
source(paste0(repo, 'analysis/3_msg_parameters/ref_new_capacity.R'))

# ref_activity
source(paste0(repo, 'analysis/3_msg_parameters/ref_activity.R'))

# soft_activity
varlist <-  c('node_loc', 'technology', 'year_act', 'time', 'value', 'unit')
year_act <- MESSAGE.years[MESSAGE.years >= MESSAGE.model.horizon.start] # future only
unit <- '???'
time <- 'year'
value_lo <- -0.1
value_up <- 0.1
source(paste0(repo, 'analysis/3_msg_parameters/soft_activity.R'))

# technical_lifetime
parname <- 'technical_lifetime'
varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
value <- MESSAGE.technical.lifetime
unit <- 'y'
year_vtg <- MESSAGE.years
source(paste0(repo, 'analysis/3_msg_parameters/technical_lifetime.R'))

# emission_factor
parname <- 'emission_factor'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'emission', 'value', 'unit')
mode <- 'M1'
unit <- 'kg/kWa'
year_act <- MESSAGE.years
year_vtg <- year_act
time <- 'year'
emission <- 'CO2'
source(paste0(repo, 'analysis/3_msg_parameters/emission_factor.R'))

# var_cost (exports)
parname <- 'var_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'time', 'value', 'unit')
mode <- 'M1'
unit <- 'USD/GWa'
year_act <- MESSAGE.years
year_vtg <- year_act
time <- 'year'
source(paste0(repo, 'analysis/3_msg_parameters/var_cost.R'))

# var_cost (imports)
parname <- 'var_cost'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'time', 'value', 'unit')
mode <- 'M1'
unit <- 'USD/GWa'
year_act <- MESSAGE.years
year_vtg <- year_act
time <- 'year'
value <- 0
source(paste0(repo, 'analysis/3_msg_parameters/var_cost_imports.R'))

# relation_activity (only change exports, include imports too)
year_act <- MESSAGE.years
source(paste0(repo, 'analysis/3_msg_parameters/relation_activity.R'))
sink()