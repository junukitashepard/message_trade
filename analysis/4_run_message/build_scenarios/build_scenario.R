########################
# Build all parameters #
########################
rm(list = ls())
wd <- 'H:/message_trade/analysis/4_run_message/build_scenarios/'
wd.data <- 'H:/data/'
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')

reg.input <- paste0(wd.data, "output/analysis/regress/")
input <-   paste0(wd.data, "output/analysis/msg_parameters/")
output <-  paste0(wd.data, "output/analysis/msg_parameters/SCENARIOS/")

# Import baseline parameters (import variable costs)
#####################################################
import_technologies <- c('oil_imp', 'foil_imp', 'loil_imp', 'coal_imp', 'LNG_imp')

var_cost.base <- data.frame()
for (t in import_technologies) {
  indf <- readRDS(file.path(input, paste0('var_cost/', t, '.rds')))
  var_cost.base <- rbind(var_cost.base, indf)
}

var_cost.base$energy <- stringr::str_replace(var_cost.base$technology, '_imp', "")
var_cost.base$value <- NA

# Import historical import costs from regression analysis # 
###########################################################
regdf <- readRDS(file.path(reg.input, 'regdf.rds'))

regdf <- group_by(regdf, year, energy, msg.region.j) %>% 
         summarize(mean.vc = mean(var_cost, na.rm = T),
                   med.vc = median(var_cost, na.rm = T))
regdf <- subset(regdf, !is.na(msg.region.j) & msg.region.j != "")

regdf$msg.region.j <- paste0('R14_', regdf$msg.region.j)

hist.med <- group_by(regdf, energy, msg.region.j) %>% summarize(hist.med.vc  = median(med.vc, na.rm = T))

# Combine historic prices with import variable costs #
######################################################
df <- left_join(var_cost.base, regdf, by = c('node_loc' = 'msg.region.j', 'year_vtg' = 'year', 'energy'))
df <- left_join(df, hist.med, by = c('node_loc' = 'msg.region.j', 'energy'))

df$value <- df$med.vc # Use median variable cost
df$value[is.na(df$value)] <- df$hist.med.vc[is.na(df$value)] # Use historic median for where missing or in future

df$value[df$value > 5000] <- NA # Removes outlier for SCS

var_cost.base <- df[c('technology', 'time', 'value', 'unit', 'year_act', 'year_vtg', 'node_loc', 'mode')]

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
tech_lifetime = 5

# List of energy commodities
energy_list <- c('oil', 'coal', 'loil', 'foil', 'LNG')

# List of technologies
export_technologies <- c('oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp')

# List of regions
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Build scenario #
##################
# Adjust var_cost
adj_var_cost <- function(scenario, adjust_variable) {
  
  df <- readRDS(file.path(wd, paste0('var_cost_effects/', scenario, '.rds')))
  suppressWarnings(df <- left_join(var_cost.base, df, by = c('technology', 'node_loc', 'year_act')))
  names(df)[names(df) == adjust_variable] <- 'adj_var'
  df$adj_var[is.nan(df$adj_var) | is.na(df$adj_var) | is.infinite(df$adj_var)] <- 0
  
  df$value <- (df$value + (df$value*(df$adj_var/100)))/100
  df$value[df$value < 0 ] <- 0
  df$adj_var <- NULL
  
  return(df)
}

# Adjust for tariffs
baseline <- adj_var_cost('baseline', 'mean_tariff')
tariff_high <- adj_var_cost('tariff_high', 'mean_tariff')
tariff_low <- adj_var_cost('tariff_low', 'mean_tariff')

# Plot
ggplot(aes(x = year_act, y = value, colour = node_loc, shape = technology), data = baseline) +
  geom_point()

# Write scenario output
for (scen in c('baseline', 'tariff_high', 'tariff_low')) {
  for (t in import_technologies) {
    
    assign('df', get(scen))
    
    df <- subset(df, grepl(t, technology))
    if (t == 'oil_imp') {df <- subset(df, substr(technology, 1, 3) == 'oil')} # so we don't include foil or loil
    
    df <- subset(df, !is.nan(value) & !is.infinite(value) & !is.na(value))
    
    saveRDS(df, file.path(output, paste0(scen, '/var_cost/', t, '.rds')))
    write.csv(df, file.path(output, paste0(scen, '/var_cost/', t, '.csv')))
  }
}
