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
msg_dir <- "C:/ProgramData/Anaconda3/Lib/site-packages/message_ix/model/output"

# Import baseline parameters (import variable costs)
#####################################################
var_cost.base <- data.frame()
for (t in import_technologies) {
  indf <- readRDS(file.path(input, paste0('var_cost/', t, '.rds')))
  var_cost.base <- rbind(var_cost.base, indf)
}

var_cost.base$energy <- stringr::str_replace(var_cost.base$technology, '_imp', "")
var_cost.base$value <- NA

# Import model commodity prices for importers # 
###############################################
for (t in c('shipped', 'secondary', 'primary')) {
  price_df <- read_MESSAGE(msg_scenario = 'baseline_no_tariff', 
                           msg_version = baseline_no_tariff_version, 
                           msg_variable = 'PRICE_COMMODITY')
  price_df <- subset(price_df, grepl(t, level))
  
  price_df$importer <- paste0('R14_', toupper(substr(price_df$commodity, nchar(price_df$commodity)-2, nchar(price_df$commodity))))
  price_df$energy <- sub('\\_.*', '', price_df$commodity)
  price_df$energy[price_df$energy == 'fueloil'] <- 'foil'
  price_df$energy[price_df$energy == 'lightoil'] <- 'loil'
  price_df$energy[price_df$energy == 'crudeoil'] <- 'oil'
  price_df$baseline_price <- price_df$value
  price_df$year_all <- as.numeric(price_df$year_all)
  
  price_df <- price_df[c('importer', 'energy', 'year_all', 'baseline_price')]
  names(price_df) <- c('importer', 'energy', 'year_all', paste0('price_', t))
  
  assign(paste0('prices_', t), price_df, envir = parent.frame())
}

# Minus price of secondary/primary product
prices_secondary <- group_by(prices_secondary, energy, year_all) %>% summarize(price_secondary = min(price_secondary, na.rm = T))
prices_primary <- group_by(prices_primary, energy, year_all) %>% summarize(price_primary = min(price_primary, na.rm = T))

prices <- left_join(prices_shipped, prices_secondary, by = c('energy', 'year_all'))
prices <- left_join(prices, prices_primary, by = c('energy', 'year_all'))

prices$value <- 0
prices$value[!is.na(prices$price_shipped)] <- prices$price_shipped[!is.na(prices$price_shipped)]

prices <- prices[c('importer', 'energy', 'year_all', 'value')]
names(prices) <- c('importer', 'energy', 'year_all', 'baseline_price')

# Combine historic prices with import variable costs #
######################################################
var_cost.base$node_loc <- as.character(var_cost.base$node_loc)
df <- inner_join(var_cost.base, prices, by = c('node_loc' = 'importer', 'year_vtg' = 'year_all', 'energy'))
df$value <- df$baseline_price

var_cost.base <- df[c('technology', 'time', 'value', 'unit', 'year_act', 'year_vtg', 'node_loc', 'mode')]

# Build scenario #
##################
# Adjust var_cost
adj_var_cost <- function(scenario, adjust_variable) {
  
  df <- readRDS(file.path(repo, paste0('analysis/4_run_message/build_scenarios/var_cost_effects/', scenario, '.rds')))
  suppressWarnings(df <- left_join(var_cost.base, df, by = c('technology', 'node_loc', 'year_act')))
  names(df)[names(df) == adjust_variable] <- 'adj_var'
  df$adj_var[is.nan(df$adj_var) | is.na(df$adj_var) | is.infinite(df$adj_var)] <- 0
  
  df$value <- df$value*(df$adj_var/100)
  df$value[df$value < 0 ] <- 0
  df$adj_var <- NULL
  
  return(df)
}

# Adjust for tariffs
baseline <- adj_var_cost('baseline', 'tariff')
tariff_high <- adj_var_cost('tariff_high', 'tariff')
tariff_low <- adj_var_cost('tariff_low', 'tariff')

# Plot
ggplot(aes(x = year_act, y = value, colour = node_loc, shape = technology), data = tariff_high) +
  geom_line() +geom_point()

# Write scenario output
for (scen in c('baseline', 'tariff_high', 'tariff_low')) {
  for (t in import_technologies) {
    
    assign('df', get(scen))
    
    df <- subset(df, grepl(t, technology))
    if (t == 'oil_imp') {df <- subset(df, substr(technology, 1, 3) == 'oil')} # so we don't include foil or loil
    
    df <- subset(df, !is.nan(value) & !is.infinite(value) & !is.na(value))
    
    saveRDS(df, file.path(output, paste0(scen, '/var_cost/', t, '.rds')))
    write.csv(df, file.path(output, paste0(scen, '/var_cost/', t, '.csv')))
    write.csv(df, file.path(output, paste0('CO2_tax_', scen, '/var_cost/', t, '.csv')))
  }
}
