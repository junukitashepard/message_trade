####################################
# Compile dataframe for carbon tax #
####################################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')

output <-  paste0(wd, "output/analysis/msg_parameters/SCENARIOS/")

# Create empty dataframe for parameter tax_emission
type_year <- c(seq(2020, 2055, by = 5), seq(2060, 2110, by = 10))
regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')
  regions <- paste0('R14_', toupper(regions))

basedf <- expand.grid(regions, type_year)
names(basedf) <- c('node', 'type_year')

basedf$type_emission <- 'totalCe'
basedf$type_tec <- 'all'
basedf$unit <- 'USD/tC'

# Create high tax and low tax scenarios
tax_scenarios <- function(scen.value) {
  df <- basedf
  df$value <- 0
  
  # Get annual increase of 5%
  df$i <- df$type_year-min(df$type_year)
  df$value <- scen.value*(1.05^df$i)
  
  df <- arrange(df, type_emission, type_tec, unit, node, type_year)
  
  df <- df[c('node', 'type_emission', 'type_tec', 'type_year', 'value', 'unit')]
  return(df)
}

#tax_hi <- tax_scenarios(60)
tax_lo <- tax_scenarios(100) # in $/tCO2

for (scenario in c('baseline', 'tariff_high', 'tariff_low')) {
  write.csv(tax_lo, file.path(output, paste0('CO2_tax_', scenario, '/tax_emission/tax_emission.csv')))
}
  
  