####################################
# Compile dataframe for carbon tax #
####################################
# Create empty dataframe for parameter tax_emission
type_year <- MESSAGE.years[MESSAGE.years >= MESSAGE.model.horizon.start]

basedf <- expand.grid(toupper(paste0(region.number, '_', region.list)), type_year)
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

tax_df <- tax_scenarios(100) # in $/tCO2

for (scenario in c('baseline', 'tariff_high', 'tariff_low')) {
  write.csv(tax_df, file.path(output, paste0('CO2_tax_', scenario, '/tax_emission/tax_emission.csv')))
}
  
  