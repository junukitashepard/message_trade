###############################################
# Compile data to create chart with emissions #
###############################################
rm(list = ls())
wd <- "C:/Users/jus3/message_trade"
repo <- "C:/Users/jus3/message_trade"
msg_dir <- "C:/Users/jus3/message_trade/gdx_files"

output <- paste0(repo, '/tables')
figures <- paste0(repo, '/figures/unformatted')

setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('NISTunits')
library('gridExtra')
library('grid')
library('RColorBrewer')

igdx('C:/GAMS/win64/29.1') # Set GDX path

regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Import gdx activity
import_gdx_activity_imports <- function(scenario, version, scenario.name) {
  activity <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'ACT')
  
  activity$vintage <- as.numeric(activity$vintage)
  activity$year_all <- as.numeric(activity$year_all)
  
  activity <- subset(activity, grepl('_imp', tec) & tec != 'elec_imp' & tec != 'gas_imp')
  
  activity$importer <- stringr::str_replace(activity$node, 'R14_', '')
  activity$energy <- sub("\\_.*", "", activity$tec)
  activity$year <- activity$year_all
  activity$scenario <- scenario
  activity$scenario.name <- scenario.name
  
  activity <- unique(activity[c('scenario', 'scenario.name', 'importer', 'energy', 'year', 'value')])
  
  activity <- subset(activity, value > 1)
  return(activity)
}

# Import gdx emission factor
add_emission_factor <- function(activity.df, scenario, version) {
  
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'emission_factor')
  
  df <- subset(df, field == 'CO2')
  df <- subset(df, grepl('_imp', tec))
  df <- subset(df, tec != 'elec_imp' & tec != 'gas_imp')
  
  df$importer <- stringr::str_replace(df$node, 'R14_', '')
  df$energy <- sub("\\_.*", "", df$tec)
  
  df <- unique(df[c('importer', 'energy', 'value')])
  names(df) <- c('importer', 'energy', 'emission_factor')
  
  activity.df <- left_join(activity.df, df, by = c('importer', 'energy'))
  
  activity.df$imported_emissions <- activity.df$value * activity.df$emission_factor

  activity.df <- group_by(activity.df, importer, year, scenario, scenario.name) %>%
                 summarize(imported_emissions = sum(imported_emissions))
  
  # Fill in zeros
  outdf <- expand.grid(unique(activity.df$importer), unique(activity.df$year))
  outdf$scenario <- scenario
  names(outdf) <- c('importer', 'year', 'scenario')
  outdf$importer <- as.character(outdf$importer)
  outdf <- left_join(outdf, activity.df, by = c('importer', 'year', 'scenario'))
  
  outdf <- outdf[c('importer', 'year', 'scenario', 'imported_emissions')]
  outdf$imported_emissions[is.na(outdf$imported_emissions)] <- 0
  
  return(outdf)
}

# Import gdx emissions (all)
add_all_emissions <- function(activity.df, scenario, version) {
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'EMISS')
  
  df <- subset(df, emission == 'CO2' & type_tec == 'all')
  df$importer <- sub('.*\\_', '', df$node)
  df$scenario <- scenario
  
  df <- df[c('scenario', 'importer', 'field', 'value')]
  names(df) <- c('scenario', 'importer', 'year', 'overall_emissions')
  df$year <- as.numeric(df$year)
  
  activity.df <- left_join(activity.df, df, by = c('scenario', 'importer', 'year'))
  
  return(activity.df)
}

# Import files, estimate emissions
baseline <- import_gdx_activity_imports(scenario = 'baseline', version = 5, scenario.name = 'Baseline') %>%
  add_emission_factor(scenario = 'baseline', version = 5) %>%
  add_all_emissions(scenario = 'baseline', version = 5)

tariff_low <- import_gdx_activity_imports(scenario = 'tariff_low', version = 3, scenario.name = 'Tariff: Low') %>%
  add_emission_factor(scenario = 'tariff_low', version = 3) %>%
  add_all_emissions(scenario = 'tariff_low', version = 3)

tariff_high <- import_gdx_activity_imports(scenario = 'tariff_high', version = 3, scenario.name = 'Tariff: High') %>%
  add_emission_factor(scenario = 'tariff_high', version = 3) %>%
  add_all_emissions(scenario = 'tariff_high', version = 3)

CO2_baseline <- import_gdx_activity_imports(scenario = 'CO2_tax_baseline', version = 2, scenario.name = 'CO2 Tax: Baseline') %>%
  add_emission_factor(scenario = 'CO2_tax_baseline', version = 2) %>%
  add_all_emissions(scenario = 'CO2_tax_baseline', version = 2)

CO2_tariff_low <- import_gdx_activity_imports(scenario = 'CO2_tax_tariff_low', version = 2, scenario.name = 'CO2 Tax: Low Tariffs') %>%
  add_emission_factor(scenario = 'CO2_tax_tariff_low', version = 2) %>%
  add_all_emissions(scenario = 'CO2_tax_tariff_low', version = 2)

CO2_tariff_high <- import_gdx_activity_imports(scenario = 'CO2_tax_tariff_high', version = 2, scenario.name = 'CO2 Tax: High Tariffs') %>%
  add_emission_factor(scenario = 'CO2_tax_tariff_high', version = 2) %>%
  add_all_emissions(scenario = 'CO2_tax_tariff_high', version = 2)

alldf <- rbind(baseline, 
               tariff_high, tariff_low,
               CO2_baseline, CO2_tariff_low, CO2_tariff_high)

alldf <- group_by(alldf, scenario, year) %>% 
         summarize(imported_emissions = sum(imported_emissions, na.rm = T),
                   overall_emissions = sum(overall_emissions, na.rm = T))

# Plot difference from baseline #
#################################
plotdf <- subset(alldf, scenario != 'baseline')

basedf <- subset(alldf, scenario == 'baseline')[c('year', 'imported_emissions', 'overall_emissions')]
names(basedf) <- c('year', 'imported_baseline', 'overall_baseline')

plotdf <- left_join(plotdf, basedf, by = c('year'))

plotdf$diff_imported_emissions <- ((plotdf$imported_emissions - plotdf$imported_baseline)/plotdf$imported_baseline) * 100
plotdf$diff_overall_emissions <- ((plotdf$overall_emissions - plotdf$overall_baseline)/plotdf$overall_baseline) * 100
plotdf <- subset(plotdf, year < 2055)

# Plot
imported_plot <-
  ggplot(aes(x = year, y = diff_imported_emissions, colour = scenario, group = scenario), data = plotdf) + 
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "", y = '% difference from baseline', 
       colour = 'Scenario', title = "Emissions linked to fuel imports") + 
  theme(text = element_text(size = 15), legend.position = 'none') +
  scale_colour_brewer(palette = 'Set1') +
  lims(y = c(-60, 20))

overall_plot <-
  ggplot(aes(x = year, y = diff_overall_emissions, colour = scenario, group = scenario), data = plotdf) + 
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "", y = '% difference from baseline', 
       colour = 'Scenario', title = "Overall emissions") + 
  theme(text = element_text(size = 15), legend.position = 'none') +
  scale_colour_brewer(palette = 'Set1') +
  lims(y = c(-60, 20))

# Output as PDF
ggsave(file.path(figures, 'imported_emisisons_global.pdf'), imported_plot,
       units = 'in', width = 6, height = 6, device = 'pdf')
ggsave(file.path(figures, 'overall_emissions_global.pdf'), overall_plot,
       units = 'in', width = 6, height = 6, device = 'pdf')
