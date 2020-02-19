######################
# Dependence indices #
######################
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

igdx('C:/GAMS/win64/29.1') # Set GDX path

# Conversion factor
gwa_to_ej <- 8760*3600*(10^-9)

# Import gdx activity
import_gdx_activity <- function(scenario, version, scenario_name) {
  activity <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'ACT')
  
  activity$vintage <- as.numeric(activity$vintage)
  activity$year_all <- as.numeric(activity$year_all)
  
  activity <- subset(activity, grepl('_exp_', tec) & grepl('gas_exp', tec) == F)
  activity <- subset(activity, grepl('_glb', tec) == F)
  
  activity$importer <- toupper(substr(activity$tec, nchar(activity$tec)-2, nchar(activity$tec)))
  activity$exporter <- stringr::str_replace(activity$node, 'R14_', '')
  activity$energy <- sub("\\_.*", "", activity$tec)
  activity$year <- activity$year_all
  activity$scenario <- scenario
  activity$scenario_name <- scenario_name
  
  activity <- activity[c('scenario', 'scenario_name', 'exporter', 'importer', 'energy', 'year', 'value')]
  
  return(activity)
}

# Import gdx relation
import_gdx_relation <- function(scenario, version, scenario_name) {
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'REL')
  
  df <- subset(df, relation == 'PE_total_direct')
  
  df$year <- as.numeric(df$field)
  df$region <- sub('R14_', '', df$node)
  df$total_energy <- df$value
  df$scenario <- scenario
  
  df <- df[c('region', 'year', 'total_energy', 'scenario')]
  
  return(df)
}

# Combine scenarios: activity
baseline <- import_gdx_activity(scenario = 'baseline', scenario_name = 'Baseline', version = 5)

tariff_high <- import_gdx_activity(scenario = 'tariff_high', scenario_name = 'High Tariff', version = 4) 
tariff_low <- import_gdx_activity(scenario = 'tariff_low', scenario_name = 'Low Tariff', version = 4)

CO2_baseline <- import_gdx_activity(scenario = 'CO2_tax_baseline', scenario_name = 'CO2 Tax- Baseline', version = 2) 
CO2_tariff_high <- import_gdx_activity(scenario = 'CO2_tax_tariff_high', scenario_name = 'CO2 Tax- High Tariff', version = 2) 
CO2_tariff_low <- import_gdx_activity(scenario = 'CO2_tax_tariff_low', scenario_name = 'CO2 Tax- Low Tariff', version = 2) 

activity <- rbind(baseline, 
                  tariff_high, tariff_low, 
                  CO2_baseline, CO2_tariff_high, CO2_tariff_low)

# Combine scenarios: relation
baseline <- import_gdx_relation(scenario = 'baseline', scenario_name = 'Baseline', version = 5)

tariff_high <- import_gdx_relation(scenario = 'tariff_high', scenario_name = 'High Tariff', version = 4) 
tariff_low <- import_gdx_relation(scenario = 'tariff_low', scenario_name = 'Low Tariff', version = 4)

CO2_baseline <- import_gdx_relation(scenario = 'CO2_tax_baseline', scenario_name = 'CO2 Tax: Baseline', version = 2) 
CO2_tariff_high <- import_gdx_relation(scenario = 'CO2_tax_tariff_high', scenario_name = 'CO2 Tax: High Tariff', version = 2) 
CO2_tariff_low <- import_gdx_relation(scenario = 'CO2_tax_tariff_low', scenario_name = 'CO2 Tax: Low Tariff', version = 2) 

relation <- rbind(baseline, 
                  tariff_high, tariff_low, 
                  CO2_baseline, CO2_tariff_high, CO2_tariff_low)

rm(list = c('baseline', 'tariff_high', 'tariff_low', 'CO2_baseline', 'CO2_tariff_high', 'CO2_tariff_low'))

# Clean activity
activity <- subset(activity, year <= 2050)

imports <- group_by(activity, scenario, scenario_name, importer, exporter, year) %>% summarize(imports = sum(value, na.rm = T))

# Clean relation
relation <- subset(relation, year <= 2050)

# Combine files
plotdf <- left_join(relation, imports, by = c('year', 'region' = 'importer', 'scenario'))
isid('plotdf', c('region', 'exporter', 'year', 'scenario'))

# Convert to EJ
plotdf$imports <- plotdf$imports * gwa_to_ej
plotdf$total_energy <- plotdf$total_energy * gwa_to_ej

# Share of energy
plotdf$dependence <- plotdf$imports/plotdf$total_energy

# Remove CAS, SCS
plotdf <- subset(plotdf, !(region %in% c('CAS', 'SCS', 'UBM')) &
                   !(exporter %in% c('CAS', 'SCS', 'UBM')))

# Plot by scenario
plot_dependence <- function(scenario.name) {
  plot <- 
    ggplot(data = subset(plotdf, scenario_name == scenario.name & year == 2050),
           aes(x = region, y = dependence*100, fill = exporter)) +
    geom_bar(stat = 'identity', colour = 'black') + 
    scale_fill_brewer(palette = 'Spectral') +
    labs(y = 'Import dependence (%)', x = 'Region', fill = 'Exporter',
         title = scenario.name) +
    lims(y = c(0, 100))

  return(plot)
}

baseline_plot <- plot_dependence('Baseline')
co2tax_plot <- plot_dependence('CO2 Tax- Baseline')

ggsave(file.path(figures, 'import_dependence_baseline.pdf'), baseline_plot,
       device = 'pdf', units = 'in', height = 6, width = 6)
ggsave(file.path(figures, 'import_dependence_co2tax.pdf'), co2tax_plot,
       device = 'pdf', units = 'in', height = 6, width = 6)
