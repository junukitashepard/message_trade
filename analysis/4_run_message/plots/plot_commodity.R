#####################
# Diversity indices #
#####################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
msg_dir <- "C:/ProgramData/Anaconda3/Lib/site-packages/message_ix/model/output"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('NISTunits')

input <- paste0(wd, "output/derived/nodes")
output <- paste0(wd, "output/analysis/message")

energy_list <- c('crudeoil', 'lightoil', 'fueloil', 'coal', 'LNG')
regions_list <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Import gdx activity
import_gdx_cost <- function(scenario, version, scenario_name) {
  
  cost <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'PRICE_COMMODITY')
  names(cost) <- c('node', 'commodity', 'level', 'year', 'field', 'price')
  
  # Keep levels of imports
  cost <- subset(cost, sub('_.*', '', commodity) %in% energy_list & 
                       sub('.*_', '', commodity) %in% regions_list)
  
  cost <- cost[c('node', 'commodity', 'year', 'price')]
  
  cost$year <- as.numeric(cost$year)
  cost$scenario <- scenario
  cost$scenario_name <- scenario_name
  
  cost$energy_name[grepl('crudeoil', cost$commodity)] <- 'Crude Oil'
  cost$energy_name[grepl('fueloil', cost$commodity)] <- 'Fuel Oil'
  cost$energy_name[grepl('lightoil', cost$commodity)] <- 'Light Oil'
  cost$energy_name[grepl('coal', cost$commodity)] <- 'Coal'
  cost$energy_name[grepl('LNG', cost$commodity)] <- 'LNG'
  
  return(cost)
}

baseline_COST <- import_gdx_cost(scenario = 'baseline', scenario_name = 'Baseline', version = 16)

tariff_high_COST <- import_gdx_cost(scenario = 'tariff_high', scenario_name = 'High Tariff', version = 17) 
tariff_low_COST <- import_gdx_cost(scenario = 'tariff_low', scenario_name = 'Low Tariff', version = 11)

CO2_baseline_COST <- import_gdx_cost(scenario = 'CO2_tax_baseline', scenario_name = 'CO2 Tax: Baseline', version = 27) 
CO2_tariff_high_COST <- import_gdx_cost(scenario = 'CO2_tax_tariff_high', scenario_name = 'CO2 Tax: High Tariff', version = 11) 
CO2_tariff_low_COST <- import_gdx_cost(scenario = 'CO2_tax_tariff_low', scenario_name = 'CO2 Tax: Low Tariff', version = 7) 

sanction_NAM_MEA_COST <- import_gdx_cost(scenario = 'NAM_MEA_sanction', scenario_name = 'Sanction: NAM-MEA', version = 9)


cost <- rbind(tariff_high_COST, tariff_low_COST, 
              sanction_NAM_MEA_COST,
              CO2_baseline_COST, CO2_tariff_high_COST, CO2_tariff_low_COST)

baseline_COST$price_baseline <- baseline_COST$price

cost <- left_join(cost, baseline_COST[c('node', 'year', 'commodity', 'price_baseline')], by = c('node', 'year', 'commodity'))
cost$diff_baseline <- ((cost$price-cost$price_baseline)/cost$price_baseline)*100

# Node
cost$node <- toupper(sub('.*_', '', cost$commodity))

ggplot(aes(x = year, y = diff_baseline, colour = scenario_name), data = subset(cost, grepl('CO2', scenario) == F & node == 'NAM' & 
                                                                                 (energy_name == 'Crude Oil' | energy_name == 'Light Oil'))) +
  geom_point() +
  geom_line(size = 1) +
  facet_grid(.~energy_name) +
  theme(legend.position = 'bottom', text = element_text(size = 16)) +
  labs(x = 'Year', y = 'Difference from baseline commodity price ($M/GWa)', colour = 'Scenario', title = 'Region: NAM') +
  scale_color_brewer(palette = 'Set2')

  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = 'bottom') +
  labs(x = 'Year', y = 'Difference from baseline nodal cost (%)', fill = 'Scenario')
