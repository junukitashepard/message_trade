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

# Import gdx activity
import_gdx_cost <- function(scenario, version, scenario_name) {
  cost <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'COST_NODAL')
  names(cost) <- c('node', 'year', 'cost')
  cost$year <- as.numeric(cost$year)
  cost$scenario <- scenario
  cost$scenario_name <- scenario_name
  return(cost)
}

baseline_COST <- import_gdx_cost(scenario = 'baseline', scenario_name = 'Baseline', version = 15)

tariff_high_COST <- import_gdx_cost(scenario = 'tariff_high', scenario_name = 'High Tariff', version = 15) 
tariff_low_COST <- import_gdx_cost(scenario = 'tariff_low', scenario_name = 'Low Tariff', version = 9)

CO2_baseline_COST <- import_gdx_cost(scenario = 'CO2_tax_baseline', scenario_name = 'CO2 Tax: Baseline', version = 23) 
CO2_tariff_high_COST <- import_gdx_cost(scenario = 'CO2_tax_tariff_high', scenario_name = 'CO2 Tax: High Tariff', version = 8) 
CO2_tariff_low_COST <- import_gdx_cost(scenario = 'CO2_tax_tariff_low', scenario_name = 'CO2 Tax: Low Tariff', version = 5) 

sanction_NAM_CPA_COST <- import_gdx_cost(scenario = 'NAM_CPA_sanction', scenario_name = 'Sanction: NAM-CPA', version = 9)   
sanction_CPA_PAO_COST <- import_gdx_cost(scenario = 'CPA_PAO_sanction', scenario_name = 'Sanction: CPA-PAO', version = 7)  

sanction_NAM_MEA_COST <- import_gdx_cost(scenario = 'NAM_MEA_sanction', scenario_name = 'Sanction: NAM-MEA', version = 8)
sanction_NAM_MEA_COST_direct <- import_gdx_cost(scenario = 'NAM_MEA_sanction_onlydirect', scenario_name = 'Sanction, direct only: NAM-MEA', version = 1)

cost <- rbind(tariff_high_COST, tariff_low_COST, 
              sanction_NAM_CPA_COST,sanction_NAM_MEA_COST,
              sanction_NAM_MEA_COST_direct)

baseline_COST$cost_baseline <- baseline_COST$cost
cost <- left_join(cost, baseline_COST[c('node', 'year', 'cost_baseline')], by = c('node', 'year'))
cost$diff_baseline <- ((cost$cost-cost$cost_baseline)*100)/cost$cost_baseline

ggplot(aes(x = year, y = diff_baseline, fill = scenario_name), data = subset(cost, node == 'R14_PAO')) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = 'bottom') +
  labs(x = 'Year', y = 'Difference from baseline nodal cost (%)', fill = 'Scenario')
