###############################################
# Compile data to create chart with emissions #
###############################################
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
import_gdx_emissions <- function(scenario, version, scenario.name) {
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'EMISS')
  
  df <- subset(df, node == 'World' & emission == 'TCE' & type_tec == 'all')
  df$scenario <- scenario.name
  
  df <- df[c('scenario', 'field', 'value')]
  names(df) <- c('scenario', 'year', 'emissions_TCE')
  
  return(df)
}

baseline_EMISS <- import_gdx_emissions(scenario = 'baseline', version = 10, scenario.name = 'Baseline')
  baseline_EMISS <- baseline_EMISS[c('year', 'emissions_TCE')]
  names(baseline_EMISS) <- c('year', 'baseline_emissions')
  
tariff_high_EMISS <- import_gdx_emissions(scenario = 'tariff_high', version = 8, scenario.name = 'High tariff')
tariff_low_EMISS <- import_gdx_emissions(scenario = 'tariff_low', version = 4, scenario.name = 'Low tariff')

CO2_bound_EMISS <- import_gdx_emissions(scenario = 'CO2_bound', version = 6, scenario.name = 'CO2 bound')

sanction_NAM_CPA_EMISS <- import_gdx_emissions(scenario = 'NAM_CPA_sanction', version = 5, scenario.name = 'NAM-CPA sanctions') 
sanction_CPA_PAO_EMISS <- import_gdx_emissions(scenario = 'CPA_PAO_sanction', version = 3, scenario.name = 'CPA-PAO sanctions') 
sanction_NAM_MEA_EMISS <- import_gdx_emissions(scenario = 'NAM_MEA_sanction', version = 3, scenario.name = 'NAM-MEA sanctions') 

alldf <- rbind(tariff_high_EMISS, tariff_low_EMISS, 
 #              CO2_bound_EMISS, 
               sanction_NAM_CPA_EMISS, sanction_CPA_PAO_EMISS, sanction_NAM_MEA_EMISS)

alldf <- left_join(alldf, baseline_EMISS, by = c('year'))

alldf$diff_baseline <- ((alldf$emissions_TCE - alldf$baseline_emissions)/alldf$baseline_emissions)*100

# Plot emissions
ggplot(aes(x = year, y = diff_baseline, colour = scenario, group = scenario), data = alldf) + 
  geom_point() +
  geom_line(size = 1) + 
  labs(x = "Year", y = "Difference from baseline (%)", colour = 'Scenario') + 
  theme(text = element_text(size = 15), legend.position = 'bottom')

