###############################################
# Compile data to create chart with emissions #
###############################################
source('config_post_process.R')

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
library('ggsci')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('NISTunits')
library('gridExtra')
library('grid')
library('RColorBrewer')

igdx('C:/GAMS/win64/29.1') # Set GDX path

# Function to import and collapse activity to global level
import_sa_gdx <- function(scenario.name, scenario.version) {
  
  df <- read_MESSAGE(msg_scenario = scenario.name, msg_version = scenario.version, msg_variable = 'ACT')

  df$vintage <- as.numeric(df$vintage)
  df$year_all <- as.numeric(df$year_all)
  
  df <- subset(df, grepl('_imp', tec) & tec != 'elec_imp' & tec != 'gas_imp')
  
  df$importer <- stringr::str_replace(df$node, 'R14_', '')
  df$energy <- sub("\\_.*", "", df$tec)
  df$year <- df$year_all
  df$scenario <- scenario.name
  
  df <- unique(df[c('scenario', 'importer', 'energy', 'year', 'value')])
  
  df <- subset(df, value > 1)

  df$value <- df$value *  (8760*3600*(10^-9)) # convert to EJ
  
  df <- group_by(df, scenario, energy, year) %>% summarize(value = sum(value, na.rm = T))
  
  return(df)
}

co2_tax <- import_sa_gdx('CO2_tax_baseline', 2)
lowco2_tax <- import_sa_gdx('low_CO2_tax_baseline', 1)
highco2_tax <- import_sa_gdx('high_CO2_tax_baseline', 1)

plotdf <- rbind(co2_tax, lowco2_tax, highco2_tax)

plotdf <- subset(plotdf, year <= 2050)

oil_plot <- 
ggplot(aes(x = year, y = value, colour = energy, shape = scenario),
       data = subset(plotdf, energy %in% c('loil', 'oil'))) +
  geom_point(size = 3) +
  geom_line(size = 1) + 
  scale_color_npg() +
  labs(x = '', y = 'Size of trade network (EJ)', title = 'Sensitivity analysis: Emission tax levels')

plot <- 
  ggplot(aes(x = year, y = value, colour = energy, shape = scenario),
         data = subset(plotdf, !(energy %in% c('loil', 'oil')))) +
  geom_point(size = 3) +
  geom_line(size = 1) + 
  scale_color_aaas() +
  labs(x = '', y = 'Size of trade network (EJ)', title = 'Sensitivity analysis: Emission tax levels')

ggsave(file.path(figures, 'sensitivity_analysis_tax.pdf'), plot, device = 'pdf',
       units = 'in', width = 7, height = 6)
ggsave(file.path(figures, 'sensitivity_analysis_tax_oil.pdf'), oil_plot, device = 'pdf',
       units = 'in', width = 7, height = 6)
