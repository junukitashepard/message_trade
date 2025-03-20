####################################
# Oil extraction rates by scenario #
####################################
rm(list = ls())
wd <- "C:/Users/jus3/message_trade"
repo <- "C:/Users/jus3/message_trade"
msg_dir <- "C:/Users/jus3/message_trade/gdx_files"
output <- paste0(repo, '/tables')
setwd(wd)

igdx('C:/GAMS/win64/29.1') # Set GDX path

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('openxlsx')

library('RColorBrewer')
ncolors <- 14
mycolors <- colorRampPalette(brewer.pal(8, 'Paired'))(ncolors)

# Conversion factor
gwa_to_ej <- 8760*3600*(10^-9)

# Import scenarios #
####################
add_scenario <- function(scenario_name, message_version) {
  
  df <- read_MESSAGE(msg_scenario = scenario_name, msg_version = message_version, msg_variable = 'ACT') # Baseline (global schema)
  
  df <- subset(df, tec %in% c('oil_extr_1', 'oil_extr_2', 'oil_extr_3', 'oil_extr_4', 'oil_extr_5',
                              'oil_extr_6', 'oil_extr_7'))
  
  df$country <- gsub('R14_', '', df$node)
  df$year <- df$year_all
  
  df <- group_by(df, country, year) %>% summarize(extraction = sum(value, na.rm = T))

  df$extraction <- df$extraction * gwa_to_ej
  names(df) <- c('country', 'year', scenario_name)
  
  return(df)
  
}

extractdf <- add_scenario('baseline', 5)

extractdf <- left_join(extractdf, add_scenario('tariff_high', 4), by = c('country', 'year'))


