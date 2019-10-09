###############################
# Post process MESSAGE output #
# Variable = ACT #
###############################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
msg_dir <- "C:/ProgramData/Anaconda3/Lib/site-packages/message_ix/model/output"

figures <- paste0(repo, 'figures')

setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('circlize')

library('RColorBrewer')


# Set colors
color_regions <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', 
                   '#ff7f00', '#ffff33', '#a65628', '#f781bf', 
                   '#999999', '#fb9a99', '#cab2d6')
names(color_regions) <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 
                          'PAO', 'PAS', 'RUS', 'SAS', 'WEU')


# Plot GLOBAL baseline #
########################
# Import files
baseline <- read_MESSAGE(msg_scenario = 'baseline', msg_version = 16, msg_variable = 'ACT') # Baseline (global schema)
baseline$vintage <- as.numeric(baseline$vintage)
baseline$year_all <- as.numeric(baseline$year_all)

co2_tax <- read_MESSAGE(msg_scenario = 'CO2_tax_baseline', msg_version = 27, msg_variable = 'ACT') # Baseline (global schema)
co2_tax$vintage <- as.numeric(co2_tax$vintage)
co2_tax$year_all <- as.numeric(co2_tax$year_all)

# Function: plot chord diagram
plot_activity <- function(indata, plot.energy, plot.year) {
  
  environment(isid) <- environment()
  
  assign('df', indata)
  
  df <- subset(df, as.numeric(year_all) > 2015)
  df <- subset(df, grepl('_exp_', tec))
  
  df$importer <- toupper(substr(df$tec, nchar(df$tec) - 2, nchar(df$tec)))
  df$exporter <- substr(df$node, 5, 7)
  df$energy <- sub('_exp_.*', '', df$tec)
  
  df <- subset(df, energy != 'gas') # exclude gas
  isid('df', c('exporter', 'importer', 'year_all', 'energy'))
  
  plotdf <- subset(df, year_all == plot.year)[c('exporter', 'importer', 'energy', 'value')]
  plotdf <- subset(plotdf, !(exporter %in% c('GLB', 'CAS', 'SCS')) &
                           !(importer %in% c('GLB', 'CAS', 'SCS')))

  plotdf$value <- plotdf$value *  (8760*3600*(10^-9)) # convert to EJ
  
  # Subset based on energy
  if (plot.energy == 'all') {
    plotdf <- group_by(plotdf, exporter, importer) %>% summarize(value = sum(value, na.rm = T))
  } else {
    plotdf <- subset(plotdf, energy == plot.energy)
    plotdf$energy <- NULL
  }

  par(cex = 2)
  chordDiagram(plotdf, grid.col = color_regions)
}

# Plot and compare
plot_and_save <- function(energytype) {

  pdf(paste0(figures, '/CD_baseline_', energytype, '_2050.pdf')) 
  
  plot_activity(indata = baseline,
                plot.energy = energytype,
                plot.year = 2050)
  dev.off()
  
  pdf(paste0(figures, '/CD_CO2_', energytype, '_2050.pdf')) 
  
  
  plot_activity(indata = co2_tax,
                plot.energy = energytype,
                plot.year = 2050)
  dev.off()
}

for (e in c('all', 'oil', 'coal', 'foil', 'loil', 'LNG')) {
  plot_and_save(e)
}

