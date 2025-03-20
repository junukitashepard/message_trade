############################
# Chord diagrams for paper #
############################
library('shiny')
library('ggplot2')
library('chorddiag')

source('config_post_process.R')

figures <- paste0(repo, 'figures/chord_diagrams')

baseline <- read_MESSAGE(msg_scenario = 'baseline', msg_version = 5, msg_variable = 'ACT')
co2_tax <- read_MESSAGE(msg_scenario = 'co2_tax_baseline', msg_version = 2, msg_variable = 'ACT')

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

  print(paste0("Total ", plot.energy, ": ", sum(plotdf$value)))
  
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

for (e in c('all', 'oil', 'coal', 'eth', 'foil', 'loil', 'LNG')) {
  plot_and_save(e)
}