###############################
# Post process MESSAGE output #
# Variable = ACT #
###############################
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

library('RColorBrewer')
ncolors <- 14
mycolors <- colorRampPalette(brewer.pal(8, 'Paired'))(ncolors)

# Plot GLOBAL baseline #
########################
activity <- read_MESSAGE(msg_scenario = 'baseline', msg_version = 16, msg_variable = 'ACT') # Baseline (global schema)

activity$vintage <- as.numeric(activity$vintage)
activity$year_all <- as.numeric(activity$year_all)

activity <- subset(activity, year_all <= 2050)

plot_activity <- function(energy, trade) {
  
  assign('df', activity)
  
  df <- subset(df, grepl(paste0(energy, '_exp_'), tec))
  if (energy == 'oil') {
    df <- subset(df, substr(tec, 1, 3) == energy)
  }
  
  df <- subset(df, as.numeric(year_all) > 2015)

  df$importer <- toupper(substr(df$tec, nchar(df$tec) - 2, nchar(df$tec)))
  df$exporter <- substr(df$node, 5, 7)

  if (trade == 'imp') {
    df$node1 <- df$importer
    df$node2 <- df$exporter
  } else {
    df$node1 <- df$exporter
    df$node2 <- df$importer
  }
  
  # Plot
  assign('regions', unique(df$node1))

  for (r in regions) {
    assign (paste0('plot.', r), 
            ggplot(aes(x = year_all, y = value, fill = node2), 
                   data = subset(df, node1 == r)) + 
              geom_bar(stat = 'identity') + 
              labs(x = 'Year', 
                   y = paste0(toupper(substr(energy, 1, 1)), substr(energy, 2, nchar(energy)),
                              ' ', trade, 'orts (GWa)'), 
                   fill = 'Trading region', 
                   title = paste0('Region: ', r)) +
              theme(legend.position = 'bottom') + 
              scale_fill_manual(values = mycolors))
  }
  assign('plotlist', mget(ls(pattern = 'plot.')))
  
  assign('allplots', gridExtra::arrangeGrob(grobs = plotlist))
  return(allplots)
}


exports_baseline <-  plot_activity(energy = 'coal', trade = 'exp')
exports_hightariff <- plot_activity(energy = 'coal', trade = 'exp')

# Oil trade
oil_exports <- plot_activity(energy = 'oil', trade = 'exp')
oil_imports <- plot_activity(energy = 'oil', trade = 'imp')

# Light oil trade
loil_exports <- plot_activity(energy = 'loil', trade = 'exp')
loil_imports <- plot_activity(energy = 'loil', trade = 'imp')

# Fuel oil trade
foil_exports <- plot_activity(energy = 'foil', trade = 'exp')
foil_imports <- plot_activity(energy = 'foil', trade = 'imp')

# LNG trade
LNG_exports <- plot_activity(energy = 'LNG', trade = 'exp')
LNG_imports <- plot_activity(energy = 'LNG', trade = 'imp')

# Coal trade
coal_exports <- plot_activity(energy = 'coal', trade = 'exp')
coal_imports <- plot_activity(energy = 'coal', trade = 'imp')