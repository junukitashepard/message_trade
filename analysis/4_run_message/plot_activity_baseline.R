###############################
# Post process MESSAGE output #
# Variable = ACT #
# GLOBAL SCHEMA (baseline) #
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
activity <- read_MESSAGE(msg_version = 71, msg_variable = 'ACT') # Baseline (global schema)

activity$vintage <- as.numeric(activity$vintage)
activity$year_all <- as.numeric(activity$year_all)

plot_global <- function(energy, trade) {
  
  assign('df', activity)
  
  df <- subset(df, grepl(paste0(energy, '_', trade), tec))
  
  if (energy == 'oil') {
    df <- subset(df, substr(tec, 1, 3) == 'oil')
  }
  
  df <- subset(df, as.numeric(year_all) > 2015)
  
  df$node <- substr(df$node, 5, 7)
  
  # Plot
  assign('regions', unique(df$node))
  
  for (r in regions) {
    plot <- ggplot(aes(x = year_all, y = value), 
                   data = subset(df, node == r)) + 
            geom_bar(stat = 'identity') + 
            labs(x = 'Year', y = paste0(toupper(substr(energy, 1, 1)), substr(energy, 2, nchar(energy)), 
                                        ' ', trade, 'orts (GWa)'), title = paste0('Region: ', r)) + 
            theme(text = element_text(size = 16))
    assign(paste0('plot.', r), ggplotGrob(plot))
  }
  
  assign('plotlist', mget(ls(pattern = 'plot.')))

  assign('allplots', gridExtra::arrangeGrob(grobs = plotlist))
  return(allplots)
}

# Run programs
oil_imports <- plot_global(energy = 'oil', trade = 'imp')
oil_exports <- plot_global(energy = 'oil', trade = 'exp')


