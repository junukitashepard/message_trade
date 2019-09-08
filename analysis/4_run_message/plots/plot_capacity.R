###############################
# Post process MESSAGE output #
# Variable = CAP #
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
capacity <- read_MESSAGE(msg_scenario = 'shipping_CLT', msg_version = 1, msg_variable = 'CAP_NEW') # Baseline (global schema)

capacity$field <- as.numeric(capacity$field)

capacity <- subset(capacity, grepl('shipping', tec))[c('node', 'tec', 'field', 'value')]
capacity <- unique(capacity)

capacity$fuel <- sub('.*\\_', '', capacity$tec)
capacity$fuel <- paste0(toupper(substr(capacity$fuel, 1, 1)), substr(capacity$fuel, 2, nchar(capacity$fuel)))
capacity$fuel[capacity$fuel == 'Elec'] <- 'Electricity'

capacity$shipping <- sub('\\_.*', '', capacity$tec)

plot_capacity <- function(shipping_type, y_axis_text) {
  
  assign('df', capacity)
  
  df <- subset(df, grepl(shipping_type, shipping))
  
  df <- subset(df, as.numeric(field) > 2015)
  
  # Plot
  assign('regions', unique(df$node))
  
  plot <-
  ggplot(aes(x = field, y = value, fill = node), data = df) +
    geom_bar(stat = 'identity') +
    facet_grid(~fuel) + 
    labs(x = 'Year', y = y_axis_text, fill = 'Region') +
    theme(legend.position = 'bottom',
          text = element_text(size = 18))
  
  return(plot)
}

# Plot new capacity
liquid_shipping <- plot_capacity('liquid', 'New liquid shipping capacity, diesel (bton-km)')
solid_shipping <- plot_capacity('solid', 'New solid shipping capacity, diesel (bton-km)')
LNG_shipping <- plot_capacity('LNG', 'New LNG shipping capacity, diesel (bton-km)')