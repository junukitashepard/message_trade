#######################################
# Graph total net exports by scenario #
#######################################
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
library('wesanderson')

input <- paste0(wd, "output/analysis/message")

# Import file
df <- readRDS(file.path(input, 'message_trade.rds'))

# Fix energy labels
df$energy[df$energy == 'coal'] <- 'Coal'
df$energy[df$energy == 'foil'] <- 'Fuel Oil'
df$energy[df$energy == 'loil'] <- 'Light Oil'
df$energy[df$energy == 'oil'] <- 'Crude Oil'

# Graph of total net exports
df <- unique(df[c('scenario', 'id', 'year', 'energy', 'net_exports')])

df <- subset(df, energy != 'dummy' & year < 2100)
df <- group_by(df, scenario, year, energy) %>% summarise(net_exports = sum(net_exports))

plot_net_exports <- function(scenario.name, scenario.title) {
  plot <- 
  ggplot(aes(x = year, y = net_exports, fill = energy), data = subset(df, scenario == scenario.name)) +
    geom_area(stat = 'identity', colour = 'black') +
    theme(text = element_text(size = 20)) +
    labs(x = '', y = 'Global net exports (GWa)', fill = 'Fossil fuel', title = scenario.title) +
    scale_fill_brewer(palette = 'Set1') + 
    ylim(0, 10000)
  
  return(plot)
}

plot_baseline <- plot_net_exports('baseline', 'Baseline (Median Tariffs)')
plot_tariff_high <- plot_net_exports('tariff_high', 'High Tariffs')
plot_tariff_low <- plot_net_exports('tariff_low', 'Low Tariffs')

plot_co2_baseline <- plot_net_exports('CO2_tax_baseline', 'Emissions Tax + Median Tariffs')
plot_co2_tariff_high <- plot_net_exports('CO2_tax_tariff_high', 'Emissions Tax + High Tariffs')
plot_co2_tariff_low <- plot_net_exports('CO2_tax_tariff_low', 'Emissions Tax + Low Tariffs')


