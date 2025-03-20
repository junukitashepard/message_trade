############################
# Chord diagrams for paper #
############################
library('shiny')
library('ggplot2')
library('chorddiag')

source('config_post_process.R')

figures <- paste0(repo, 'figures/unformatted')

baseline <- read_MESSAGE(msg_scenario = 'baseline', msg_version = 5, msg_variable = 'ACT')
co2_tax <- read_MESSAGE(msg_scenario = 'co2_tax_baseline', msg_version = 2, msg_variable = 'ACT')

# Function: plot chord diagram
collapse_activity <- function(indata, scenario_name) {
  
  environment(isid) <- environment()
  
  assign('df', indata)
  
  df <- subset(df, as.numeric(year_all) > 2015)
  df <- subset(df, grepl('_exp_', tec))
  
  df$importer <- toupper(substr(df$tec, nchar(df$tec) - 2, nchar(df$tec)))
  df$exporter <- substr(df$node, 5, 7)
  df$energy <- sub('_exp_.*', '', df$tec)
  
  df <- subset(df, energy != 'gas') # exclude gas
  isid('df', c('exporter', 'importer', 'year_all', 'energy'))
  

  df <- subset(df, !(exporter %in% c('GLB', 'CAS', 'SCS')) &
                     !(importer %in% c('GLB', 'CAS', 'SCS')))
  
  df <- group_by(df, year_all, energy) %>% summarize(value = sum(value, na.rm = T))
  
  df$value <- df$value *  (8760*3600*(10^-9)) # convert to EJ
  
  df$scenario <- scenario_name
  
  return(df)
}

# Collapse to scenario and energy
plotdf <- rbind(as.data.frame(collapse_activity(baseline, 'Baseline')), 
                as.data.frame(collapse_activity(co2_tax, 'CO2 Tax')))

# Plot activity over time
plotdf$year <- as.numeric(plotdf$year_all)
plotdf <- subset(plotdf, year <= 2050)

plot <- 
ggplot(aes(x = year, y = value, color = energy), data = plotdf) +
  geom_point() +
  geom_line() +
  facet_grid(.~scenario) + 
  labs(x = '', y = 'Size of network (EJ)', color = '') +
  theme(text = element_text(size = 15))

ggsave(file.path(figures, 'size_of_networks.pdf'), plot = plot,
       units = 'in', width = 7, height = 5, device = 'pdf')


