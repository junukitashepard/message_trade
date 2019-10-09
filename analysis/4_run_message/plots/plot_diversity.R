#####################
# Diversity indices #
#####################
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
import_gdx_activity <- function(scenario, version, scenario_name) {
  activity <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'ACT')
  
  activity$vintage <- as.numeric(activity$vintage)
  activity$year_all <- as.numeric(activity$year_all)
  
  activity <- subset(activity, grepl('_exp_', tec) & grepl('gas_exp', tec) == F)
  activity <- subset(activity, grepl('_glb', tec) == F)
  
  activity$importer <- toupper(substr(activity$tec, nchar(activity$tec)-2, nchar(activity$tec)))
  activity$exporter <- stringr::str_replace(activity$node, 'R14_', '')
  activity$energy <- sub("\\_.*", "", activity$tec)
  activity$year <- activity$year_all
  activity$scenario <- scenario
  activity$scenario_name <- scenario_name
  
  activity <- activity[c('scenario', 'scenario_name', 'exporter', 'importer', 'energy', 'year', 'value')]
  
  return(activity)
}

baseline_ACT <- import_gdx_activity(scenario = 'baseline', scenario_name = 'Baseline', version = 16)

tariff_high_ACT <- import_gdx_activity(scenario = 'tariff_high', scenario_name = 'High Tariff', version = 17) 
tariff_low_ACT <- import_gdx_activity(scenario = 'tariff_low', scenario_name = 'Low Tariff', version = 11)

CO2_baseline_ACT <- import_gdx_activity(scenario = 'CO2_tax_baseline', scenario_name = 'CO2 Tax: Baseline', version = 27) 
CO2_tariff_high_ACT <- import_gdx_activity(scenario = 'CO2_tax_tariff_high', scenario_name = 'CO2 Tax: High Tariff', version = 11) 
CO2_tariff_low_ACT <- import_gdx_activity(scenario = 'CO2_tax_tariff_low', scenario_name = 'CO2 Tax: Low Tariff', version = 7) 

activity <- rbind(baseline_ACT, 
                  tariff_high_ACT, tariff_low_ACT, 
                  CO2_baseline_ACT, CO2_tariff_high_ACT, CO2_tariff_low_ACT)

activity <- subset(activity, year <= 2050)

# Collapse by importer/exporter
collapse_trade <- function(trader) {
  df <- activity
  names(df)[names(df) == trader] <- 'trader'
  
  df <- group_by(df, scenario, scenario_name, trader, energy, year) %>% mutate(total = sum(value))
  df$hhi <- ((df$value/df$total)*100)^2
  
  df <- group_by(df, scenario, scenario_name, trader, energy, year, total) %>% summarise(hhi = sum(hhi))
  
  df$energy[df$energy == 'coal'] <- 'Coal'
  df$energy[df$energy == 'foil'] <- 'Fuel Oil'
  df$energy[df$energy == 'loil'] <- 'Light Oil'
  df$energy[df$energy == 'oil'] <- 'Crude Oil'
  
  names(df)[names(df) == 'trader'] <- trader
  return(df)
}

imports <- collapse_trade('importer')
exports <- collapse_trade('exporter')

graph_HHI <- function(trade, scenario.list, regions = 'all', 
                      energy.list, title.name = '', manually.colour = NULL, 
                      legend = 'bottom') {

  if (regions == 'all') {
    baseline_plot <- group_by(trade, scenario, scenario_name, energy, year) %>% 
                     summarise(hhi = weighted.mean(hhi, total))
    baseline_plot <- subset(baseline_plot, scenario %in% scenario.list & energy %in% energy.list)
  } else {
    baseline_plot <- trade
    names(baseline_plot)[names(baseline_plot) %in% c('importer', 'exporter')] <- 'trader'
    baseline_plot <- subset(baseline_plot, scenario %in% scenario.list & trader %in% regions & energy %in% energy.list)
  }
  
  if (length(energy.list) > 1) {
    baseline_plot <- group_by(baseline_plot, scenario, scenario_name, year) %>%
                     summarize(hhi = weighted.mean(hhi, total, na.rm = T))
  }
  
  plot <- 
  ggplot(aes(x = year, y = hhi, colour = scenario_name), data = baseline_plot) + 
    geom_point(size = 2) + 
    geom_line(size = 2) + 
    labs(x = 'Year', y = 'Mean HHI', colour = 'Scenario', title = title.name) + 
    theme(text = element_text(size = 15),
          legend.position = legend)  

  
  if (is.null(manually.colour)) {
    plot <- plot + scale_color_brewer(palette = 'Dark2')
  } else {
    plot <- plot + scale_color_manual(values = manually.colour)
  }
  
  return(plot)
}

# Top regions
energylist <-  c('Coal', 'Crude Oil', 'Fuel Oil', 'Light Oil', 'LNG')
scenariolist <-  c('baseline',
                   'tariff_high', 'tariff_low',
                   'CO2_tax_baseline', 'CO2_tax_tariff_high', 'CO2_tax_tariff_low')

top_exp <- group_by(baseline_ACT, exporter, year) %>% summarize(value = sum(value))
top_imp <- group_by(baseline_ACT, importer, year) %>% summarise(value = sum(value))

top_exp <- arrange(subset(top_exp, year == 2050), desc(value))
top_imp <- arrange(subset(top_imp, year == 2050), desc(value))


MEA_exports_plot <- graph_HHI(trade = exports, 
                              energy.list = energylist,
                              scenario.list  = scenariolist,
                              title.name = 'Energy exports from Middle East', 
                              legend = 'none', regions = 'MEA')

LAM_exports_plot <- graph_HHI(trade = exports, 
                              energy.list = energylist,
                              scenario.list  = scenariolist,
                              title.name = 'Energy exports from Latin America', 
                              legend = 'none', regions = 'LAM')

RUS_exports_plot <- graph_HHI(trade = exports, 
                              energy.list = energylist,
                              scenario.list  = scenariolist,
                              title.name = 'Energy exports from Russia', 
                              legend = 'none', regions = 'RUS')

PAS_exports_plot <- graph_HHI(trade = imports, 
                              energy.list = energylist,
                              scenario.list  = scenariolist,
                              title.name = 'Energy imports to Pacific Asia', 
                              legend = 'none', regions = 'PAS')

SAS_exports_plot <- graph_HHI(trade = imports, 
                              energy.list = energylist,
                              scenario.list  = scenariolist,
                              title.name = 'Energy imports to South Asia', 
                              legend = 'none', regions = 'SAS')

WEU_exports_plot <- graph_HHI(trade = imports, 
                              energy.list = energylist,
                              scenario.list  = scenariolist,
                              title.name = 'Energy imports to Western Europe', 
                              legend = 'none', regions = 'WEU')



# Plot importer share/exporter share over time
calculate_share <- function(trade) {
  df <- trade
  
  df <- group_by(df, scenario, energy, year) %>% mutate(global_trade = sum(total))
  df$share <- df$total/df$global_trade
  
  return(df)
  
}

imports <- calculate_share(imports)
exports <- calculate_share(exports)

# By region
plot_region <- function(trade, trader, graphname, region.name, region.title){

  plotdf <- trade
  names(plotdf)[names(plotdf) == trader] <- 'trader'
  
  plotdf <- subset(plotdf, trader == region.name)
  
  plot <- 
    ggplot(aes(x = year, y = share, colour = scenario_name), data = plotdf) +
    geom_point(size = 2) + 
    geom_line(size = 2) + 
    facet_grid(.~energy) + 
    labs(x = 'Year', y = graphname, colour = 'Scenario', title = region.title) + 
    theme(legend.position = 'bottom', text = element_text(size = 20)) +
    scale_color_brewer(palette = 'Dark2')
  
  return(plot)
  
}

plot_region(imports, 'importer', 'Share of imports', 'SAS', 'Southern Asia')
plot_region(imports, 'importer', 'Share of imports', 'NAM', 'North America')
plot_region(imports, 'importer', 'Share of imports', 'CPA', 'Centrally Planned Asia')

# By energy
region_list <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'RUS', 'SAS', 'UBM', 'WEU')
region_list <- expand.grid(region_list, year_act <- c(seq(2020, 2055, by = 5), seq(2060, 2110, by = 10)))

names(region_list) <- c('trader', 'year')

plot_energy <- function(trade, trader, energy.name, plot.title){
  
  plotdf <- trade
  names(plotdf)[names(plotdf) == trader] <- 'trader'
  plotdf <- subset(plotdf, energy == energy.name)
  
  plotdf <- group_by(plotdf, year, trader) %>% summarise(region_total = sum(total))
  plotdf <- group_by(plotdf, year) %>% mutate(total = sum(region_total))
  
  plotdf$share <- plotdf$region_total/plotdf$total
  
  plotdf <- suppressWarnings(left_join(region_list, plotdf, by = c('trader', 'year')))
  plotdf$share[is.na(plotdf$share)] <- 0
  
  plot <- 
    ggplot(aes(x = year, y = share, fill = trader), data = plotdf) +
    geom_area(colour = 'black') + 
    labs(x = 'Year', y = 'Share of global exports', fill = 'Region', title = plot.title) + 
    theme(legend.position = 'right', text = element_text(size = 20)) +
    scale_color_brewer(palette = 'Dark2')
  
  return(plot)
  
}

share_crude_imports <- plot_energy(imports, 'importer', 'Crude Oil', 'Share of crude oil imports')
share_crude_exports <- plot_energy(exports, 'exporter', 'Crude Oil', 'Share of crude oil exports')