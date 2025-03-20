###############################################
# Compile data to create chart with emissions #
###############################################
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
library('gridExtra')
library('grid')
library('RColorBrewer')

input <- paste0(wd, "output/derived/nodes")
output <- paste0(wd, "output/analysis/message")

regions <- c('afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 'pas', 'rus', 'sas', 'scs', 'ubm', 'weu')

# Import gdx activity
import_gdx_activity_imports <- function(scenario, version, scenario.name) {
  activity <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'ACT')
  
  activity$vintage <- as.numeric(activity$vintage)
  activity$year_all <- as.numeric(activity$year_all)
  
  activity <- subset(activity, grepl('_imp', tec) & tec != 'elec_imp' & tec != 'eth_imp' & tec != 'lh2_imp' & tec != 'meth_imp')
  
  activity$importer <- stringr::str_replace(activity$node, 'R14_', '')
  activity$energy <- sub("\\_.*", "", activity$tec)
  activity$year <- activity$year_all
  activity$scenario <- scenario
  activity$scenario.name <- scenario.name
  
  activity <- unique(activity[c('scenario', 'scenario.name', 'importer', 'energy', 'year', 'value')])
  
  activity <- subset(activity, value > 1)
  return(activity)
}

# Import gdx emission factor
add_emission_factor <- function(activity.df, scenario, version) {
  
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'emission_factor')
  
  df <- subset(df, field == 'CO2')
  df <- subset(df, grepl('_imp', tec))
  df <- subset(df, tec != 'elec_imp' & tec != 'eth_imp' & tec != 'lh2_imp' & tec != 'meth_imp' & tec != 'gas_imp')
  
  df$importer <- stringr::str_replace(df$node, 'R14_', '')
  df$energy <- sub("\\_.*", "", df$tec)
  
  df <- unique(df[c('importer', 'energy', 'value')])
  names(df) <- c('importer', 'energy', 'emission_factor')

  activity.df <- left_join(activity.df, df, by = c('importer', 'energy'))
  
  activity.df$CO2_emissions <- activity.df$value * activity.df$emission_factor
  
  return(activity.df)
}

# Import gdx emissions (all)
add_all_emissions <- function(activity.df, scenario, version) {
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'EMISS')
  
  df <- subset(df, emission == 'TCE' & type_tec == 'all')
  df$importer <- sub('.*\\_', '', df$node)
  df$scenario <- scenario
  
  df <- df[c('scenario', 'importer', 'field', 'value')]
  names(df) <- c('scenario', 'importer', 'year', 'all_emissions_TCE')
  df$year <- as.numeric(df$year)
  
  activity.df <- left_join(activity.df, df, by = c('scenario', 'importer', 'year'))

  return(activity.df)
}


# Import files, estimate emissions
baseline <- import_gdx_activity_imports(scenario = 'baseline', version = 16, scenario.name = 'Baseline') %>%
            add_emission_factor(scenario = 'baseline', version = 16) %>%
            add_all_emissions(scenario = 'baseline', version = 16)

tariff_low <- import_gdx_activity_imports(scenario = 'tariff_low', version = 11, scenario.name = 'Tariff: Low') %>%
              add_emission_factor(scenario = 'tariff_low', version = 11) %>%
              add_all_emissions(scenario = 'tariff_low', version = 11)

tariff_high <- import_gdx_activity_imports(scenario = 'tariff_high', version = 17, scenario.name = 'Tariff: High') %>%
               add_emission_factor(scenario = 'tariff_high', version = 17) %>%
               add_all_emissions(scenario = 'tariff_high', version = 17)

CO2_baseline <- import_gdx_activity_imports(scenario = 'CO2_tax_baseline', version = 27, scenario.name = 'CO2 Tax: Baseline') %>%
                add_emission_factor(scenario = 'CO2_tax_baseline', version = 27) %>%
                add_all_emissions(scenario = 'CO2_tax_baseline', version = 27)

CO2_tariff_low <- import_gdx_activity_imports(scenario = 'CO2_tax_tariff_low', version = 7, scenario.name = 'CO2 Tax: Low Tariffs') %>%
                  add_emission_factor(scenario = 'CO2_tax_tariff_low', version = 7) %>%
                  add_all_emissions(scenario = 'CO2_tax_tariff_low', version = 7)

CO2_tariff_high <- import_gdx_activity_imports(scenario = 'CO2_tax_tariff_high', version = 11, scenario.name = 'CO2 Tax: High Tariffs') %>%
                   add_emission_factor(scenario = 'CO2_tax_tariff_high', version = 11) %>%
                   add_all_emissions(scenario = 'CO2_tax_tariff_high', version = 11)

sanction_MEA_NAM <- import_gdx_activity_imports(scenario = 'NAM_MEA_sanction', version = 9, scenario.name = 'Sanction: NAM-MEA') %>%
                    add_emission_factor(scenario = 'NAM_MEA_sanction', version = 9) %>%
                    add_all_emissions(scenario = 'NAM_MEA_sanction', version = 9)

alldf <- rbind(baseline, 
               tariff_high, tariff_low,
               CO2_baseline, CO2_tariff_low, CO2_tariff_high)


# Plot difference from baseline #
#################################
plotdf <- subset(alldf, scenario != 'baseline')

basedf <- subset(alldf, scenario == 'baseline')[c('importer', 'year', 'energy', 'CO2_emissions', 'all_emissions_TCE')]
  names(basedf) <- c('importer', 'year', 'energy', 'CO2_emissions_baseline', 'all_emissions_TCE_baseline')

plotdf <- left_join(plotdf, basedf, by = c('importer', 'energy', 'year'))
  
plotdf$diff_CO2_emissions <- ((plotdf$CO2_emissions - plotdf$CO2_emissions_baseline)/plotdf$CO2_emissions)

plotdf <- subset(plotdf, year < 2055)

# Region labels
region_names <- c('Africa', 'Central Asian States', 'Centrally-Planned Asia', 'Eastern Europe', 'Latin America',
                  'Middle East', 'North America', 'Pacific OECD', 'Pacific South', 'Russia', 'Southern Asia', 
                  'Southern Caucasus', 'Belarus, Moldova, Ukraine', 'Western Europe')

for (r in regions) {
  plotdf$region_name[plotdf$importer == toupper(r)] <- region_names[which(regions == r)]
}

# Energy labels
plotdf$energy_name[plotdf$energy == 'coal'] <- 'Coal'
plotdf$energy_name[plotdf$energy == 'foil'] <- 'Fuel Oil'
plotdf$energy_name[plotdf$energy == 'loil'] <- 'Light Oil'
plotdf$energy_name[plotdf$energy == 'oil'] <- 'Crude Oil'
plotdf$energy_name[plotdf$energy == 'LNG'] <- 'LNG'

plotdf <- subset(plotdf, energy != 'NA')

# Plot by energy #
##################
plot_emissions_byenergy <- function(indf, plot.scenario, include.legend = 'bottom', fontsize = 15) {
  
  df <- subset(indf, scenario == plot.scenario)
  
  plot.title <- unique(indf$scenario.name[indf$scenario == plot.scenario])
  
  plot <-
    ggplot(data = subset(df, diff_CO2_emissions != 0),
           aes(x = year, y = diff_CO2_emissions, colour = region_name)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_grid(.~energy_name) +
    labs(x = '', y = '% difference from baseline imported emissions', colour = 'Region',
         title = plot.title) +
    theme(text = element_text(size = fontsize), legend.position = include.legend) 
  
  return(plot)
}

plot.high_tariff <- plot_emissions_byenergy(plotdf, 'tariff_high')
plot.low_tariff <- plot_emissions_byenergy(plotdf, 'tariff_low')

# Plot overall #
################
# Must add colors to palette
colpal <- colorRampPalette(brewer.pal(name = 'Dark2', n = 8))(14)

# Function: Plot overall emissions
plot_emissions_overall <- function(indf, plot.scenario, plot.variable, plot.y_title,
                                   include.legend = 'bottom', fontsize = 15) {
  
  df <- subset(indf, scenario == plot.scenario)

  df <- group_by(df, importer, region_name, year) %>% 
        summarize(CO2_emissions = sum(CO2_emissions, na.rm = T),
                  all_emissions_TCE = sum(all_emissions_TCE,na.rm = T),
                  CO2_emissions_baseline = sum(CO2_emissions_baseline, na.rm = T),
                  all_emissions_TCE_baseline = sum(all_emissions_TCE_baseline, na.rm = T))  
  
  df$diff_CO2_emissions <- ((df$CO2_emissions - df$CO2_emissions_baseline)/df$CO2_emissions_baseline)*100
  df$diff_all_emissions <- ((df$all_emissions_TCE - df$all_emissions_TCE_baseline)/df$all_emissions_TCE_baseline) *100
  
  plot.title <- unique(indf$scenario.name[indf$scenario == plot.scenario])
  
  df[, 'yvar'] <- df[, plot.variable]
  
  df$yvar[df$importer == 'UBM'&df$year == 2040] <- NA
  
  plot <-
    ggplot(data = subset(df, yvar != 0),
           aes(x = year, y = yvar, colour = region_name)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    labs(x = '', y = plot.y_title, colour = 'Region',
         title = plot.title) +
    theme(text = element_text(size = fontsize), legend.position = include.legend) +
    scale_color_manual(values = colpal)
  
  return(plot)
}

# Imported emissions
plot_emissions_overall(indf = plotdf, 
                       plot.scenario = 'tariff_low', 
                       plot.variable = 'diff_CO2_emissions',
                       plot.y_title = '% difference from baseline imported emissions')

# All emissions
plot_emissions_overall(indf = plotdf, 
                       plot.scenario = 'tariff_low', 
                       plot.variable = 'diff_all_emissions',
                       plot.y_title = '% difference from all baseline emissions')


# For World
isid('plotdf', c('importer', 'scenario', 'energy', 'year'))
global_df <- group_by(plotdf, scenario, scenario.name, year) %>% 
             summarize(CO2_emissions = sum(CO2_emissions, na.rm = T),
                       all_emissions = sum(all_emissions_TCE, na.rm = T))
base_global <- group_by(basedf, year) %>%
               summarize(CO2_emissions_baseline = sum(CO2_emissions_baseline, na.rm = T),
                         all_emissions_baseline = sum(all_emissions_TCE_baseline, na.rm = T))
global_df <- left_join(global_df, base_global, by = c('year'))

global_df$diff_baseline <- ((global_df$CO2_emissions-global_df$CO2_emissions_baseline)*100)/global_df$CO2_emissions_baseline
global_df$diff_overall <- ((global_df$all_emissions-global_df$all_emissions_baseline)*100)/global_df$all_emissions_baseline
global_df$share_CO2 <- (global_df$CO2_emissions/global_df$all_emissions) * 100

global_plot <-
  ggplot(aes(x = year, y = diff_baseline, colour = scenario.name, group = scenario), data = global_df) + 
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "", y = '% difference from baseline', 
       colour = 'Scenario', title = "Emissions linked to fuel imports") + 
  theme(text = element_text(size = 15), legend.position = 'none') +
  scale_colour_brewer(palette = 'Set1')

overall_plot <-
  ggplot(aes(x = year, y = diff_overall, colour = scenario.name, group = scenario), data = global_df) + 
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "", y = '% difference from baselines', 
       colour = 'Scenario', title = "Overall emissions") + 
  theme(text = element_text(size = 15), legend.position = 'none') +
  scale_colour_brewer(palette = 'Set1')

# By region (top three importers)
top3 <- group_by(baseline, importer) %>% summarise(value = sum(value))
top3 <- arrange(top3, desc(value))$importer[1:3]

setup_region <- function() {
  
  regdf <- group_by(plotdf, importer, scenario, scenario.name, year) %>% 
    summarize(CO2_emissions = sum(CO2_emissions, na.rm = T),
              all_emissions = sum(all_emissions_TCE, na.rm = T))
  basereg <- group_by(basedf, importer, year) %>%
              summarize(CO2_emissions_baseline = sum(CO2_emissions_baseline, na.rm = T),
              all_emissions_baseline = sum(all_emissions_TCE_baseline, na.rm = T))
  
  regdf <- left_join(regdf, basereg, by = c('year', 'importer'))
  
  regdf$diff_baseline <- ((regdf$CO2_emissions-regdf$CO2_emissions_baseline)*100)/regdf$CO2_emissions_baseline
  regdf$diff_overall <- ((regdf$all_emissions-regdf$all_emissions_baseline)*100)/regdf$all_emissions_baseline
  regdf$share_CO2 <- (regdf$CO2_emissions/regdf$all_emissions) * 100

  return(regdf)
}

plot_imported_emissions <- function(region) {
  import_plot <-
    ggplot(aes(x = year, y = diff_baseline, colour = scenario.name, group = scenario), 
           data = subset(region_df, importer == region)) + 
    geom_point(size = 2) +
    geom_line(size = 1) + 
    labs(x = "", y = '% difference from baseline', 
         colour = 'Scenario', title = "Emissions linked to fuel imports") + 
    theme(text = element_text(size = 15), legend.position = 'none') +
    scale_colour_brewer(palette = 'Set1')
  
  return(import_plot)
}

plot_overall_emissions <- function(region) {
  overall_plot <-
    ggplot(aes(x = year, y = diff_overall, colour = scenario.name, group = scenario), 
           data = subset(region_df, importer == region)) + 
    geom_point(size = 2) +
    geom_line(size = 1) + 
    labs(x = "", y = '% difference from baselines', 
         colour = 'Scenario', title = "Overall emissions") + 
    theme(text = element_text(size = 15), legend.position = 'none') +
    scale_colour_brewer(palette = 'Set1')
  return(overall_plot)
}

region_df <- setup_region()
plot_imported_emissions('LAM')
plot_overall_emissions('LAM')

