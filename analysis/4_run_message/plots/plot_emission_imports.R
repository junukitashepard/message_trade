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
               sanction_MEA_NAM,
               CO2_baseline, CO2_tariff_low, CO2_tariff_high)

# Plot emissions by region
alldf <- subset(alldf, year < 2100)

# Region labels
region_names <- c('Africa', 'Central Asian States', 'Centrally-Planned Asia', 'Eastern Europe', 'Latin America',
                  'Middle East', 'North America', 'Pacific OECD', 'Pacific South', 'Russia', 'Southern Asia', 
                  'Southern Caucasus', 'Belarus, Moldova, Ukraine', 'Western Europe')

# Energy labels
alldf$energy_name[alldf$energy == 'coal'] <- 'Coal'
alldf$energy_name[alldf$energy == 'foil'] <- 'Fuel Oil'
alldf$energy_name[alldf$energy == 'loil'] <- 'Light Oil'
alldf$energy_name[alldf$energy == 'oil'] <- 'Crude Oil'
alldf$energy_name[alldf$energy == 'LNG'] <- 'LNG'

# Convert units
alldf$CO2_emissions <- alldf$CO2_emissions * (10^6) * (10^-3) # convert to ton
alldf$all_emissions_TCE <- alldf$all_emissions_TCE * (10^6) * (10^-3)

# Separate by type of scenario (non-CO2 vs. CO2)
nonco2_df <- subset(alldf, grepl('CO2', scenario) == F)
co2_df <- subset(alldf, grepl('CO2', scenario))

# Function: plot emissions #
############################
plot_emissions <- function(indf, plot.region, include.legend = 'bottom', fontsize = 28) {
  
  plotdf <- subset(indf, importer == plot.region)
  plotdf <- subset(plotdf, !is.na(energy_name) & energy_name != 'NA')
  
  plotdf$share_CO2 <- (plotdf$CO2_emissions/plotdf$all_emissions_TCE) * 100
  
  title.region <- region_names[which(regions == tolower(plot.region))]
  
  assign(paste0('plot.', plot.region),
         
         ggplot(aes(x = year, y = share_CO2, colour = scenario.name, group = scenario), data = plotdf) + 
           geom_point(size = 2) +
           geom_line(size = 1) + 
           facet_grid(.~energy_name) +
           labs(x = "", y = '% of CO2 emissions from imports', colour = 'Scenario', title = title.region) + 
           theme(text = element_text(size = fontsize), legend.position = include.legend) +
           scale_colour_brewer(palette = 'Set1'),
         
         envir = parent.frame())
}

# Plot: non-CO2 policies #
##########################
# By region
for (r in c(regions)) {
  if (r %in% regions) {r <- toupper(r)}
  plot_emissions(alldf, r, fontsize = 18)
}

# For World
global_alldf <- group_by(alldf, scenario, scenario.name, year) %>% 
                summarize(CO2_emissions = sum(CO2_emissions, na.rm = T),
                          all_emissions = sum(all_emissions_TCE, na.rm = T))

global_alldf$share_CO2 <- (global_alldf$CO2_emissions/global_alldf$all_emissions) * 100

global_share <-
  ggplot(aes(x = year, y = share_CO2, colour = scenario.name, group = scenario), data = global_alldf) + 
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "", y = '% of CO2 emissions from imports', colour = 'Scenario', title = "World") + 
  theme(text = element_text(size = 15), legend.position = 'bottom') +
  scale_colour_brewer(palette = 'Set1')

global_plot <-
  ggplot(aes(x = year, y = CO2_emissions, colour = scenario.name, group = scenario), data = global_alldf) + 
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "", y = 'CO2 emissions from imports (t)', colour = 'Scenario', title = "World") + 
  theme(text = element_text(size = 15), legend.position = 'bottom') +
  scale_colour_brewer(palette = 'Set1')
