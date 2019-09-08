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
import_gdx_emissions <- function(scenario, version, scenario.name) {
  df <- read_MESSAGE(msg_scenario = scenario, msg_version = version, msg_variable = 'EMISS')
  
  df <- subset(df, emission == 'TCE' & type_tec == 'all')
  df$scenario <- scenario.name
  
  df <- df[c('scenario', 'node', 'field', 'value')]
  names(df) <- c('scenario', 'node', 'year', 'emissions_TCE')
  
  return(df)
}

baseline_EMISS <- import_gdx_emissions(scenario = 'baseline', version = 16, scenario.name = 'Baseline')
  baseline_EMISS <- unique(baseline_EMISS[c('year', 'node', 'emissions_TCE')])
  names(baseline_EMISS) <- c('year', 'node', 'baseline_emissions')
  
tariff_high_EMISS <- import_gdx_emissions(scenario = 'tariff_high', version = 17, scenario.name = 'High tariff')
tariff_low_EMISS <- import_gdx_emissions(scenario = 'tariff_low', version = 11, scenario.name = 'Low tariff')

CO2_tax_baseline_EMISS <- import_gdx_emissions(scenario = 'CO2_tax_baseline', version = 27, scenario.name = 'CO2 tax, baseline')
CO2_tax_tariff_high_EMISS <- import_gdx_emissions(scenario = 'CO2_tax_tariff_high', version = 11, scenario.name = 'CO2 tax, high tariff')
CO2_tax_tariff_low_EMISS <- import_gdx_emissions(scenario = 'CO2_tax_tariff_low', version = 7, scenario.name = 'CO2 tax, low tariff')

sanction_NAM_MEA_EMISS <- import_gdx_emissions(scenario = 'NAM_MEA_sanction', version = 9, scenario.name = 'Sanction: NAM-MEA')

alldf <- rbind(tariff_high_EMISS, tariff_low_EMISS, 
               CO2_tax_baseline_EMISS, CO2_tax_tariff_high_EMISS, CO2_tax_tariff_low_EMISS,
               sanction_NAM_MEA_EMISS)

alldf <- left_join(alldf, baseline_EMISS, by = c('year', 'node'))

alldf$diff_baseline <- ((alldf$emissions_TCE - alldf$baseline_emissions)/alldf$baseline_emissions)*100

# Plot emissions by region
alldf$region <- stringr::str_replace(alldf$node, 'R14_', '')
alldf$year <- as.numeric(alldf$year)
alldf <- subset(alldf, year < 2100)

# Region labels
region_names <- c('Africa', 'Central Asian States', 'Centrally-Planned Asia', 'Eastern Europe', 'Latin America',
                  'Middle East', 'North America', 'Pacific OECD', 'Pacific South', 'Russia', 'Southern Asia', 
                  'Southern Caucasus', 'Belarus, Moldova, Ukraine', 'Western Europe')

# Separate by type of scenario (non-CO2 vs. CO2)
nonco2_df <- subset(alldf, grepl('CO2', scenario) == F)
co2_df <- subset(alldf, grepl('CO2', scenario))

# Function: plot emissions #
############################
plot_emissions <- function(indf, plot.region, global.title = "World", include.legend = 'none', y_text = '', fontsize = 12) {
  
  plotdf <- subset(indf, region == plot.region)
  plotdf <- unique(plotdf[c('scenario', 'year', 'diff_baseline')])
  
  title.region <- region_names[which(regions == tolower(plot.region))]
  if (plot.region == 'World') {title.region <- global.title}
  
  assign(paste0('plot.', plot.region),
         
    ggplot(aes(x = year, y = diff_baseline, colour = scenario, group = scenario), data = plotdf) + 
      geom_point(size = 2) +
      geom_line(size = 2) + 
      labs(x = "", y = y_text, colour = 'Scenario', title = title.region) + 
      theme(text = element_text(size = fontsize), legend.position = include.legend) +
      scale_colour_brewer(palette = 'Set1'),
    
    envir = parent.frame())
}

# Plot: non-CO2 policies #
##########################
# By region
for (r in c(regions, 'World')) {
  if (r %in% regions) {r <- toupper(r)}
  plot_emissions(nonco2_df, r, fontsize = 10)
}

nonCO2_plot <-
grid.arrange(plot.AFR, plot.CAS, plot.CPA, plot.EEU, plot.LAM,
             plot.MEA, plot.NAM, plot.PAO, plot.PAS, plot.RUS,
             plot.SAS, plot.SCS, plot.UBM, plot.WEU, plot.World,
             ncol = 5,
             left = textGrob("Difference from baseline emissions (%)", rot = 90, gp = gpar(fontsize = 15)))

# World
plot_emissions(nonco2_df, 'World', 'Global Emissions: Trade Policies',
               include.legend = 'right', y_text = 'Difference from baseline emissions (%)', fontsize = 20)

# Plot: CO2 policies #
######################
# By region
for (r in c(regions, 'World')) {
  if (r %in% regions) {r <- toupper(r)}
  plot_emissions(co2_df, r, fontsize = 10)
}

CO2_plot <-
  grid.arrange(plot.AFR, plot.CAS, plot.CPA, plot.EEU, plot.LAM,
               plot.MEA, plot.NAM, plot.PAO, plot.PAS, plot.RUS,
               plot.SAS, plot.SCS, plot.UBM, plot.WEU, plot.World,
               ncol = 5,
               left = textGrob("Difference from baseline emissions (%)", rot = 90, gp = gpar(fontsize = 15)))


# World
plot_emissions(co2_df, 'World', "Global Emissions: Emissions Tax",
               include.legend = 'right', y_text = 'Difference from baseline emissions (%)', fontsize = 20)
