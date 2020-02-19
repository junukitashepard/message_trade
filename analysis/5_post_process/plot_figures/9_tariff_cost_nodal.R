#######################################################
# Compare tariff policies' marginal impacts under tax #
#######################################################
source('config_post_process.R')

wd <- "C:/Users/jus3/message_trade"
repo <- "C:/Users/jus3/message_trade"
msg_dir <- "C:/Users/jus3/message_trade/gdx_files"

output <- paste0(repo, '/tables')
figures <- paste0(repo, '/figures/unformatted')

setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('ggsci')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('NISTunits')
library('gridExtra')
library('grid')
library('RColorBrewer')

igdx('C:/GAMS/win64/29.1') # Set GDX path

# Import files #
################
# Function to import and collapse activity to global level
import_friendly_gdx <- function(scenario.name, scenario.version) {
  
  df <- read_MESSAGE(msg_scenario = scenario.name, msg_version = scenario.version, msg_variable = 'COST_NODAL')
  
  names(df) <- c('region', 'year', 'cost_nodal')
  df$year <- as.numeric(df$year)
  
  df$scenario <- scenario.name
  
  return(df)
}

co2_tax_tariff_high <- import_friendly_gdx('CO2_tax_tariff_high', 2)
co2_tax_tariff_low <- import_friendly_gdx('CO2_tax_tariff_low', 2)
co2_tax_tariff_baseline <- import_friendly_gdx('CO2_tax_baseline', 2)
  co2_tax_tariff_baseline <- co2_tax_tariff_baseline[c('region', 'year', 'cost_nodal')]
  names(co2_tax_tariff_baseline) <- c('region', 'year', 'cost_nodal_baseline')

df <- rbind(co2_tax_tariff_high, co2_tax_tariff_low)
df <- left_join(df, co2_tax_tariff_baseline, by = c('region', 'year'))

df$diff <- df$cost_nodal - df$cost_nodal_baseline
df$diff_perc <- df$diff/df$cost_nodal_baseline

df <- subset(df, region != 'R14_GLB')
df <- subset(df, year < 2055)

# Plot #
########
plotdf <- df

# Separate exporters and importers
exporter_list <- c('LAM', 'MEA', 'RUS')
importer_list <- c('PAS', 'SAS', 'CPA')

plotdf$trade[plotdf$region %in% paste0('R14_', exporter_list)] <- 'Exporter'
plotdf$trade[plotdf$region %in% paste0('R14_', importer_list)] <- 'Importer'

plotdf <- group_by(plotdf, year, scenario, trade) %>% summarize(mean_diff = mean(diff, na.rm = T))
plotdf <- subset(plotdf, !is.na(trade))

alldf <- group_by(df, year, scenario) %>% summarize(mean_diff = mean(diff, na.rm = T))
alldf$trade <- 'All'

plotdf <- rbind(plotdf, alldf)

plot <- 
ggplot(data = plotdf,
       aes(x = year, y = mean_diff, colour = scenario, shape = trade)) +
  geom_point(size = 3) + 
  geom_line(size = 1) +
  scale_color_aaas() +
  labs()

ggsave(file.path(figures, 'tariff_cost_effects.pdf'), plot, device = 'pdf',
       units = 'in', width = 7, height = 6)


