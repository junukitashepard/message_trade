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
library('ggsci')

library('RColorBrewer')
ncolors <- 14
mycolors <- colorRampPalette(brewer.pal(8, 'Paired'))(ncolors)

# List of energy commodities
energy_list <- c('COAL', 'OIL', 'FOIL', 'LOIL', 'LNG')

# Conversion factor
gwa_to_ej <- 8760*3600*(10^-9)

# Plot GLOBAL baseline #
########################
gps <- read_MESSAGE(msg_scenario = 'baseline_global_schema', msg_version = 1, msg_variable = 'ACT') # Baseline (global schema)
bts <- read_MESSAGE(msg_scenario = 'baseline', msg_version = 16, msg_variable = 'ACT') # Baseline (global schema)

gps$model <- 'Global Pool'
bts$model <- 'Bilateral Trade'

# Global pool is vintaged
gps <- group_by(gps, node, tec, year_all, mode, field, model) %>%
       summarize(value = sum(value))

bts$vintage <- NULL

# Bilateral trade has aggregated exports, remove
bts <- subset(bts, !(tec %in% paste0(tolower(energy_list), '_exp')))

# Combine BTS and GPS
df <- rbind(as.data.frame(gps), as.data.frame(bts))

# Keep trade technologies
df <- subset(df, grepl('_exp', tec))

# Separate importer and exporter
df$exporter <- gsub('R14_', '', df$node)
df$importer <- toupper(gsub('.*_exp_', '', df$tec))
df$importer[grepl('_EXP', df$importer)] <- 'POOL'

# Auxiliary variables
df$year <- as.numeric(df$year_all)
df$energy <- toupper(gsub('_exp.*', '', df$tec))

# Keep only energy represented
df <- subset(df, energy %in% energy_list)

# Subset to relevant variables
df <- unique(df[c('importer', 'exporter', 'year', 'energy', 'value', 'model')])
isid('df', c('importer', 'exporter', 'year', 'energy', 'model'))

# Keep pre-2050 data
df <- subset(df, year <= 2050)

# Convert to EJ
df$value <- df$value * gwa_to_ej

# Fix energy names
df$energy.name[df$energy == 'COAL'] <- 'Coal'
df$energy.name[df$energy == 'FOIL'] <- 'Fuel oil'
df$energy.name[df$energy == 'LOIL'] <- 'Light oil'
df$energy.name[df$energy == 'OIL'] <- 'Crude oil'
df$energy.name[df$energy == 'LNG'] <- 'LNG'

# Comprehensive list of regions, set to 0 where necessary
regionlist <- unique(c(df$importer, df$exporter))
basedf <- expand.grid(regionlist, regionlist)
  names(basedf) <- c('importer', 'exporter')
  #basedf <- subset(basedf, importer != exporter)
  basedf$importer <- as.character(basedf$importer)
  basedf$exporter <- as.character(basedf$exporter)

yeardf <- data.frame()
for (y in unique(df$year)) {
  tdf <- basedf
  tdf$year <- y
  yeardf <- rbind(as.data.frame(yeardf), as.data.frame(tdf))
}
yeardf$model <- 'Global Pool'
yeardf1 <- yeardf; yeardf1$model <- 'Bilateral Trade'

basedf <- rbind(yeardf, yeardf1)

# Plot by energy commodity for top 5 exporters/importers #
##########################################################
plot_comparison <- function(energy_type) {
  plotdf <- subset(df, energy == energy_type)
  plotdf$energy <- NULL
  
  plotdf <- left_join(basedf, plotdf, by = c('importer', 'exporter', 'year', 'model'))
  plotdf$value[is.na(plotdf$value)] <- 0
  
  # Top 3 list
  t3df <- group_by(plotdf, exporter) %>% summarize(value = sum(value, na.rm = T))
  t3df <- arrange(t3df, desc(value))
  
  t3_list <- t3df$exporter[1:3]
  
  # Plot for top 3
  i <- 1
  for (r in t3_list) {
    assign(paste0('plot', i),
    ggplot(data = subset(plotdf, exporter == r),
           aes(x = year, y = value, fill = importer)) +
      geom_bar(stat = 'identity', colour = 'black') +
      facet_grid(.~model) + 
      labs(x = 'Year', y = 'Trade flow (GWa)', fill = 'Importer',
           title = paste0('Region: ', r)) +
      theme(text = element_text(size = 15),
            axis.text.x = element_text(angle = 90)),
    envir = parent.frame())
    
    i <- i + 1
  }
  
  plot <- 
  ggpubr::ggarrange(plot1, plot2, plot3,
                    ncol = 3)
  
  return(plot)
}

plot_comparison('OIL')
plot_comparison('FOIL')
plot_comparison('LOIL')
plot_comparison('COAL')
plot_comparison('LNG')

# Plot overall changes in exports and imports #
###############################################
# Overall size of network
total_trade <- group_by(df, year, energy, energy.name, model) %>% summarize(value = sum(value))

ggplot(data = total_trade,
       aes(x = year, y = value, fill = energy.name)) +
  geom_bar(stat = 'identity', colour = 'black') +
  facet_grid(.~model) +
  labs(x = 'Year', y = 'Total exports (EJ)', fill = 'Energy') + 
  theme(text = element_text(size = 18)) +
  scale_fill_nejm()

# By region
# Get 0 values
fulldf <- data.frame()
for (e in energy_list) {
  indf <- basedf
  indf$energy <- e
  fulldf <- rbind(fulldf, indf)
}

fulldf <- left_join(fulldf, df, by = c('importer', 'exporter', 'year', 'model', 'energy'))

# Fix energy names
fulldf$energy.name[fulldf$energy == 'COAL'] <- 'Coal'
fulldf$energy.name[fulldf$energy == 'FOIL'] <- 'Fuel oil'
fulldf$energy.name[fulldf$energy == 'LOIL'] <- 'Light oil'
fulldf$energy.name[fulldf$energy == 'OIL'] <- 'Crude oil'
fulldf$energy.name[fulldf$energy == 'LNG'] <- 'LNG'

fulldf$value[is.na(fulldf$value)] <- 0

total_exports <- group_by(df, exporter, year, energy, energy.name, model) %>% summarize(value = sum(value))
total_imports <- group_by(df, importer, year, energy, energy.name, model) %>% summarize(value = sum(value))

total_imports <- subset(total_imports, importer != 'POOL')

# Funtion put GPS and BTS values in different columns
to_wide <- function(indf) {
  
  indf1 <- subset(indf, model == 'Global Pool')
  indf2 <- subset(indf, model == 'Bilateral Trade')
  indf1$model <- indf2$model <- NULL
  
  names(indf1) <- c('region', 'year', 'energy', 'energy.name', 'GPS')
  names(indf2) <- c('region', 'year', 'energy', 'energy.name', 'BTS')
  
  outdf <- full_join(indf1, indf2, by = c('region', 'year', 'energy', 'energy.name'))
  
  outdf$GPS[is.na(outdf$GPS)] <- 0
  outdf$BTS[is.na(outdf$BTS)] <- 0
  
  return(outdf)
}

total_exports <- to_wide(total_exports)
total_imports <- to_wide(total_imports)

# Plot different between total of GPS and BTS
total_exports$difference <- (total_exports$BTS - total_exports$GPS)
total_imports$difference <- (total_imports$BTS - total_imports$GPS)

ggplot(data = total_exports,
       aes(x = year, y = difference, colour = region)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_grid(.~energy.name) + 
  scale_colour_manual(values = mycolors) + 
  labs(x = '', y = 'Difference in exports (EJ)', colour = 'Region') +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90))
  
ggplot(data = total_imports,
       aes(x = year, y = difference, colour = region)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_grid(.~energy.name) + 
  scale_colour_manual(values = mycolors) + 
  labs(x = '', y = 'Difference in imports (EJ)', colour = 'Region') +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90))
