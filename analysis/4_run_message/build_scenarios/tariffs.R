###############################################
# Compile data frame of scenario coefficients #
###############################################
rm(list = ls())
wd.data <- "H:/data/"
wd <- 'H:/message_trade/analysis/4_run_message/build_scenarios/'
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')
library('ggplot2')

raw <-      paste0(wd.data, "raw")
input <-    paste0(wd.data, "output/analysis/regress/")
output <-   paste0(wd.data, "output/analysis/regress/")
temp <-     paste0(wd.data, "temp/")

source(paste0(wd, '4_regress.R'))
#######################################################
# Import msg regions
msg_regions <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
names(msg_regions) <- c('iso', 'msgregion')

# Build base dataframe #
########################
region_list <- c('AFR', 'CAS', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'RUS', 'SAS', 'WEU', 'UBM', 'SCS')
energy_list <- c('oil', 'coal', 'foil', 'LNG')
MESSAGE_years <- c(seq(1995, 2055, by = 5), seq(2060, 2110, by = 10))

basedf <- expand.grid(tolower(region_list), energy_list)
basedf <- paste0(basedf$Var2, '_imp')
basedf <- expand.grid(basedf, paste0('R14_', region_list))
names(basedf) <- c('technology', 'node_loc')

df <- data.frame()
for (y in MESSAGE_years) {
  tdf <- basedf
  tdf$year_act <- y
  df <- rbind(df, tdf)
}
basedf <- df
df <- NULL

basedf$technology <- as.character(basedf$technology)
basedf$importer <- toupper(stringr::str_replace(basedf$node_loc, 'R14_', ''))
                           
basedf <- unique(basedf)

# Tariffs #
###########
# Import tariff data
tariff <- read.csv(file.path(raw, 'WorldBank/tariff_primary_goods.csv'), stringsAsFactors = F)
names(tariff)[1:3] <- c('country', 'iso', 'var')

outdf <- data.frame()
for (y in 1995:2015) {
  df <- tariff[c('iso', paste0('X', y))]
  names(df) <- c('iso', 'tariff_rate')
  df$year <- y
  outdf <- rbind(outdf, df)
}
tariff <- left_join(outdf, msg_regions, by = c('iso'))
tariff <- subset(tariff, !is.na(msgregion) & msgregion != "")

tariff <- subset(tariff, tariff_rate < 500) # Excludes Nepal in 2008

tariff <- group_by(tariff, msgregion, year) %>% summarise(mean_tariff = mean(tariff_rate, na.rm = T))

# Plot tariff data
tariff_plot <- 
ggplot(aes(x = year, y = mean_tariff, colour = msgregion), data = tariff) +
  geom_point() +
  geom_line(size = 1) + 
  labs(x = "Year", y = "Median tariff rate (%) of region", colour = 'Region') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

# Link to basedf
hist_mean_tariff <- group_by(tariff, msgregion) %>% summarise(hist_tariff = median(mean_tariff, na.rm = T))
hist_mean_tariff$high_tariff <- hist_mean_tariff$hist_tariff * 10
hist_mean_tariff$low_tariff <- (hist_mean_tariff$hist_tariff * 10)*-1

scen.tariff <- left_join(basedf, tariff, by = c('importer' = 'msgregion', 'year_act' = 'year'))
  scen.tariff <- left_join(scen.tariff, hist_mean_tariff, by = c('importer' = 'msgregion'))
  scen.tariff$mean_tariff[is.na(scen.tariff$mean_tariff)] <- 0

scen.tariff_hi <- scen.tariff_lo <- scen.tariff

# High tariffs
scen.tariff_hi$mean_tariff[scen.tariff_hi$year_act >= 2030] <- scen.tariff_hi$high_tariff[scen.tariff_hi$year_act >= 2030]
scen.tariff_hi$mean_tariff[scen.tariff_hi$year_act > 2015 & scen.tariff_hi$year_act < 2030] <- NA
scen.tariff_hi <- arrange(scen.tariff_hi, technology, node_loc, importer, year_act)
scen.tariff_hi$mean_tariff <- zoo::na.approx(scen.tariff_hi$mean_tariff)

hi_tariff_plot <-
ggplot(aes(x = year_act, y = mean_tariff, colour = importer), data = unique(scen.tariff_hi[c('year_act', 'importer', 'mean_tariff')])) +
  geom_point() + 
  geom_line(size = 1) +
  labs(x = "Year", y = "Median import tariff on primary products (%)", colour = 'Importer', title = 'High tariff scenario') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

scen.tariff_hi <- scen.tariff_hi[c('technology', 'node_loc', 'year_act', 'mean_tariff')]

# Low tariffs
scen.tariff_lo$mean_tariff[scen.tariff_lo$year_act >= 2030] <- scen.tariff_lo$low_tariff[scen.tariff_lo$year_act >= 2030]
scen.tariff_lo$mean_tariff[scen.tariff_lo$year_act > 2015 & scen.tariff_lo$year_act < 2030] <- NA
scen.tariff_lo <- arrange(scen.tariff_lo, technology, node_loc, importer, year_act)
scen.tariff_lo$mean_tariff <- zoo::na.approx(scen.tariff_lo$mean_tariff)

lo_tariff_plot <-
  ggplot(aes(x = year_act, y = mean_tariff, colour = importer), data = unique(scen.tariff_lo[c('year_act', 'importer', 'mean_tariff')])) +
  geom_point() + 
  geom_line(size = 1) +
  labs(x = "Year", y = "Median import tariff on primary products (%)", colour = 'Importer', title = 'Low tariff scenario') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

scen.tariff_lo <- scen.tariff_lo[c('technology', 'node_loc', 'year_act', 'mean_tariff')]

scen.tariff_baseline <- scen.tariff
scen.tariff_baseline$mean_tariff[scen.tariff_baseline$year_act > 2015] <- 
  scen.tariff_baseline$hist_tariff[scen.tariff_baseline$year_act > 2015]
  scen.tariff_baseline <- scen.tariff_baseline[c('technology', 'node_loc', 'year_act', 'mean_tariff')]

# Save for compilation
saveRDS(scen.tariff_baseline, file.path(wd, 'var_cost_effects/baseline.rds'))
saveRDS(scen.tariff_hi, file.path(wd, 'var_cost_effects/tariff_high.rds'))
saveRDS(scen.tariff_lo, file.path(wd, 'var_cost_effects/tariff_low.rds'))


