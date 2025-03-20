###############################################
# Compile data frame of scenario coefficients #
###############################################
# Import msg regions
msg_regions <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
names(msg_regions) <- c('iso', 'msgregion')

# Build base dataframe #
########################
basedf <- expand.grid(tolower(region.list.trade), energy.types.BACI)
basedf <- paste0(basedf$Var2, '_exp_', basedf$Var1)
basedf <- expand.grid(basedf, paste0(region.number, '_', region.list.trade))
names(basedf) <- c('technology', 'node_loc')

df <- data.frame()
for (y in MESSAGE.years) {
  tdf <- basedf
  tdf$year_act <- y
  df <- rbind(df, tdf)
}
basedf <- df
df <- NULL

basedf$technology <- as.character(basedf$technology)
basedf$importer <- toupper(substr(basedf$technology, 
                                  nchar(basedf$technology)-2, 
                                  nchar(basedf$technology)))

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
  labs(x = "Year", y = "Mean tariff rate (%) of region", colour = 'Region') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

# Link to basedf
hist_mean_tariff <- group_by(tariff, msgregion) %>% summarise(hist_tariff = mean(mean_tariff, na.rm = T))
hist_mean_tariff$high_tariff <- hist_mean_tariff$hist_tariff * 2
hist_mean_tariff$low_tariff <- hist_mean_tariff$hist_tariff * 0.25

scen.tariff <- left_join(basedf, tariff, by = c('importer' = 'msgregion', 'year_act' = 'year'))
  scen.tariff <- left_join(scen.tariff, hist_mean_tariff, by = c('importer' = 'msgregion'))
  scen.tariff$mean_tariff[is.na(scen.tariff$mean_tariff)] <- 0
  
scen.tariff_hi <- scen.tariff_lo <- scen.tariff
scen.tariff_hi$mean_tariff[scen.tariff_hi$year_act > 2015] <- scen.tariff_hi$high_tariff[scen.tariff_hi$year_act > 2015]
  scen.tariff_hi <- scen.tariff_hi[c('technology', 'node_loc', 'year_act', 'mean_tariff')]
scen.tariff_lo$mean_tariff[scen.tariff_lo$year_act > 2015] <- scen.tariff_lo$low_tariff[scen.tariff_lo$year_act > 2015]
  scen.tariff_lo <- scen.tariff_lo[c('technology', 'node_loc', 'year_act', 'mean_tariff')]
  
scen.tariff_baseline <- scen.tariff
scen.tariff_baseline$mean_tariff[scen.tariff_baseline$year_act > 2015] <- scen.tariff_baseline$hist_tariff[scen.tariff_baseline$year_act > 2015]
  scen.tariff_baseline <- scen.tariff_baseline[c('technology', 'node_loc', 'year_act', 'mean_tariff')]



