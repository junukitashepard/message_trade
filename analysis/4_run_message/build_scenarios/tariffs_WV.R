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
library('cluster')
library('NbClust')
library('factoextra')
library('RMySQL')

raw <-      paste0(wd.data, "raw")
derived <-  paste0(wd.data, 'output/derived')
# input <-    paste0(wd.data, "output/analysis/regress/")
# output <-   paste0(wd.data, "output/analysis/regress/")
temp <-     paste0(wd.data, "temp/")

source(paste0(wd, '4_regress.R'))
#######################################################
# Import msg regions
msg_regions <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
names(msg_regions) <- c('iso', 'msgregion')

# Build base dataframe #
########################
region_list <- c('AFR', 'CAS', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'RUS', 'SAS', 'WEU', 'UBM', 'SCS')
energy_list <- c('oil', 'coal', 'foil', 'loil', 'LNG')
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

# Import files #
################
# Import tariff data
tariff <- read.csv(file.path(raw, 'WTO/WtoData_EnergyTariff_AVE.csv'), stringsAsFactors = F)
tariff <- tariff[c('Reporting.Economy.Code', 'Reporting.Economy', 'Product.Sector.Code', 'Year', 'Value')]
names(tariff) <- c('importer.baci.code', 'importer', 'hs6', 'year', 'tariff')

tariff$hs6 <- as.character(tariff$hs6)
tariff <- subset(tariff, nchar(hs6) == 6)
tariff$hs6 <- as.numeric(tariff$hs6)

# Import and link HS conversion file
hs_conversion <- read.csv(file.path(raw, 'BACI/energy2hs4_MSG.csv'), stringsAsFactors = F)
tariff <- inner_join(tariff, hs_conversion[c('hs6', 'msg.energy')], by = c('hs6'))

# Import country names
web_countries <- read.csv(file.path(raw, 'ConversionTables/web_countries.csv'), stringsAsFactors = F)

# Import MESSAGE regions
message_regions <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
names(message_regions) <- c('iso.country', 'msg.region')
message_regions <- subset(message_regions, !is.na(msg.region) & msg.region != "")

# Import World Bank indicators
import_WB <- function(indf, varname) {
  
  assign('df', get(indf))
  
  df$ï..Country.Name <- NULL
  
  outdf <- data.frame()
  for (y in 1995:2018) {
    tdf <- df[c('Country.Code', paste0('X', y))]
    names(tdf) <- c('iso.country', varname)
    tdf$year <- y
    outdf <- rbind(outdf, tdf)
  }
  
  return(outdf)
}

gdp <- read.csv(file.path(raw, 'WorldBank/GDP_Per_Capita.csv'), stringsAsFactors = F)
gdp.df <- import_WB('gdp', 'gdp_pc')
gdp.df <- subset(gdp.df, !is.na(gdp_pc))
rm(gdp)

# Import World Bank trade data
trade_openness <- read.csv(file.path(raw, 'WorldBank/Trade_PercGDP.csv'), stringsAsFactors = F)
trade_openness.df <- import_WB('trade_openness', 'trade_perc_GDP')
trade_openness.df <- subset(trade_openness.df, !is.na(trade_perc_GDP))
rm(trade_openness)

# Import BACI trade data
import_baci <- function(year) {

  environment(assert) <- environment()

  print(paste0('Importing: ', year))

  sql_statement <- paste0("SELECT * FROM BACI_TRADE.BACI_", y, " ",
                          "WHERE LEFT(BACI_TRADE.BACI_", y, ".hs6, 2) = '27' OR ",
                          "LEFT(BACI_TRADE.BACI_", y, ".hs6, 2) = '38' OR ",
                          "LEFT(BACI_TRADE.BACI_", y, ".hs6, 2) = '28' OR ",
                          "LEFT(BACI_TRADE.BACI_", y, ".hs6, 2) = '22'")

  import_sql(statement = sql_statement,
             user = 'root',
             password = 'SEAmonst3r!',
             dbname = 'BACI_TRADE',
             outdf = 'baci')

  baci$hs6 <- as.character(baci$hs6)
  assert('nchar(baci$hs6) == 6')

  # Link BACI to crosswalks
  baci <- left_join(baci, web_countries, by = c('j' = 'baci.country')) # importer only

  return(baci)
}

baci.df <- data.frame()
for (y in 1995:2014) {
  in.baci <- import_baci(y)
  baci.df <- rbind(baci.df, in.baci)
}

# Aggregate to mean by weight
hs_conversion$hs6 <- as.character(hs_conversion$hs6)
baci.df <- inner_join(baci.df, hs_conversion[c('hs6', 'msg.energy')], by = c('hs6'))

# Build reference groups #
##########################
# Link GDP and trade openness
ref_group.df <- inner_join(gdp.df, trade_openness.df, by = c('iso.country', 'year'))

# K-means cluster
ref_group.out <- data.frame()
for (y in 1995:2018) {
  
  assign('df', subset(ref_group.df, year == y))
  df <- arrange(df, iso.country)
  
  # Convert to matrix
  assign('mat', as.matrix(df[c('gdp_pc', 'trade_perc_GDP')]))
  rownames(mat) <- df$iso.country
  colnames(mat) <- c('gdp_pc', 'trade_perc_GDP')
  
  group_cluster <- kmeans(mat, centers = 5, nstart = 25)
  
  assign('outdf', as.data.frame(group_cluster$cluster))
  outdf$iso.country <- rownames(mat)
  outdf$year <- y
  names(outdf) <- c('ref_group', 'iso.country', 'year')
  outdf <- outdf[c('iso.country', 'year', 'ref_group')]
  
  ref_group.out <- rbind(ref_group.out, outdf)
}

ref_group.out <- inner_join(ref_group.out, web_countries[c('iso.country', 'baci.country')], by = c('iso.country'))

# Build product-importer-exporter weights for energy aggregation #
##################################################################
# Assign reference group by importer
wt.energy <- inner_join(baci.df, ref_group.out, by = c('j' = 'baci.country', 'iso.country' = 'iso.country', 't' = 'year'))

# Build weight to aggregate to importer-exporter-energy-year
wt.energy <- group_by(wt.energy, j, t) %>% mutate(reporter_imports = sum(q))
wt.energy <- group_by(wt.energy, ref_group, t) %>% mutate(refgroup_imports = sum(q))
wt.energy$weight <- wt.energy$q * (wt.energy$reporter_imports/wt.energy$refgroup_imports)

# Build importer-exporter weights for regional aggregation #
############################################################
# Assign reference group
wt.region <- inner_join(baci.df, ref_group.out, by = c('j' = 'baci.country', 'iso.country' = 'iso.country', 't' = 'year'))

# Assign MESSAGE regions
wt.region <- inner_join(wt.region, message_regions, by = c('iso.country'))

# Build weight to aggregate to importer-exporter-year
wt.region <- group_by(wt.region, i, j, t, iso.country, msg.energy, msg.region, ref_group) %>% summarize(q = sum(q))

wt.region <- group_by(wt.region, j, t) %>% mutate(reporter_imports = sum(q))
wt.region <- group_by(wt.region, ref_group, t) %>% mutate(refgroup_imports = sum(q))
wt.region$weight <- wt.region$q * (wt.region$reporter_imports/wt.region$refgroup_imports)

# Link weights to tariff data and aggregate #
#############################################
tariff$hs6 <- as.character(tariff$hs6)

# Link energy weights
tariff.out <- inner_join(tariff, wt.energy[c('i', 'j', 't', 'hs6', 'msg.energy', 'weight')], 
                         by = c('importer.baci.code' = 'j', 'hs6', 'msg.energy', 'year' = 't'))

# Aggregate to importer-energy level
tariff.out <- group_by(tariff.out, importer.baci.code, importer, year, msg.energy) %>% summarize(tariff = weighted.mean(tariff, weight))

# Link region weights
tariff.out <- inner_join(tariff.out, wt.region[c('i', 'j', 't', 'msg.energy', 'msg.region', 'weight')],
                         by = c('importer.baci.code' = 'j', 'msg.energy', 'year' = 't'))

# Aggregate to region-energy level
tariff.out <- group_by(tariff.out, msg.region, year, msg.energy) %>% summarize(tariff = weighted.mean(tariff, weight))

# Clean up #
############
tariff.loil <- subset(tariff.out, msg.energy == 'foil') # assign foil tariffs to loil tariffs
tariff.loil$msg.energy <- 'loil'

tariff.out <- subset(tariff.out, msg.energy != 'loil')
tariff.out <- rbind(tariff.out, tariff.loil)

tariff.out <- subset(tariff.out, !is.na(tariff))

# Only include energy of interest
tariff.out <- subset(tariff.out, msg.energy %in% energy_list)

# Plot tariff data #
####################
tariff_plot <- 
ggplot(aes(x = year, y = tariff, colour = msg.region), data = tariff.out) +
  geom_point() +
  geom_line(size = 1) + 
  labs(x = "Year", y = "Mean AVE (%) of region", colour = 'Region') +
  theme(legend.position = 'bottom', text = element_text(size = 15)) + 
  facet_grid(.~msg.energy)

# Build parameter #
###################
hist_med_tariff <- group_by(tariff.out, msg.region) %>% summarise(hist_tariff = median(tariff, na.rm = T))
hist_med_tariff$high_tariff <- hist_med_tariff$hist_tariff * 5
hist_med_tariff$low_tariff <- 0

scen.tariff <- left_join(basedf, tariff.out, by = c('importer' = 'msg.region', 'year_act' = 'year'))
  scen.tariff <- left_join(scen.tariff, hist_med_tariff, by = c('importer' = 'msg.region'))
  scen.tariff$tariff[is.na(scen.tariff$tariff)] <- 0

scen.tariff_hi <- scen.tariff_lo <- scen.tariff

# High tariffs
scen.tariff_hi$tariff[scen.tariff_hi$year_act >= 2030] <- scen.tariff_hi$high_tariff[scen.tariff_hi$year_act >= 2030]
scen.tariff_hi$tariff[scen.tariff_hi$year_act > 2015 & scen.tariff_hi$year_act < 2030] <- NA
scen.tariff_hi <- arrange(scen.tariff_hi, technology, node_loc, importer, year_act)
scen.tariff_hi$tariff <- zoo::na.approx(scen.tariff_hi$tariff)

hi_tariff_plot <-
ggplot(aes(x = year_act, y = tariff, colour = importer), data = unique(scen.tariff_hi[c('year_act', 'importer', 'tariff')])) +
  geom_point() + 
  geom_line(size = 1) +
  labs(x = "Year", y = "AVE on primary products (%)", colour = 'Importer', title = 'High tariff scenario') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

scen.tariff_hi <- scen.tariff_hi[c('technology', 'node_loc', 'year_act', 'tariff')]

# Low tariffs
scen.tariff_lo$tariff[scen.tariff_lo$year_act >= 2030] <- scen.tariff_lo$low_tariff[scen.tariff_lo$year_act >= 2030]
scen.tariff_lo$tariff[scen.tariff_lo$year_act > 2015 & scen.tariff_lo$year_act < 2030] <- NA
scen.tariff_lo <- arrange(scen.tariff_lo, technology, node_loc, importer, year_act)
scen.tariff_lo$tariff <- zoo::na.approx(scen.tariff_lo$tariff)

lo_tariff_plot <-
  ggplot(aes(x = year_act, y = tariff, colour = importer), data = unique(scen.tariff_lo[c('year_act', 'importer', 'tariff')])) +
  geom_point() + 
  geom_line(size = 1) +
  labs(x = "Year", y = "AVE on primary products (%)", colour = 'Importer', title = 'Low tariff scenario') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

scen.tariff_lo <- scen.tariff_lo[c('technology', 'node_loc', 'year_act', 'tariff')]

# Baseline tariffs
scen.tariff_baseline <- scen.tariff
scen.tariff_baseline$tariff[scen.tariff_baseline$year_act > 2015] <- scen.tariff_baseline$hist_tariff[scen.tariff_baseline$year_act > 2015]
scen.tariff_baseline <- scen.tariff_baseline[c('technology', 'node_loc', 'year_act', 'tariff')]

baseline_tariff_plot <-
    ggplot(aes(x = year_act, y = tariff, colour = node_loc), data = unique(scen.tariff_baseline[c('year_act', 'node_loc', 'tariff')])) +
    geom_point() + 
    geom_line(size = 1) +
    labs(x = "Year", y = "AVE on primary products (%)", colour = 'Importer', title = 'Baseline') +
    theme(legend.position = 'bottom', text = element_text(size = 15))
  
# Save for compilation
saveRDS(scen.tariff_baseline, file.path(wd, 'var_cost_effects/baseline.rds'))
saveRDS(scen.tariff_hi, file.path(wd, 'var_cost_effects/tariff_high.rds'))
saveRDS(scen.tariff_lo, file.path(wd, 'var_cost_effects/tariff_low.rds'))


