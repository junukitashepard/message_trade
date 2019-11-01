###############################################
# Compile data frame of scenario coefficients #
###############################################
# Import msg regions
msg_regions <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
names(msg_regions) <- c('iso', 'msgregion')

# Build base dataframe #
########################
basedf <- expand.grid(region.list, energy.types)
basedf <- paste0(basedf$Var2, '_imp')
basedf <- expand.grid(basedf, paste0(region.number, '_', toupper(region.list)))
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
basedf$importer <- toupper(stringr::str_replace(basedf$node_loc, paste0(region.number, '_'), ''))
                           
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
wt.energy <- dplyr::inner_join(baci.df, ref_group.out, by = c('j' = 'baci.country', 'iso.country' = 'iso.country', 't' = 'year'))

# Build weight to aggregate to importer-exporter-energy-year
wt.energy <- group_by(wt.energy, j, t) %>% mutate(reporter_imports = sum(q))
wt.energy <- group_by(wt.energy, ref_group, t) %>% mutate(refgroup_imports = sum(q))
wt.energy$weight <- wt.energy$q * (wt.energy$reporter_imports/wt.energy$refgroup_imports)

# Build importer-exporter weights for regional aggregation #
############################################################
# Assign reference group
wt.region <- dplyr::inner_join(baci.df, ref_group.out, by = c('j' = 'baci.country', 'iso.country' = 'iso.country', 't' = 'year'))

# Assign MESSAGE regions
wt.region <- dplyr::inner_join(wt.region, message_regions, by = c('iso.country'))

# Build weight to aggregate to importer-exporter-year
wt.region <- dplyr::group_by(wt.region, i, j, t, iso.country, msg.energy, msg.region, ref_group) %>% 
             dplyr::summarize(q = sum(q))

wt.region <- dplyr::group_by(wt.region, j, t) %>% dplyr::mutate(reporter_imports = sum(q))
wt.region <- dplyr::group_by(wt.region, ref_group, t) %>% dplyr::mutate(refgroup_imports = sum(q))
wt.region$weight <- wt.region$q * (wt.region$reporter_imports/wt.region$refgroup_imports)

# Link weights to tariff data and aggregate #
#############################################
tariff$hs6 <- as.character(tariff$hs6)

# Link energy weights
tariff.out <- dplyr::inner_join(tariff, wt.energy[c('i', 'j', 't', 'hs6', 'msg.energy', 'weight')], 
                         by = c('importer.baci.code' = 'j', 'hs6', 'msg.energy', 'year' = 't'))

# Aggregate to importer-energy level
tariff.out <- dplyr::group_by(tariff.out, importer.baci.code, importer, year, msg.energy) %>% 
              dplyr::summarize(tariff = weighted.mean(tariff, weight))

# Link region weights
tariff.out <- dplyr::inner_join(tariff.out, wt.region[c('i', 'j', 't', 'msg.energy', 'msg.region', 'weight')],
                         by = c('importer.baci.code' = 'j', 'msg.energy', 'year' = 't'))

# Aggregate to region-energy level
tariff.out <- dplyr::group_by(tariff.out, msg.region, year, msg.energy) %>% dplyr::summarize(tariff = weighted.mean(tariff, weight))

# Clean up #
############
# Assign tariffs for fuel oil to fuel oil-adjacent energy
for (e in energy.types.trade.foil) {
  tariff.foil <- subset(tariff.out, msg.energy == 'foil') # assign foil tariffs to loil tariffs
  tariff.foil$msg.energy <- e

  tariff.out <- subset(tariff.out, msg.energy != e)
  tariff.out <- rbind(tariff.out, tariff.foil)
}

# Assign tariffs for LNG to LNG-adjacent energy
for (e in energy.types.trade.LNG) {
  tariff.LNG <- subset(tariff.out, msg.energy == 'LNG') # assign foil tariffs to loil tariffs
  tariff.LNG$msg.energy <- e
  
  tariff.out <- subset(tariff.out, msg.energy != e)
  tariff.out <- rbind(tariff.out, tariff.LNG)
}

tariff.out <- subset(tariff.out, !is.na(tariff))

# Only include energy of interest
tariff.out <- subset(tariff.out, msg.energy %in% energy.types)

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
hist_med_tariff <- dplyr::group_by(tariff.out, msg.region) %>% 
                   dplyr::summarise(hist_tariff = median(tariff, na.rm = T))
hist_med_tariff$high_tariff <- hist_med_tariff$hist_tariff * 5
hist_med_tariff$low_tariff <- 0

tariff.out$technology <- paste0(tariff.out$msg.energy, '_imp')
scen.tariff <- dplyr::left_join(basedf, tariff.out[c('msg.region', 'year', 'tariff', 'technology')], 
                         by = c('importer' = 'msg.region', 'year_act' = 'year', 'technology'))
  scen.tariff <- dplyr::left_join(scen.tariff, hist_med_tariff, by = c('importer' = 'msg.region'))
  scen.tariff$tariff[is.na(scen.tariff$tariff)] <- 0

scen.tariff_hi <- scen.tariff_lo <- scen.tariff

# High tariffs
scen.tariff_hi$tariff[scen.tariff_hi$year_act >= 2030] <- scen.tariff_hi$high_tariff[scen.tariff_hi$year_act >= 2030]
scen.tariff_hi$tariff[scen.tariff_hi$year_act >= 2015 & scen.tariff_hi$year_act < 2030] <- NA
scen.tariff_hi <- arrange(scen.tariff_hi, technology, node_loc, importer, year_act)
scen.tariff_hi$tariff <- zoo::na.approx(scen.tariff_hi$tariff)

hi_tariff_plot <-
ggplot(aes(x = year_act, y = tariff, colour = importer), 
       data = group_by(scen.tariff_hi, year_act, importer)%>%summarize(tariff = mean(tariff, na.rm = T))) +
       geom_point(size = 2) + 
       geom_line(size = 1) +
       labs(x = "Year", y = "AVE on primary products (%)", colour = 'Importer', title = 'High tariff scenario') +
       theme(legend.position = 'bottom', text = element_text(size = 15))

scen.tariff_hi <- scen.tariff_hi[c('technology', 'node_loc', 'year_act', 'tariff')]

# Low tariffs
scen.tariff_lo$tariff[scen.tariff_lo$year_act >= 2030] <- scen.tariff_lo$low_tariff[scen.tariff_lo$year_act >= 2030]
scen.tariff_lo$tariff[scen.tariff_lo$year_act >= 2015 & scen.tariff_lo$year_act < 2030] <- NA
scen.tariff_lo <- arrange(scen.tariff_lo, technology, node_loc, importer, year_act)
scen.tariff_lo$tariff <- zoo::na.approx(scen.tariff_lo$tariff)

lo_tariff_plot <-
  ggplot(aes(x = year_act, y = tariff, colour = importer), 
         data = group_by(scen.tariff_lo, year_act, importer)%>%summarize(tariff = mean(tariff, na.rm = T))) +
  geom_point() + 
  geom_line(size = 1) +
  labs(x = "Year", y = "AVE on primary products (%)", colour = 'Importer', title = 'Low tariff scenario') +
  theme(legend.position = 'bottom', text = element_text(size = 15))

scen.tariff_lo <- scen.tariff_lo[c('technology', 'node_loc', 'year_act', 'tariff')]

# Baseline tariffs
scen.tariff_baseline <- scen.tariff
scen.tariff_baseline$tariff[scen.tariff_baseline$year_act >= 2015] <- 
  scen.tariff_baseline$hist_tariff[scen.tariff_baseline$year_act >= 2015]

baseline_tariff_plot <-
    ggplot(aes(x = year_act, y = tariff, colour = importer), 
           data = group_by(scen.tariff_baseline, year_act, importer)%>%summarize(tariff = mean(tariff, na.rm = T))) +
    geom_point() + 
    geom_line(size = 1) +
    labs(x = "Year", y = "AVE on primary products (%)", colour = 'Importer', title = 'Baseline') +
    theme(legend.position = 'bottom', text = element_text(size = 15))
  
scen.tariff_baseline <- scen.tariff_baseline[c('technology', 'node_loc', 'year_act', 'tariff')]

# Save for compilation
saveRDS(scen.tariff_baseline, file.path(repo, 'analysis/4_run_message/build_scenarios/var_cost_effects/baseline.rds'))
saveRDS(scen.tariff_hi, file.path(repo, 'analysis/4_run_message/build_scenarios/var_cost_effects/tariff_high.rds'))
saveRDS(scen.tariff_lo, file.path(repo, 'analysis/4_run_message/build_scenarios/var_cost_effects/tariff_low.rds'))


