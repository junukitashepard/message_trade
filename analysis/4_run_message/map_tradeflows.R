#####################################################
# Compile data to create map with arrows in Tableau #
#####################################################
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
import_gdx_activity <- function(scenario, version) {
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
  
  activity <- activity[c('scenario', 'exporter', 'importer', 'energy', 'year', 'value')]
  
  return(activity)
}

baseline_ACT <- import_gdx_activity(scenario = 'baseline', version = 15)
tariff_high_ACT <- import_gdx_activity(scenario = 'tariff_high', version = 15) 
tariff_low_ACT <- import_gdx_activity(scenario = 'tariff_low', version = 9)

# CO2_tax_baseline_ACT <- import_gdx_activity(scenario = 'CO2_tax_baseline', version = 24)
# CO2_tax_tariff_high_ACT <- import_gdx_activity(scenario = 'CO2_tax_tariff_high', version = 9) 
# CO2_tax_tariff_low_ACT <- import_gdx_activity(scenario = 'CO2_tax_tariff_low', version = 5)  
# 
# sanction_NAM_CPA_ACT <- import_gdx_activity(scenario = 'NAM_CPA_sanction', version = 9)   
# sanction_CPA_PAO_ACT <- import_gdx_activity(scenario = 'CPA_PAO_sanction', version = 7)  
sanction_NAM_MEA_ACT <- import_gdx_activity(scenario = 'NAM_MEA_sanction', version = 8)
sanction_direct_NAM_MEA_ACT <- import_gdx_activity(scenario = 'NAM_MEA_sanction_onlydirect', version = 1)
activity <- rbind(baseline_ACT, 
                  tariff_high_ACT, tariff_low_ACT, 
                  # CO2_tax_baseline_ACT, CO2_tax_tariff_high_ACT, CO2_tax_tariff_low_ACT,
                  sanction_NAM_MEA_ACT, sanction_direct_NAM_MEA_ACT)

# Get net exports
regions <- read.csv(file.path(wd, 'raw/ConversionTables/region_coordinates.csv'), stringsAsFactors = F)

paths <- left_join(activity, activity, by = c('exporter' = 'importer', 'importer' = 'exporter', 'energy', 'scenario', 'year'))
paths$value.x[is.na(paths$value.x)] <- 0
paths$value.y[is.na(paths$value.y)] <- 0
paths$net_exports <- paths$value.x - paths$value.y

paths <- subset(paths, net_exports > 0)
paths <- paths[c('scenario', 'exporter', 'importer', 'energy', 'year', 'net_exports')]

# Add coordinates
paths <- left_join(paths, regions, by = c('exporter' = 'region'))
paths <- left_join(paths, regions, by = c('importer' = 'region'))

names(paths) <- c('scenario', 'exporter', 'importer', 'energy', 'year', 'net_exports',
                  'lat.exporter', 'long.exporter', 'lat.importer', 'long.importer')

pair_id <- unique(paths[c('exporter', 'importer')])
pair_id$id <- 1:nrow(pair_id)

paths <- left_join(paths, pair_id, by = c('exporter', 'importer'))

paths$net_exports[paths$net_exports == 0] <- NA

# Reshape long
paths.exporter <- unique(paths[c('scenario', 'exporter', 'energy', 'year', 'id', 'net_exports', 'lat.exporter', 'long.exporter')])
  paths.exporter$step <- 1
paths.importer <- unique(paths[c('scenario', 'importer', 'energy', 'year', 'id', 'net_exports', 'lat.importer', 'long.importer')])
  paths.importer$step <- 2
names(paths.exporter) <- names(paths.importer) <- c('scenario', 'region', 'energy', 'year', 'id', 'net_exports', 'lat', 'long', 'step')

paths <- rbind(paths.exporter, paths.importer)
isid('paths', c('scenario', 'energy', 'year', 'id', 'step'))

paths$net_exports[paths$step == 2] <- 0

# Make dummy values for parameter sizing
dummy_size <- paths
dummy_size <- unique(dummy_size[c('year', 'net_exports')])
dummy_size <- group_by(dummy_size, year) %>% summarize(net_exports = max(net_exports, na.rm = T))
dummy_size$energy <- 'dummy'
dummy_size$lat <- -53
dummy_size$long <- -31

paths2 <- paths
paths2$net_exports <- NULL
dummy_size <- left_join(dummy_size, paths2, by = c('year'))

for (v in c('energy', 'lat', 'long')) {
  names(dummy_size)[names(dummy_size) == paste0(v, '.x')] <- 'var.x'
  names(dummy_size)[names(dummy_size) == paste0(v, '.y')] <- 'var.y'
  
  dummy_size$var <- dummy_size$var.x
  dummy_size$var[is.na(dummy_size$var)] <- dummy_size$var.y[is.na(dummy_size$var)]
  dummy_size$var.x <- dummy_size$var.y <- NULL
  
  names(dummy_size)[names(dummy_size) == 'var'] <- v
}

dummy_size <- dummy_size[c(names(paths))]

paths <- rbind(as.data.frame(paths), as.data.frame(dummy_size))

# Assign label for amount of trade
paths <- group_by(paths, id, energy, year, scenario) %>% mutate(max_trade = max(net_exports))

write.csv(paths, file.path(output, paste0('message_trade.csv')))
saveRDS(paths, file.path(output, 'message_trade.rds'))

# Map differences between sanction scenarios #
##############################################
baseline <- subset(paths, scenario == 'baseline' & energy == 'oil')[c('year', 'id', 'step', 'lat', 'long', 'net_exports', 'max_trade')]
names(baseline) <- c('year', 'id', 'step', 'lat', 'long', 'baseline_exports', 'baseline_max')

nam_mea <- subset(paths, scenario == 'NAM_MEA_sanction' & energy == 'oil')[c('year', 'id', 'step', 'lat', 'long', 'net_exports', 'max_trade')]
names(nam_mea) <- c('year', 'id', 'step', 'lat', 'long', 'nam_mea_exports', 'nam_mea_max')

nam_mea_direct <- subset(paths, scenario == 'NAM_MEA_sanction_onlydirect' & energy == 'oil')[c('year', 'id', 'step', 'lat', 'long', 'net_exports', 'max_trade')]
names(nam_mea_direct) <- c('year', 'id', 'step', 'lat', 'long', 'nam_mea_direct_exports', 'nam_mea_direct_max')

outdf <- left_join(baseline, nam_mea, by = c('year', 'id', 'step', 'lat', 'long'))
outdf <- left_join(outdf, nam_mea_direct, by = c('year', 'id', 'step', 'lat', 'long'))

outdf$nam_mea_exports[is.na(outdf$nam_mea_exports)] <- outdf$baseline_exports[is.na(outdf$baseline_exports)] <- 0
outdf$nam_mea_direct_exports[is.na(outdf$nam_mea_direct_exports)] <- outdf$baseline_exports[is.na(outdf$baseline_exports)] <- 0

# Differece WITH indirect sanction effect
outdf$difference <- outdf$nam_mea_max-outdf$baseline_max

outdf$difference_color[outdf$difference > 0] <- 'more'
outdf$difference_color[outdf$difference < 0] <- 'less'
outdf$difference_color[is.na(outdf$difference_color)] <- 'grey'

# Difference WITHOUT indirect sanction effect
outdf$difference_direct <- outdf$nam_mea_direct_max-outdf$baseline_max

outdf$difference_direct_color[outdf$difference_direct > 0] <- 'more'
outdf$difference_direct_color[outdf$difference_direct < 0] <- 'less'
outdf$difference_direct_color[is.na(outdf$difference_direct)] <- 'grey'

outdf$nam_mea_exports[is.na(outdf$nam_mea_exports)] <- outdf$baseline_exports[is.na(outdf$nam_mea_exports)]
outdf$nam_mea_direct_exports[is.na(outdf$nam_mea_direct_exports)] <- outdf$baseline_exports[is.na(outdf$nam_mea_direct_exports)]

outdf2 <- outdf
names(outdf2) <- c('year', 'id', 'step', 'lat', 'long', 
                   'nam_mea_exports', 'nam_mea_max', 
                   'nam_mea_direct_exports', 'nam_mea_direct_max',
                   'baseline_exports', 'baseline_max', 
                   'difference', 'difference_color', 'difference_direct', 'difference_direct_color')

outdf2$baseline_exports <- outdf2$baseline_max <- outdf2$difference <- outdf2$difference_direct <- NA
outdf2$difference_color <- outdf2$difference_direct_color <- 'baseline'
outdf2 <- outdf2[names(outdf)]

outdf <- rbind(outdf, outdf2)

write.csv(outdf, file.path(output, 'nam_mea.csv'))



