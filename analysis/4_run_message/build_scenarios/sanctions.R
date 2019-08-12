########################################################
# Create sanction scenario, include embedded relations #
# Use baseline scenario as counterfactual #
########################################################
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

input <- paste0(wd, "output/analysis/msg_parameters/")
output <- paste0(wd, "output/analysis/msg_parameters/")

# Get global baseline activity and var_cost #
#############################################
activity <- read_MESSAGE(msg_scenario = 'baseline', msg_version = 8, msg_variable = 'ACT') # Baseline (global schema)
activity <- subset(activity, grepl('_exp_', tec))

# Import base var_cost
export_technologies <- c('oil_exp', 'loil_exp', 'foil_exp', 'LNG_exp', 'coal_exp')

var_cost.base <- data.frame()
for (t in export_technologies) {
  df <- readRDS(file.path(input, paste0('SCENARIOS/baseline/var_cost/', t, '.rds')))
  var_cost.base <- rbind(var_cost.base, df)
}

# Get embodied activity with two regions
########################################
embodied_activity <- function(region1, region2) {
  
  activity$exporter <- stringr::str_replace(activity$node, 'R14_', '')
  activity$importer <- toupper(substr(activity$tec, nchar(activity$tec)-2, nchar(activity$tec)))
  
  trades_with_region1 <- subset(activity, exporter != region1 & importer == region1)
    trades_with_region1 <- group_by(trades_with_region1, exporter, importer, vintage, year_all) %>% mutate(trade_region1 = mean(value, na.rm = T))
    trades_with_region1 <- unique(trades_with_region1[c('node', 'vintage', 'year_all', 'trade_region1')])
  
  trades_with_region2 <- subset(activity, exporter != region2 & importer == region2)
    trades_with_region2 <- group_by(trades_with_region2, exporter, importer, vintage, year_all) %>% mutate(trade_region2 = mean(value, na.rm = T))
    trades_with_region2 <- unique(trades_with_region2[c('node', 'vintage', 'year_all', 'trade_region2')])

  emb_trade <- full_join(trades_with_region1, trades_with_region2, by = c('node', 'vintage', 'year_all'))
  emb_trade$trade_region1[is.na(emb_trade$trade_region1)] <- emb_trade$trade_region2[is.na(emb_trade$trade_region2)] <- 0

  # Assing proclivity for trade with CPA vs. NAM
  emb_trade$pref_region1 <- emb_trade$pref_region2 <- 0
  emb_trade$pref_region1[emb_trade$trade_region1 > emb_trade$trade_region2] <- 1
  emb_trade$pref_region2[emb_trade$trade_region2 > emb_trade$trade_region1] <- 1
  
  emb_trade <- emb_trade[c('node', 'vintage', 'year_all', 'pref_region1', 'pref_region2')]
  names(emb_trade) <- c('node', 'vintage', 'year_all', paste0('pref_', region1), paste0('pref_', region2))
  
  emb_trade$vintage <- as.numeric(emb_trade$vintage)
  emb_trade$year_all <- as.numeric(emb_trade$year_all)
  
  return(emb_trade)
}

emb_trade <- embodied_activity('CPA', 'PAO')

# Link embodied trade back to variable cost #
#############################################
add_var_cost <- function(indf, region1, region2) {
  
  var_cost <- left_join(var_cost.base, indf, by = c('node_loc' = 'node', 'year_vtg' = 'vintage', 'year_act' = 'year_all'))
  
  var_cost$importer <- paste0('R14_', toupper(substr(var_cost$technology, nchar(var_cost$technology)-2, nchar(var_cost$technology))))
  var_cost <- left_join(var_cost, emb_trade, by = c('importer' = 'node', 'year_vtg' = 'vintage', 'year_act' = 'year_all'))
  
  for (i in c('x', 'y')) {
    names(var_cost)[names(var_cost) == paste0('pref_', region1, '.', i)] <- paste0('pref_region1.', i)
    names(var_cost)[names(var_cost) == paste0('pref_', region2, '.', i)] <- paste0('pref_region2.', i)
  }
  
  var_cost$pref_region1.x[is.na(var_cost$pref_region1.x)] <- var_cost$pref_region2.x[is.na(var_cost$pref_region2.x)] <- 0 # export proclivity
  var_cost$pref_region1.y[is.na(var_cost$pref_region1.y)] <- var_cost$pref_region2.y[is.na(var_cost$pref_region2.y)] <- 0 # import proclivity


  # Apply 30% increase to variable cost for model year if embodied
  # Export proclivity
  var_cost$value_add[var_cost$year_act > 2015 &
                     var_cost$pref_region1.x == 1 &
                     var_cost$importer == paste0('R14_', region2)] <- 1.0*var_cost$value[var_cost$year_act > 2015 &
                                                                           var_cost$pref_region1.x == 1 &
                                                                           var_cost$importer == paste0('R14_', region2)]
  var_cost$value_add[var_cost$year_act > 2015 &
                     var_cost$pref_region2.x == 1 &
                     var_cost$importer == paste0('R14_', region1)] <- 1.0*var_cost$value[var_cost$year_act > 2015 &
                                                                           var_cost$pref_region2.x == 1 &
                                                                           var_cost$importer == paste0('R14_', region1)]
  # Import proclivity
  var_cost$value_add[var_cost$year_act > 2015 &
                     var_cost$pref_region1.y == 1 &
                     var_cost$node_loc == paste0('R14_', region2)] <- 1.0*var_cost$value[var_cost$year_act > 2015 &
                                                                           var_cost$pref_region1.y == 1 &
                                                                           var_cost$node_loc == paste0('R14_', region2)]
  var_cost$value_add[var_cost$year_act > 2015 &
                     var_cost$pref_region2.y == 1 &
                     var_cost$node_loc == paste0('R14_', region1)] <- 1.0*var_cost$value[var_cost$year_act > 2015 &
                                                                           var_cost$pref_region2.y == 1 &
                                                                           var_cost$node_loc == paste0('R14_', region1)]

  var_cost$value_add[is.na(var_cost$value_add)] <- 0
  var_cost$value <- var_cost$value + var_cost$value_add

  # Make region1-region2 energy trade prohibitively expensive
  var_cost$value[var_cost$node_loc == paste0('R14_', region1) & grepl(tolower(region2), var_cost$technology)] <- 10*max(var_cost$value, na.rm = T)
  var_cost$value[var_cost$node_loc == paste0('R14_', region2) & grepl(tolower(region1), var_cost$technology)] <- 10*max(var_cost$value, na.rm = T)
  
  var_cost <- var_cost[names(var_cost.base)]
  
  return(var_cost)
}

var_cost <- add_var_cost(emb_trade, 'CPA', 'PAO')

# Run programs #
################
# CPA-NAM sanctions
emb_trade <- embodied_activity('CPA', 'NAM')
cpa_nam_var_cost <-add_var_cost(emb_trade, 'CPA', 'NAM')

for (t in export_technologies) {
  df <- subset(cpa_nam_var_cost, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/NAM_CPA_sanction/var_cost/', t, '.csv')))
}

# CPA-PAO sanctions
emb_trade <- embodied_activity('CPA', 'PAO')
cpa_pao_var_cost <-add_var_cost(emb_trade, 'CPA', 'PAO')

for (t in export_technologies) {
  df <- subset(cpa_pao_var_cost, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/CPA_PAO_sanction/var_cost/', t, '.csv')))
}

# NAM-MEA sanctions
emb_trade <- embodied_activity('NAM', 'MEA')
nam_mea_var_cost <-add_var_cost(emb_trade, 'NAM', 'MEA')

for (t in export_technologies) {
  df <- subset(nam_mea_var_cost, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/NAM_MEA_sanction/var_cost/', t, '.csv')))
}