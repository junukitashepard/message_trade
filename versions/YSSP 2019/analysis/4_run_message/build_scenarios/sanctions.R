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

# Get global baseline var_cost and historical alliances #
#########################################################
ubset(activity, grepl('_exp_', tec))

# Import base var_cost
export_technologies <- c('oil_exp', 'loil_exp', 'foil_exp', 'LNG_exp', 'coal_exp')

var_cost.base <- data.frame()
for (t in export_technologies) {
  df <- readRDS(file.path(input, paste0('SCENARIOS/baseline/var_cost/', t, '.rds')))
  var_cost.base <- rbind(var_cost.base, df)
}

# Alliances
alliances <- readRDS(file.path(input, paste0('SCENARIOS/historical_alliances.rds')))

# Assign costs based on historical alliances
assign_premium <- function(region1, region2) {
  
  for (r in 1:2) {
    assign('sanctionregion', get(paste0('region', r)))
    df <- subset(alliances, msgregion1 == sanctionregion | msgregion2 == sanctionregion)
      df$partner <- NA
      df$partner[df$msgregion1 != sanctionregion] <- df$msgregion1[df$msgregion1 != sanctionregion]
      df$partner[is.na(df$partner)] <- df$msgregion2[df$msgregion2 != sanctionregion]
      
    df <- df[c('partner', 'alliance')]
    df <- group_by(df, partner) %>% summarise(alliance = sum(alliance))
    names(df) <- c('partner', paste0('alliance_', r))
    
    assign(paste0('df', r), df, envir = parent.frame())
  }
  
  alliance <- full_join(df1, df2, by = c('partner'))
  alliance$alliance_1[is.na(alliance$alliance_1)] <- 0
  alliance$alliance_2[is.na(alliance$alliance_2)] <- 0
  
  alliance$preference <- NA
  alliance$preference[alliance$alliance_1 > alliance$alliance_2] <- region1
  alliance$preference[alliance$alliance_2 > alliance$alliance_1] <- region2
  
  alliance <- alliance[c('partner', 'preference')]
  return(alliance)
}

# Link embodied trade back to variable cost #
#############################################
add_var_cost <- function(region1, region2, indirect_effects = TRUE) {
  
  var_cost <- var_cost.base
  assign('allies', get(paste0('alliance_', region1, '_', region2)))
  
  var_cost$importer <- toupper(substr(var_cost$technology, nchar(var_cost$technology)-2, nchar(var_cost$technology)))
  var_cost$exporter <- toupper(stringr::str_replace(var_cost$node_loc, 'R14_', ''))
  
  # Region preferences based on historical alliance
  var_cost <- left_join(var_cost, allies, by = c('importer' = 'partner'))
  var_cost <- left_join(var_cost, allies, by = c('exporter' = 'partner'))
  names(var_cost) <- stringr::str_replace_all(names(var_cost), '\\.x', '.importer')
  names(var_cost) <- stringr::str_replace_all(names(var_cost), '\\.y', '.exporter')
  
  # For regions that tradw with region1 or region2
  var_cost$add_cost <- 0
  
  # A little more friction when trading with non-allied sanction region
  if (indirect_effects == TRUE) {
    var_cost$add_cost[var_cost$preference.exporter == region1 &
                      var_cost$importer == region2 & 
                      var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 0.2
    var_cost$add_cost[var_cost$preference.importer == region1 &
                      var_cost$exporter == region2 & 
                      var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 0.2
    
    var_cost$add_cost[var_cost$preference.exporter == region2 &
                        var_cost$importer == region1 &
                        var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 0.2
    var_cost$add_cost[var_cost$preference.importer == region2 &
                        var_cost$exporter == region1 &
                        var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 0.2
    
    var_cost$add_cost[is.na(var_cost$preference.importer) & is.na(var_cost$preference.exporter) & 
                        var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 0.2
  } 
  
  # Make prohibitively expensive for sanctioning regions for 5 years
  var_cost$add_cost[var_cost$importer == region1 & var_cost$exporter == region2 & 
                    var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 100000
  var_cost$add_cost[var_cost$importer == region2 & var_cost$exporter == region1 &
                    var_cost$year_act > 2015 & var_cost$year_act < 2030] <- 100000
  
  # Make it still a little expensive over time
  var_cost$add_cost[var_cost$importer == region1 & var_cost$exporter == region2 & 
                      var_cost$year_act > 2025] <- 0.1
  var_cost$add_cost[var_cost$importer == region2 & var_cost$exporter == region1 &
                      var_cost$year_act > 2025] <- 0.1

  var_cost$add_cost[is.na(var_cost$add_cost)] <- 0
  
  var_cost$value <- var_cost$value + (var_cost$add_cost*var_cost$value)
  var_cost <- var_cost[names(var_cost.base)]
  
  return(var_cost)
}

# Run programs #
################
# CPA-NAM sanctions
alliance_CPA_NAM <- assign_premium('CPA', 'NAM')
cpa_nam_var_cost <-add_var_cost('CPA', 'NAM')

for (t in export_technologies) {
  df <- subset(cpa_nam_var_cost, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/NAM_CPA_sanction/var_cost/', t, '.csv')))
}

# CPA-PAO sanctions
alliance_CPA_PAO <- assign_premium('CPA', 'PAO')
cpa_pao_var_cost <-add_var_cost('CPA', 'PAO')

for (t in export_technologies) {
  df <- subset(cpa_pao_var_cost, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/CPA_PAO_sanction/var_cost/', t, '.csv')))
}

# NAM-MEA sanctions
alliance_NAM_MEA <- assign_premium('NAM', 'MEA')
nam_mea_var_cost <-add_var_cost('NAM', 'MEA')

for (t in export_technologies) {
  df <- subset(nam_mea_var_cost, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/NAM_MEA_sanction/var_cost/', t, '.csv')))
}

# NAM-MEA sanctions, no indirect effects
alliance_NAM_MEA2 <- assign_premium('NAM', 'MEA')
nam_mea_var_cost2 <-add_var_cost('NAM', 'MEA', indirect_effects = FALSE)

for (t in export_technologies) {
  df <- subset(nam_mea_var_cost2, grepl(t, technology))
  write.csv(df, file.path(output, paste0('SCENARIOS/NAM_MEA_sanction_onlydirect/var_cost/', t, '.csv')))
}