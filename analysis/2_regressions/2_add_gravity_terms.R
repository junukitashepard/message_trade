########################### 
# Add gravity model terms #
###########################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')
library('openxlsx')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output/analysis/regress")
output <-   paste0(wd, "output/analysis/regress")
temp <-     paste0(wd, "temp/")

##############################
# Import file
trade <- readRDS(file.path(input, "regdf.rds"))

gravity_1993_2004 <- read.csv(file.path(raw, 'USITC/release_1.0_1993_2004.csv'), stringsAsFactors = F)
gravity_2005_2016 <- read.csv(file.path(raw, 'USITC/release_1.0_2005_2016.csv'), stringsAsFactors = F)
gravity <- rbind(gravity_1993_2004, gravity_2005_2016)
rm(list = c('gravity_1993_2004', 'gravity_2005_2016'))

# Keep relevant var in gravity and rename
gravity <- gravity[c('year', 'iso3_o', 'iso3_d', 
                     'landlocked_o', 'island_o', 'gdp_pwt_const_o', 'pop_o',
                     'landlocked_d', 'island_d', 'gdp_pwt_const_d', 'pop_d',
                     'contiguity', 'agree_cu', 'agree_eia', 'agree_fta', 'agree_psa', 'agree_pta', 
                     'sanction_threat', 'sanction_imposition',
                     'common_language', 'colony_of_destination_ever', 'colony_of_origin_ever')]
names(gravity) <- c('year', 'iso.i', 'iso.j',
                    'landlocked.i', 'island.i', 'gdp.i', 'pop.i',
                    'landlocked.j', 'island.j', 'gdp.j', 'pop.j',
                    'contiguity','agree_cu', 'agree_eia','agree_fta', 'agree_psa', 'agree_pta',
                    'sanction_threat', 'sanction_imposition',
                    'common_language', 'i_colony_of_j', 'j_colony_of_i')
gravity$in_gravity <- 1

# Combine gravity and trade
isid('trade', c('iso.i', 'iso.j', 'year', 'energy'))
isid('gravity', c('iso.i', 'iso.j', 'year'))

trade <- left_join(trade, gravity, by = c('iso.i', 'iso.j', 'year'))
assert('trade$in_gravity == 1')
trade$in_gravity <- NULL

# Add lags and polynomials #
############################
for (variable in c('q_e_gwa', 
                   'agree_cu', 'agree_eia', 'agree_fta', 'agree_psa', 'agree_pta', 
                   'sanction_threat', 'sanction_imposition')) {
  
  names(trade)[names(trade) == variable] <- 'var'
  
  trade <- dplyr::arrange(trade, i, j, energy, year)
  
  # Replace NA with 0 for gravity variables
  #if (variable != 'q_e_gwa') {trade$var[is.na(trade$var)] <- 0}
  
  # Add lag
  trade <- group_by(trade, i, j, energy) %>% 
           mutate(lag_var = lag(var))
  
  # Add polynomials
  trade$var_p2 <- trade$var^2
  trade$var_p3 <- trade$var^3
  
  names(trade)[names(trade) == 'var'] <- variable
  names(trade)[names(trade) == 'lag_var'] <- paste0('lag1.', variable)
  names(trade)[names(trade) == 'var_p2'] <- paste0(variable, '_p2')
  names(trade)[names(trade) == 'var_p3'] <- paste0(variable, '_p3')
}

# Save file
saveRDS(trade, file.path(output, 'regdf.rds'))
