#################################
# Dataset of embodied alliances #
#################################
rm(list = ls())
wd <- 'H:/message_trade/analysis/4_run_message/build_scenarios/'
wd.data <- 'H:/data/'
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')

input <-   paste0(wd.data, "raw/")
output <- paste0(wd.data, "output/analysis/msg_parameters/")

# Import files
dyad <- read.csv(file.path(input, 'ArmedConflict/correlates_of_war/alliance_dyad.csv'), stringsAsFactors = F)
 # dyad <- subset(dyad, dyad_st_year > 1995)
web.countries <- read.csv(file.path(input, 'ConversionTables/web_countries.csv'), stringsAsFactors = F)
msg.regions <- read.csv(file.path(input, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
  names(msg.regions) <- c('iso', 'msgregion')
  
# Clean names in dyad file
dyad_names <- c('Antigua & Barbuda', 'Belgium', 'Central African Republic', 'Czech Republic', 'Democratic Republic of the Congo',
                'Dominican Republic', 'German Federal Republic', 'Luxembourg', 'Moldova', 'North Korea', 'Russia', 'St. Kitts and Nevis',
                'St. Lucia', 'St. Vincent and the Grenadines', 'Tanzania', 'United States of America')
web_names <- c('Antigua and Barbuda', 'Belgium-Luxembourg', 'Central African Rep.', 'Czech Rep.', 'Dem. Rep. of the Congo',
               'Dominican Rep.', 'Germany', 'Belgium-Luxembourg', 'Rep. of Moldova', "Dem. People's Rep. of Korea", 
               'Russian Federation', 'Saint Kitts and Nevis', 'Saint Lucia', 'Saint Vincent and the Grenadines', 
               'United Rep. of Tanzania', 'USA')
for (i in 1:length(dyad_names)) {
  dyad$state_name1[dyad$state_name1 == dyad_names[i]] <- dyad$state_name2[dyad$state_name2 == dyad_names[i]] <- web_names[i]
}

# Put ISO values on dyad
basedf <- inner_join(dyad, web.countries[c('web.country', 'iso.country')], by = c('state_name1' = 'web.country'))
  names(basedf)[names(basedf) == 'iso.country'] <- 'iso.country1'
basedf <- inner_join(basedf, web.countries[c('web.country', 'iso.country')], by = c('state_name2' = 'web.country'))
  names(basedf)[names(basedf) == 'iso.country'] <- 'iso.country2'

basedf <- basedf[c('iso.country1', 'iso.country2', 'year')]
basedf <- unique(subset(basedf, !is.na(iso.country1) & !is.na(iso.country2)))

basedf$alliance <- 1

# Number of years in alliance by country pair
basedf <- group_by(basedf, iso.country1, iso.country2) %>% summarise(alliance = sum(alliance))

# Assign MESSAGE regions
basedf <- left_join(basedf, msg.regions, by = c('iso.country1' = 'iso'))
basedf <- left_join(basedf, msg.regions, by = c('iso.country2' = 'iso'))
names(basedf) <- c('iso.country1', 'iso.country2', 'alliance', 'msgregion1', 'msgregion2')

basedf <- subset(basedf, !is.na(msgregion1) & !is.na(msgregion2) &
                   msgregion1 != "" & msgregion2 != "")

# Collapse to MESSAGE region
basedf <- group_by(basedf, msgregion1, msgregion2) %>% summarise(alliance = sum(alliance))

basedf <- subset(basedf, msgregion1 != msgregion2)

saveRDS(basedf, file.path(output, "SCENARIOS/historical_alliances.rds"))


