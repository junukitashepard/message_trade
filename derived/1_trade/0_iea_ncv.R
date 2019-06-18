###################################################
# Import and standardize IEA net calorific values #
###################################################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('RMySQL')

output <-   paste0(wd, "output/")
temp <-     paste0(wd, "temp/")
raw <-      paste0(wd, "raw/")

library('magrittr')
library('dplyr')
library('ggplot2')
library('stringr')
library('jsfunctions')

# Import file
cfile <- read.csv(file.path(raw, "IEA/WCONV_13062019131229715.csv"), stringsAsFactors = F)
cfile <- cfile[c('COUNTRY', 'Product', 'TIME', 'Value')]
names(cfile) <- c('iso.country', 'product', 'year', 'ncv')

# Convert to TJ/t (currently in kJ/kg)
cfile$ncv <- (cfile$ncv/10^9)*10^3

# Make sure file is unique by country energy year
isid('cfile', c('iso.country', 'product', 'year'))

cfile$energy <- NA

cfile$energy[cfile$product %in% c('Anthracite', 'Coking coal', 'Lignite', 'Other bituminous coal', 'Sub-bituminous coal')] <- 'COAL'
cfile$energy[cfile$product %in% c('Crude oil', 'Crude/NGL/feedstocks/non-crude (if no detail)', 'Oil shale and oil sands')] <- 'CRU'
cfile$energy[cfile$product %in% c('Ethane', 'Liquefied petroleum gases (LPG)', 'Natural gas liquids')] <- 'NG'
cfile$energy[cfile$product %in% c('Aviation gasoline', 'Fuel oil', 'Gas/diesel oil excl. biofuels', 'Gasoline type jet fuel',
                                  'Kerosene type jet fuel excl. biofuels', 'Motor gasoline excl. biofuels', 
                                  'Other kerosene', 'Other oil products')] <- 'PET'
cfile$energy[cfile$product %in% c('Biodiesels', 'Charcoal', 'Peat', 'Peat products')] <- 'BIO'

cfile <- subset(cfile, !is.na(energy))

# Hold NCV constant for all petroleum, all natural gas, all crude (only coal differentiate by type)
cfile$product[cfile$energy == 'PET'] <- 'petroleum'
cfile$product[cfile$energy == 'NG'] <- 'ng'
cfile$product[cfile$energy == 'CRU'] <- 'crude'

# For bioenergy, separate biodiesels, charcoal, peat
cfile$product[grepl('Peat', cfile$product)] <- 'Peat'
cfile$product <- tolower(cfile$product)

# Hold NCV constant by country across all years (1995-2017)
cfile <- dplyr::group_by(cfile, iso.country, product, energy) %>%
  dplyr::summarise(ncv = mean(ncv, na.rm = T))
cfile$ncv[is.nan(cfile$ncv)] <- NA

# Include NCV by region where NCV is missing for country
regions <- read.delim(file.path(raw, "ConversionTables/countries_regions.txt"), stringsAsFactors = F)
regions <- unique(regions[c('isoAlphaThree', 'unRegionSubregionName')])
names(regions) <- c('iso.country', 'region')

cfile <- left_join(cfile, regions, by = c('iso.country'))

# Write file
saveRDS(cfile, file.path(temp, 'ncv_convert.rds'))


