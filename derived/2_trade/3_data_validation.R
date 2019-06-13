###############################
# Compile data for validation #
###############################
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

# Import files
baci <- readRDS(file.path(temp, 'trade_bytype.rds'))
  isid('baci', c('iso.country', 'year', 'energy', 'trade'))
web <- readRDS(file.path(temp, 'iea_web_trade.rds'))
  isid('web', c('iso.country', 'flow', 'year', 'energy'))

# Oragnize files for merge
baci <- baci[c('iso.country', 'year', 'trade', 'energy', 'v', 'q', 'q_e')]
web <- web[c('iso.country', 'year', 'flow', 'energy', 'value')]
  names(web) <- c('iso.country', 'year', 'trade', 'energy', 'iea_value')
  web$trade <- tolower(web$trade)

all.df <- left_join(baci, web, by = c('iso.country', 'year', 'trade', 'energy'))

# List of countries that are not in IEA
not_iea <- dplyr::group_by(all.df, iso.country) %>%
           dplyr::summarize(max_iea = max(iea_value, na.rm = T))
not_iea <- subset(not_iea, max_iea == 0 | is.infinite(max_iea))

countrynames <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), 
                         stringsAsFactors = F)[c('iso.country', 'web.country')]
not_iea <- left_join(not_iea, countrynames, by = c('iso.country'))

# Export csv for tableau
write.csv(all.df, file.path(output, 'derived/data_validation/iea_trade_data.csv'))
