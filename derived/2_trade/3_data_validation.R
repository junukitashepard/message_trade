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

# Export csv for tableau
write.csv(all.df, file.path(output, 'derived/data_validation/iea_trade_data.csv'))
