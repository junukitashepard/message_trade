############################################################
# Import trade datasets and collapse to energy commodities #
############################################################
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

# Import code crosswalk
energyhs4 <- read.csv(file.path(raw, "BACI/energy2hs4_MSG.csv"), stringsAsFactors = F)
energyhs4$hs4 <- as.character(energyhs4$hs4)
energyhs4 <- subset(energyhs4, !is.na(hs4))
energyhs4$hs6 <- as.character(energyhs4$hs6)

energyhs4$tra.energy <- energyhs4$energy
energyhs4$energy <- energyhs4$msg.energy # USE MESSAGE ENERGY COMMODITIES 

# Import country crosswalk
countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F)
countries <- countries[c('iso.country', 'web.country', 'baci.country')]

# Import NCV values from IEA
ncv <- readRDS(file.path(temp, 'ncv_convert.rds'))

ncv.r <- dplyr::group_by(ncv, product, energy) %>%
  dplyr::summarise(ncv = mean(ncv, na.rm = T)) # Where IEA missing country data, use overall average
names(ncv.r) <- c('product', 'energy', 'global.ncv')

# Function "trade2physical": convert quantity (t) to energy flows using region-specific specific energy values
trade2physical <- function(y) {

  environment(assert) <- environment()
  
  print(paste0('Importing: ', y))
  
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
  baci <- left_join(baci, energyhs4, by = c('hs6'))
  baci <- subset(baci, !is.na(energy))
  baci <- left_join(baci, countries, by = c('i' = 'baci.country')) # exporter only

  # Convert q to energy (TJ) using NCV from IEA 
  baci <- left_join(baci, ncv, by = c('iso.country', 'hs6_desc' = 'product', 'tra.energy' = 'energy'))
  baci <- left_join(baci, ncv.r, by = c('hs6_desc' = 'product', 'tra.energy' = 'energy'))

  baci$global.ncv[baci$energy == 'NUC'] <- 500 # Uranium is a global heating value
  
  baci$ncv[is.na(baci$ncv)] <- baci$global.ncv[is.na(baci$ncv)]
  baci$global.ncv <- NULL

  check <- subset(baci, is.na(ncv))
  assert('nrow(check) == 0 | (check$energy == "elec")')
  
  baci$q_e <- baci$ncv * baci$q

  # Collapse to i-j-year-energy level
  baci <- dplyr::group_by(baci, i, j, t, energy) %>%
    dplyr::summarise(v = sum(v, na.rm = T),
                     q = sum(q, na.rm = T),
                     q_e = sum(q_e, na.rm = T))
  names(baci) <- c('i', 'j', 'year', 'energy', 'v', 'q', 'q_e')

  # Add country names
  baci <- left_join(baci, countries[c('baci.country', 'iso.country')], by = c('i' = 'baci.country'))
  baci <- left_join(baci, countries[c('baci.country', 'iso.country')], by = c('j' = 'baci.country'))
  names(baci)[names(baci) == 'iso.country.x'] <- 'iso.i'
  names(baci)[names(baci) == 'iso.country.y'] <- 'iso.j'
  
  assign(paste0('trade.', y), baci, envir = parent.frame())
}

# Function "collapsebytype": collapse to imports and exports for IEA data validation
collapsebytype <- function(year) {
  print(paste0("Collapsing data, year = ", year))
  
  assign('df', get(paste0('trade.', year)))
  
  # Collapse to imports and exports separately
  assign('imports', dplyr::group_by(df, j, iso.j, year, energy) %>%
           dplyr::summarise(v = sum(v, na.rm = T),
                            q= sum(q, na.rm = T),
                            q_e = sum(q_e, na.rm = T)))
  
  assign('exports', dplyr::group_by(df, i, iso.i, year, energy) %>%
           dplyr::summarise(v = sum(v, na.rm = T),
                            q = sum(q, na.rm = T),
                            q_e = sum(q_e, na.rm = T)))
  names(exports) <- names(imports) <- c('baci.country', 'iso.country', 'year', 'energy', 'v', 'q', 'q_e')
  exports$trade <- 'exports'
  imports$trade <- 'imports'
  
  assign(paste0('trade_bytype.', year),
         rbind(as.data.frame(exports), as.data.frame(imports)),
         envir = parent.frame())
  
}

# Run programs by year
trade2physical(1995)
collapsebytype(1995)

trade <- trade.1995
trade_bytype <- trade_bytype.1995

for (y in 1996:2014) {
  trade2physical(y)
  collapsebytype(y)
  
  assign('trade', rbind(as.data.frame(trade), as.data.frame(get(paste0('trade.', y)))))
  assign('trade_bytype', rbind(as.data.frame(trade_bytype), as.data.frame(get(paste0('trade_bytype.', y)))))
}

# Write files
saveRDS(trade, file.path(output, 'derived/trade/trade.rds'))
saveRDS(trade_bytype, file.path(temp, 'trade_bytype.rds'))



