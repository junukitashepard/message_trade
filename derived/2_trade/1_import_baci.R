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
energyhs4 <- read.csv(file.path(raw, "BACI/energy2hs4.csv"), stringsAsFactors = F)
energyhs4$hs4 <- as.character(energyhs4$hs4)
energyhs4 <- subset(energyhs4, !is.na(hs4))

# Import country names
countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F)
countries$X <- NULL
countries <- subset(countries, !is.na(subregion))

# Import specific energy
spec_energy <- read.csv(file.path(raw, "ConversionTables/energy_content_of_fuels/fuel_energy_content.csv"),
                        stringsAsFactors = F)
names(spec_energy) <- c('energy_type', 'specific_energy', 'region')

# Link specific energy to countries dataframe
se_country <- subset(spec_energy, nchar(region) == 3) # country-specific
se_region <- subset(spec_energy, nchar(region) > 3) # region-specific

se.df <- data.frame(web.country = character(0), iso.country = character(0), baci.country = integer(0),
                     region = character(0), subregion = character(0),
                     specific_energy = numeric(0), energy = character(0))

for (e in c('BIO', 'CRU', 'COAL', 'NG', 'PET', 'NUC')) {
  print(paste0("Linking: ", e))
  df.c <- subset(se_country, energy_type == e)[c('specific_energy', 'region')]
  df.r <- subset(se_region, energy_type == e)[c('specific_energy', 'region')]

  if (nrow(df.c) == 1 & df.c$region == 'all') {
    df.out <- countries
    df.out$specific_energy <- df.c$specific_energy[1]
  } else {
    df.out <- left_join(countries, df.c, by = c('iso.country' = 'region'))
    df.out$specific_energy[is.na(df.out$specific_energy)] <- 0
    for (r in unique(df.r$region)) {
      print(r)
      df.out$specific_energy[df.out$specific_energy == 0 & df.out$subregion == r] <-
        df.r$specific_energy[df.r$region == r]
    }
  }

  assert('!is.na(df.out$specific_energy)')
  df.out$energy <- e
  names(df.out) <- c('web.country', 'iso.country', 'baci.country', 'region', 'subregion', 'specific_energy', 'energy')
  se.df <- rbind(se.df, df.out)
}

se.df <- se.df[c('baci.country', 'energy', 'specific_energy')] # keep relevant variables
rm(list = c('df.c', 'df.out', 'df.r', 'se_country', 'se_region', 'spec_energy'))


# Function "trade2physical": convert quantity (t) to energy flows using region-specific specific energy values
trade2physical <- function(year) {

  print(paste0("Converting trade data, year = ", year))

  # Import trade (BACI) data from SQL database
  sql_statement <- paste0("SELECT * FROM BACI_TRADE.BACI_", year, " ",
                          "WHERE LEFT(HS6, 2) = '27' OR
                          LEFT(HS6,2) = '38' OR
                          LEFT(HS6,2) = '28'")
  quiet(
  import_sql(statement = sql_statement,
             user = 'root',
             password = 'SEAmonst3r!',
             dbname = 'BACI_TRADE',
             outdf = 'trade'
  ))

  trade$hs4 <- substr(trade$hs6, 1, 4)
  trade <- left_join(trade, energyhs4, by = c('hs4'))
  trade <- subset(trade, !is.na(energy))

  # Link specific energy by exporting country
  trade <- left_join(trade, se.df, by = c('i' = 'baci.country', 'energy' = 'energy'))
  environment(assert) <- environment(isid) <- environment()
  trade <- subset(trade, grepl('ELEC', energy) == F)

  trade <- subset(trade, i != 652 & j != 652) # country 652 does not exist

  assert('!is.na(trade$specific_energy)')

  # Collapse to i-j-year-energy level
  trade <- dplyr::group_by(trade, i, j, t, energy, specific_energy) %>%
           dplyr::summarise(v = sum(v, na.rm = T),
                            q = sum(q, na.rm = T))

  trade$q_e <- trade$q * trade$specific_energy #TJ energy flow

  # Add country names
  trade <- left_join(trade, countries[c('baci.country', 'iso.country')], by = c('i' = 'baci.country'))
  trade <- left_join(trade, countries[c('baci.country', 'iso.country')], by = c('j' = 'baci.country'))
  names(trade)[9:10] <- c('iso.i', 'iso.j')

  assign(paste0('trade.', year), trade, envir = parent.frame())
}

# Function "collapsebytype": collapse to imports and exports for IEA data validation
collapsebytype <- function(year) {
  print(paste0("Collapsing data, year = ", year))

  assign('df', get(paste0('trade.', year)))

  # Collapse to imports and exports separately
  assign('imports', dplyr::group_by(df, j, iso.j, t, energy) %>%
                            dplyr::summarise(v = sum(v, na.rm = T),
                                             q= sum(q, na.rm = T),
                                             q_e = sum(q_e, na.rm = T)))

  assign('exports', dplyr::group_by(df, i, iso.i, t, energy) %>%
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
trade2physical(year = 1995)
collapsebytype(year = 1995)

trade <- trade.1995
trade_bytype <- trade_bytype.1995

for (y in 1996:2014) {
  trade2physical(year = y)
  collapsebytype(year = y)

  assign('trade', rbind(as.data.frame(trade), as.data.frame(get(paste0('trade.', y)))))
  assign('trade_bytype', rbind(as.data.frame(trade_bytype), as.data.frame(get(paste0('trade_bytype.', y)))))
}

# Write files
saveRDS(trade, file.path(temp, 'trade.rds'))
saveRDS(trade_bytype, file.path(temp, 'trade_bytype.rds'))

