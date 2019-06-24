#########################################
# Create trade dispute matrices by year #
#########################################
# Data on trade disputes from Bown and Reynolds (2014), WTO

rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')

raw <-      paste0(wd, "raw")
output <-   paste0(wd, "output/derived/trade/")
temp <-     paste0(wd, "temp/")

# Import files 
###############
# Trade disputes dataset
td.dates <- read.csv(file.path(raw, 'TradeDisputes/bown_reynolds_tftd_july_2014_database/wto_master.csv'), stringsAsFactors = F)
td.partners <- read.csv(file.path(raw, 'TradeDisputes/bown_reynolds_tftd_july_2014_database/wto_partners.csv'), stringsAsFactors = F)

# Country list
countries <- read.csv(file.path(raw, 'ConversionTables/web_countries.csv'), stringsAsFactors = F)

# EU countries
eu_nations <- read.csv(file.path(raw, 'ConversionTables/eu_nations.csv'), stringsAsFactors = F)
  names(eu_nations) <- seq(from = 1990, to = 2015, by = 1)
  
# Pre-process TD data
#####################
# Clean dates
td.dates <- td.dates[c('DS_NO_COMMON', 'RESP_CODE', 'YEAR_FIRST_VIOLATION', 'YEAR_LAST_VIOLATION')]
names(td.dates) <- c('ds_no', 'i', 'year_start', 'year_end')

td.dates$year_start <- as.numeric(td.dates$year_start)
td.dates$year_end <- as.numeric(td.dates$year_end)

td.dates$year_start[is.na(td.dates$year_start) & !is.na(td.dates$year_end)] <- 
  td.dates$year_start[is.na(td.dates$year_start) & !is.na(td.dates$year_end)]

td.dates <- unique(td.dates)

# Clean partners
td.partners <- td.partners[, 1:2]
names(td.partners) <- c('ds_no', 'j')

# Link partners and dates
td <- full_join(td.dates, td.partners, by = c('ds_no'))

td <- subset(td, !is.na(year_start))

td <- subset(td, i != j)

td$i[td$i == "TWN"] <- td$j[td$j == "TWN"] <- "NULL" # Taiwan = Other Asia, NES (NULL)

td <- subset(td, i != 'SWZ' & j != 'SWZ') # Swaziland data not available in trade/IEA

# Build matrices
################
base_mat <- matrix(0, nrow = length(unique(countries$iso.country)), ncol = length(unique(countries$iso.country)))
colnames(base_mat) <- rownames(base_mat) <- unique(countries$iso.country)

# Fill matrix
fill_byear <- function(year) {
  
  assign('mat', base_mat)
  assign('df', subset(td, year_start <= year & year_end >= year))
  assign('eu', unique(eu_nations[, as.character(year)]))
    eu <- eu[eu != "" & eu != "LUX" & eu != 'ROM'] # Luxenburg + Romania data unavailable in trade/WEB
    
  for (r in 1:nrow(df)) {
    print(r)
    
    if (df$i[r] == "EUN") {
      for (c in eu) {
        mat[c, df$j[r]] <- mat[c, df$j[r]] + 1
        mat[df$j[r], c] <- mat[df$j[r], c] + 1
      }
    } else if (df$j[r] == "EUN") {
      for (c in eu) {
        print(c)
        mat[c, df$i[r]] <- mat[c, df$i[r]] + 1
        mat[df$i[r], c] <- mat[df$i[r], c] + 1
      }
    } else {
      mat[df$i[r], df$j[r]] <- mat[df$i[r], df$j[r]] + 1
      mat[df$j[r], df$i[r]] <- mat[df$j[r], df$i[r]] + 1
    }
  }
  return(mat)
}

# Run function
for (y in 1990:2010) {
  print(paste0("####RUNNING YEAR = ", y, "####"))
  assign(paste0('mat_', y), fill_byear(y), envir = parent.frame())
}
  

