#########################################
# Create trade dispute matrices by year #
#########################################
# Data on trade disputes from Kucik and Pelc (2019), WTO

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
output.matrix <- paste0(wd, "output/derived/matrices")
temp <-     paste0(wd, "temp/")

# Import files 
###############
# Trade disputes dataset
td.dates <- read.csv(file.path(raw, 'TradeDisputes/kucik_pelc_2019/Dates.csv'), stringsAsFactors = F)
td.products <- read.csv(file.path(raw, 'TradeDisputes/kucik_pelc_2019/Products.csv'), stringsAsFactors = F)
td.partners <- read.csv(file.path(raw, 'TradeDisputes/kucik_pelc_2019/Participants.csv'), stringsAsFactors = F)
names(td.dates)[1] <- names(td.products)[1] <- names(td.partners)[1] <- 'Dispute_No'

# Country list
countries <- read.csv(file.path(raw, 'ConversionTables/web_countries.csv'), stringsAsFactors = F)

# EU countries
eu_nations <- read.csv(file.path(raw, 'ConversionTables/eu_nations.csv'), stringsAsFactors = F)
  names(eu_nations) <- seq(from = 1990, to = 2015, by = 1)
  
# Pre-process TD data
#####################
# Clean dates
td.dates <- td.dates[c('Dispute_No', 'Outcome', 'Start_Year', 'End_Year')]
names(td.dates) <- c('ds_no', 'outcome', 'year_start', 'year_end')

td.dates$year_start <- as.numeric(td.dates$year_start)
td.dates$year_end <- as.numeric(td.dates$year_end)

td.dates$year_start[is.na(td.dates$year_start) & !is.na(td.dates$year_end)] <- 
  td.dates$year_start[is.na(td.dates$year_start) & !is.na(td.dates$year_end)]

td.dates <- unique(td.dates)
isid('td.dates', c('ds_no'))

# Clean partners
td.partners <- left_join(td.partners, countries, by = c('Participant' = 'web.country'))
td.partners <- subset(td.partners, Participant != "Swaziland" & Participant != "Namibia")
assert('unique(td.partners$Participant[is.na(td.partners$iso.country)]) == "EU"')
td.partners$iso.country[is.na(td.partners$iso.country)] <- 'EU'

td.partners.i <- subset(td.partners, Position == 'respondent')[c('Dispute_No', 'iso.country')]
td.partners.j <- subset(td.partners, Position == 'complainant')[c('Dispute_No', 'iso.country')]
names(td.partners.i) <- c('ds_no', 'i')
names(td.partners.j) <- c('ds_no', 'j')

td.partners <- inner_join(td.partners.i, td.partners.j, by = c('ds_no'))

# Clean products 
td.products <- td.products[c('Dispute_No', 'Product_Codes')]
td.products$hs2 <- substr(td.products$Product_Codes, 1, 2)

td.products$energy <- 0
td.products$energy[td.products$hs2 %in% c('22', '27', '28', '38')] <- 1

td.products <- dplyr::group_by(td.products, Dispute_No) %>%
               dplyr::summarize(energy = max(energy))
names(td.products) <- c('ds_no', 'energy')
td.products$ds_no <- as.numeric(td.products$ds_no)

# Link files
td <- inner_join(td.dates, td.partners, by = c('ds_no'))
td <- inner_join(td, td.products, by = c('ds_no'))

td <- subset(td, !is.na(year_start))

td <- subset(td, i != j)

td$i[td$i == "TWN"] <- td$j[td$j == "TWN"] <- "NULL" # Taiwan = Other Asia, NES (NULL)

td <- subset(td, i != 'SWZ' & j != 'SWZ') # Swaziland data not available in trade/IEA

td$any <- 1

td.out <- dplyr::group_by(td, i, j, year_start, outcome) %>%
          dplyr::summarise(energy = sum(energy),
                           any = sum(any))
saveRDS(td.out, file.path(output, 'trade_disputes.rds'))

# Build matrices
################
base_mat <- matrix(0, nrow = length(unique(countries$iso.country)), ncol = length(unique(countries$iso.country)))
colnames(base_mat) <- rownames(base_mat) <- unique(countries$iso.country)

# Fill matrix
fill_byear <- function(year, type = "any") {
  
  assign('mat_e', base_mat)
  assign('mat_a', base_mat)
  assign('df', subset(td, year_start <= year & year_end >= year))
  
  if (type != 'any') {df <- subset(df, outcome == type)}
  df <- dplyr::group_by(df, i, j) %>%
        dplyr::summarise(energy = sum(energy), any = sum(any))
  
  assign('eu', unique(eu_nations[, as.character(year)]))
    eu <- eu[eu != "" & eu != "LUX" & eu != 'ROM'] # Luxembourg + Romania data unavailable in trade/WEB
    
  for (r in 1:nrow(df)) {
    print(r)
    
    if (df$i[r] == "EU") {
      for (c in eu) {
        mat_e[c, df$j[r]] <- mat_e[c, df$j[r]] + df$energy[r]
        mat_e[df$j[r], c] <- mat_e[df$j[r], c] + df$energy[r]
        mat_a[c, df$j[r]] <- mat_a[c, df$j[r]] + df$any[r]
        mat_a[df$j[r], c] <- mat_a[df$j[r], c] + df$any[r]
      }
    } else if (df$j[r] == "EU") {
      for (c in eu) {
        print(c)
        mat_e[c, df$i[r]] <- mat_e[c, df$i[r]] + df$energy[r]
        mat_e[df$i[r], c] <- mat_e[df$i[r], c] + df$energy[r]
        mat_a[c, df$i[r]] <- mat_a[c, df$i[r]] + df$any[r]
        mat_a[df$i[r], c] <- mat_a[df$i[r], c] + df$any[r]
      }
    } else {
      mat_e[df$i[r], df$j[r]] <- mat_e[df$i[r], df$j[r]] + df$energy[r]
      mat_e[df$j[r], df$i[r]] <- mat_e[df$j[r], df$i[r]] + df$energy[r]
      mat_a[df$i[r], df$j[r]] <- mat_a[df$i[r], df$j[r]] + df$any[r]
      mat_a[df$j[r], df$i[r]] <- mat_a[df$j[r], df$i[r]] + df$any[r]
    }
  }
  assign(paste0('mat_E.', year), mat_e, envir = parent.frame())
  assign(paste0('mat_A.', year), mat_a, envir = parent.frame())
  
  saveRDS(mat_e, file.path(output.matrix, paste0("trade_disputes/td_ENE_", year, ".rds")))
  saveRDS(mat_a, file.path(output.matrix, paste0("trade_disputes/td_ANY_", year, ".rds")))
}

# Run function
for (y in 1995:2014) {
  print(paste0("####RUNNING YEAR = ", y, "####"))
  fill_byear(y)
}
  

