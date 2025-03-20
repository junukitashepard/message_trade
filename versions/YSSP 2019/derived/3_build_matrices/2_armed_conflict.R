##########################################
# Create armed conflict matrices by year #
##########################################
# Data on armed conflict from UCDP Project

rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')

raw <-      paste0(wd, "raw")
output <-   paste0(wd, "output/derived/")
output.matrix <- paste0(wd, "output/derived/matrices")
temp <-     paste0(wd, "temp/")
##########################################
# Import files
cf <- read.csv(file.path(raw, 'ArmedConflict/ucdp_data/ucdp-dyadic-191.csv'), stringsAsFactors = F)
cf.loc <- read.csv(file.path(raw, 'ArmedConflict/ucdp_data/location.csv'), stringsAsFactors = F)
cf.act <- read.csv(file.path(raw, 'ArmedConflict/ucdp_data/actorlist.csv'), stringsAsFactors = F)
  cf.act$ActorID <- as.character(cf.act$ActorID)
  
# Country list
countries <- read.csv(file.path(raw, 'ConversionTables/web_countries.csv'), stringsAsFactors = F)

# Pre-process conflict data
###########################
# Compile side names (2nd)
in.a <- unique(cf$side_a_2nd)
in.b <- unique(cf$side_b_2nd)

compile_side <- function(partner) {
  assign('side_original', get(paste0('in.', partner)))
  side <- stringr::str_replace_all(side_original, "Government of ", "")
  side_original <- as.data.frame(side_original)
  side_original$side_new <- side
  
  assign('sdf', data.frame(side = character(0), country = character(0)))
  
  for (i in 1:length(side)) {
    assign('sc', side[i])
    sc <- as.vector(strsplit(sc, ",")[[1]])
    sc <- trimws(sc)
    
    assign('omat', matrix(nrow = length(sc), ncol = 2))
    omat[, 2] <- sc
    omat[, 1] <- side[i]
    omat <- as.data.frame(omat)
    names(omat) <- c('side', 'country')
    
    sdf <- rbind(as.data.frame(sdf), as.data.frame(omat))
  }
  sdf$side <- as.character(sdf$side); sdf$country <- as.character(sdf$country)
  
  # Fix country names
  sdf$country[sdf$country == 'United States of America'] <- "USA"
  sdf$country[sdf$country == 'Russia (Soviet Union)'] <- "Russian Federation"
  sdf$country[sdf$country == 'Belgium' | sdf$country == 'Luxembourg'] <- 'Belgium-Luxembourg'
  sdf$country[sdf$country == 'Czech Republic'] <- "Czech Rep."
  sdf$country[sdf$country == 'Rumania'] <- "Romania"
  sdf$country[sdf$country == 'Bosnia-Herzegovina'] <- "Bosnia Herzegovina"
  sdf$country[sdf$country == 'Macedonia' | sdf$country == 'FYR'] <- "TFYR of Macedonia"
  sdf$country[sdf$country == 'South Korea'] <- "Rep. of Korea"
  sdf$country[grepl('Viet', sdf$country)] <- "Viet Nam"
  sdf$country[grepl('Yemen', sdf$country)] <- "Yemen"
  sdf$country[grepl('DR Congo', sdf$country)] <- "Dem. Rep. of the Congo"
  sdf$country[grepl('Zimbabwe', sdf$country)] <- "Zimbabwe"
  sdf$country[grepl('Lao', sdf$country)] <- "Lao People's Dem. Rep."
  sdf$country[grepl('Yemen', sdf$country)] <- "Yemen"
  sdf$country[grepl('Cambodia', sdf$country)] <- "Cambodia"
  sdf$country[grepl('North Korea', sdf$country)] <- "Dem. People's Rep. of Korea"
  sdf$country[grepl('Tanzania', sdf$country)] <- "United Rep. of Tanzania"
  sdf$country[sdf$country == 'Dominican Republic'] <- "Dominican Rep."
  sdf$country[sdf$country == 'Ivory Coast'] <- "Cote dIvoire"
  sdf$country[sdf$country == 'Moldova'] <- "Rep. of Moldova"
  sdf$country[sdf$country == 'South Africa'] <- "So. African Customs Union"
  sdf$country[sdf$country == 'Central African Republic'] <- "Central African Rep."
  
  sdf <- left_join(sdf, countries, by = c('country' = 'web.country'))
  sdf <- left_join(sdf, side_original, by = c('side' = 'side_new'))
  sdf$side_original <- as.character(sdf$side_original)
  
  assign(paste0('side.', partner), sdf[c('side', 'side_original', 'country', 'iso.country')], envir = parent.frame())
}

compile_side('a')
compile_side('b')

cf.a2 <- unique(cf[c('conflict_id', 'side_a_2nd')])
cf.b2 <- unique(cf[c('conflict_id', 'side_b_2nd')])

cf.a2 <- left_join(cf.a2, side.a, by = c('side_a_2nd' = 'side_original'))[c('conflict_id', 'iso.country')]
cf.b2 <- left_join(cf.b2, side.b, by = c('side_b_2nd' = 'side_original'))[c('conflict_id', 'iso.country')]

names(cf.a2) <- c('conflict_id', 'side_a')
names(cf.b2) <- c('conflict_id', 'side_b')

cf.a2 <- subset(cf.a2, !is.na(side.a))
cf.b2 <- subset(cf.b2, !is.na(side.b))

# Compile side names (1st)
cf.a1 <- unique(cf[c('conflict_id', 'side_a_id')])
cf.a1 <- left_join(cf.a1, cf.act[c('ActorID', 'Country')], by = c('side_a_id' = 'ActorID'))[c('conflict_id', 'Country')]
names(cf.a1) <- c('conflict_id', 'side_a')

cf.b1 <- unique(cf[c('conflict_id', 'side_b_id')])
cf.b1 <- left_join(cf.b1, cf.act[c('ActorID', 'Country')], by = c('side_b_id' = 'ActorID'))[c('conflict_id', 'Country')]
names(cf.b1) <- c('conflict_id', 'side_b')

cf.a <- unique(rbind(cf.a1, cf.a2))
cf.b <- unique(rbind(cf.b1, cf.b2))

# Link files
cf <- unique(cf[c('conflict_id', 'year', 'intensity_level')])
cf <- inner_join(cf, cf.a, by = c('conflict_id'))
cf <- inner_join(cf, cf.b, by = c('conflict_id'))

cf <- subset(cf, !is.na(side_a) & !is.na(side_b) & side_a != "" & side_b != "")
cf <- unique(cf[c('conflict_id', 'side_a', 'side_b', 'year', 'intensity_level')])
names(cf) <- c('conflict_id', 'i', 'j', 'year', 'intensity_level')

cf$minor <- 0
cf$war <- 0

cf$minor[cf$intensity_level == 1] <- 1
cf$war[cf$intensity_level == 2] <- 1

cf <- dplyr::group_by(cf, i, j, year) %>%
      dplyr::summarise(minor = sum(minor), war = sum(war))

isid('cf', c('i', 'j', 'year'))

# Save file
saveRDS(cf, file.path(output, 'conflicts/armed_conflicts.rds'))

# Allow i-j and j-i
cf2 <- cf
names(cf2) <- c('j', 'i', 'year', 'minor', 'war')
cf2 <- cf2[c('i', 'j', 'year', 'minor', 'war')]
cf <- unique(rbind(cf, cf2))

# Form matrices
base_mat <- matrix(0, nrow = length(unique(countries$iso.country)), ncol = length(unique(countries$iso.country)))
colnames(base_mat) <- rownames(base_mat) <- unique(countries$iso.country)

fill_byear <- function(in.year) {
  print(in.year)
  
  assign('mat_M', base_mat)
  assign('mat_W', base_mat)
  assign('df', subset(cf, year == in.year))
  
  for (r in 1:nrow(df)) {
    mat_M[df$i[r], df$j[r]] <- mat_M[df$i[r], df$j[r]] + df$minor[r]
    mat_M[df$j[r], df$i[r]] <- mat_M[df$j[r], df$i[r]] + df$minor[r]
    mat_W[df$i[r], df$j[r]] <- mat_W[df$i[r], df$j[r]] + df$war[r]
    mat_W[df$j[r], df$i[r]] <- mat_W[df$j[r], df$i[r]] + df$war[r]
  }
  
  assign(paste0('mat_M.', in.year), mat_M, envir = parent.frame())
  assign(paste0('mat_W.', in.year), mat_W, envir = parent.frame())
}

for (y in 1995:2015) {
  fill_byear(y)
}
