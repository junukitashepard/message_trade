##############################
# Map regions coordinate map #
##############################
rm(list = ls())
wd <- "H:/data/"

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')

input <- paste0(wd, "raw/ConversionTables/")
output <- paste0(wd, 'output/analysis/message')
# Make region coordinates dataset
region_list <- c('AFR', 'CAS', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'RUS', 'SAS', 'SCS', 'UBM', 'WEU')

regions <- unique(expand.grid(region_list, region_list))
regions[] <- lapply(regions[], function(x) as.character(x))
regions <- unique(cbind(pmin(regions[,1], regions[,2]), pmax(regions[,1], regions[,2])))
regions <- as.data.frame(regions)
regions$pair_id <- 1:nrow(regions)
names(regions) <- c('msg_region_a', 'msg_region_b', 'pair_id')

coordinates <- read.csv(file.path(input, 'region_coordinates.csv'), stringsAsFactors = F)

regions <- suppressWarnings(left_join(regions, coordinates, by = c('msg_region_a' = 'region')))
regions <- suppressWarnings(left_join(regions, coordinates, by = c('msg_region_b' = 'region')))
names(regions) <- c('msg_region_a', 'msg_region_b', 'pair_id', 'lat.a', 'long.a', 'lat.b', 'long.b')

regions <- subset(regions, msg_region_a != msg_region_b)

saveRDS(regions, file.path(output, 'regions_pairs.rds'))