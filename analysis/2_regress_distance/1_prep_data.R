############################## 
# How important is distance? #
# Pre-process data           #
##############################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output/derived")
output <-   paste0(wd, "output/analysis/distance_regress")
temp <-     paste0(wd, "temp/")

##############################
# Import files
trade <- readRDS(file.path(input, 'trade/trade.rds'))
  isid('trade', c('iso.i', 'iso.j', 'energy', 'year'))
  
ijports <- readRDS(file.path(input, 'nodes/ij_ports_fixednames.rds'))
  ijports <- unique(ijports[c('port1.iso', 'port2.iso', 'port1', 'port2', 'distance')])
  isid('ijports', c('port1.iso', 'port2.iso'))
  
regional_spec <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
  names(regional_spec) <- c('iso', 'msg.region')
  regional_spec <- subset(regional_spec, iso != "" & !is.na(iso))
  isid('regional_spec', c('iso'))

# Link regions to trade
trade <- left_join(trade, regional_spec, by = c('iso.i' = 'iso'))
trade <- left_join(trade, regional_spec, by = c('iso.j' = 'iso'))
names(trade)[10:11] <- c('msg.region.i', 'msg.region.j')

# Link ports to trade data
trade <- left_join(trade, ijports, by = c('iso.i' = 'port1.iso', 
                                          'iso.j' = 'port2.iso'))

trade <- subset(trade, !is.na(distance)) # Remove once node calculation is fixed

# Write file
saveRDS(trade, file.path(output, "distance_regdf.rds"))

