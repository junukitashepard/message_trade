##################################### 
# How important are trade disputes? #
# Run regression                    #
#####################################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')
library('openxlsx')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output/derived/trade")
output <-   paste0(wd, "output/analysis/distance_regress")
temp <-     paste0(wd, "temp/")

##############################
# Import file
trade <- readRDS(file.path(input, 'trade.rds'))
  isid('trade', c('iso.i', 'iso.j', 'energy', 'year'))
  
td <- readRDS(file.path(input, "trade_disputes.rds"))
  names(td) <- c('i', 'j', 'year', 'ds_outcome', 'ds_energy', 'ds_any')
  all.outcomes <- c('Dropped', 'In progress', 'MAS', 'Ruling', 'Withdrawn')
  
# Link files
trade <- subset(trade, year > 1995 & year < 2014)
trade$l1.year <- trade$year - 1

td.outcomes <- all.outcomes # make flexible
td.link <- subset(td, ds_outcome %in% td.outcomes)[c('i', 'j', 'year', 'ds_energy', 'ds_any')]

trade <- left_join(trade, td.link, by = c('iso.i' = 'i', 'iso.j' = 'j', 'l1.year' = 'year'))
trade$ds_any[is.na(trade$ds_any)] <- 0
trade$ds_energy[is.na(trade$ds_energy)] <- 0

# Simple regression function
distreg <- function(export.region = "all", import.region = "all", energy.type = "all",
                    full.summary = FALSE) {
  
  assign('regdf', trade)
  
  assign('form', "q_e ~ ds_any + factor(iso.i) + factor(iso.j) + factor(year)")
  
  if (export.region != "all") {regdf <- subset(regdf, msg.region.i %in% export.region)}
  if (import.region != "all") {regdf <- subset(regdf, msg.region.j %in% import.region)}
  if (energy.type != "all") {regdf <- subset(regdf, energy %in% energy.type)}
  
  if (energy.type == "all") {
    form <- paste0(form, " + factor(energy)")
  }
  
  assign('m', lm(as.formula(form), data = regdf))
  assign('coef', summary(m)$coefficients['ds_any',])
  
  # Add mean(Y)
  assign('meany', median(regdf$q_e))
  
  coef <- c(coef, meany)
  
  if (full.summary == TRUE) {
    return(summary(m))
  } else {
    return(coef)
  }
  
}