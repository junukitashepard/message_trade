############################## 
# How important is distance? #
# Run regression             #
##############################
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
input <-    paste0(wd, "output/analysis/distance_regress")
output <-   paste0(wd, "output/analysis/distance_regress")
temp <-     paste0(wd, "temp/")

##############################
# Import file
trade <- readRDS(file.path(input, "distance_regdf.rds"))
trade$distance <- trade$distance/1000

trade$var_cost[is.infinite(trade$var_cost)] <- NA
trade <- subset(trade, !is.na(var_cost) & !is.infinite(var_cost))

# Simple regression function
distreg <- function(export.region = "all", import.region = "all", energy.type = "all",
                    full.summary = FALSE) {
  
  assign('regdf', trade)
  
  assign('form', "var_cost ~ distance + factor(iso.i) + factor(iso.j) + factor(year)")
  
  if (export.region != "all") {regdf <- subset(regdf, msg.region.i %in% export.region)}
  if (import.region != "all") {regdf <- subset(regdf, msg.region.j %in% import.region)}
  if (energy.type != "all") {regdf <- subset(regdf, energy %in% energy.type)}
  
  if (energy.type == "all") {
    form <- paste0(form, " + factor(energy)")
  }
  
  assign('m', lm(as.formula(form), data = regdf))
  assign('coef', summary(m)$coefficients['distance',])
  
  # Add mean(Y)
  assign('meany', median(regdf$q_e))
  
  coef <- c(coef, meany)
  
  if (full.summary == TRUE) {
    return(summary(m))
  } else {
    return(coef)
  }
  
}

# Run function with different specifications and compile table
##############################################################
# Function: run for all regions and by region
run_reg <- function(exporters = "all", importers = "all") {
  
  assign('tab.names', c('base'))
  
  assign('model_base', distreg(export.region = exporters, import.region = importers))
  
  for (e in c('BIO', 'COAL', 'CRU', 'NG', 'PET')) {
    assign('d.coef', distreg(export.region = exporters, import.region = importers, energy.type = e))
    model_base <- rbind(model_base, d.coef)
    tab.names <- c(tab.names, e)
  }
  rownames(model_base) <- tab.names
  write.csv(model_base, file.path(output, 
                                  paste0("M_", importers, "_X_", exporters, ".csv")))
  return(model_base)
}

wb <- loadWorkbook(file.path(output, 'distance_regression.xlsx'))

# All regions
all_regions <- run_reg()
all_regions <- as.data.frame(all_regions)

writeData(wb, sheet = "All regions", all_regions, 
          startRow = 3, startCol = 2,
          colNames = FALSE, rowNames = FALSE)

# By importing region
region.list <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'SAS', 'WEU')

i <- 3
for (r in region.list) {
  print(paste0("Running regression for importer = ", r))
  assign(paste0('M_mat'), run_reg(importers = r))
  M_mat <- as.data.frame(M_mat)
  writeData(wb, sheet = "Importing region", M_mat,
            startRow = i, startCol = 2,
            colNames = FALSE, rowNames = FALSE)
  i <- i + 9
}

# By exporting region
i <- 3
for (r in region.list) {
  print(paste0("Running regression for exporter = ", r))
  assign(paste0('X_mat'), run_reg(exporters = r))
  X_mat <- as.data.frame(X_mat)
  writeData(wb, sheet = "Exporting region", X_mat,
            startRow = i, startCol = 2,
            colNames = FALSE, rowNames = FALSE)
  i <- i + 9
}

saveWorkbook(wb, file.path(output, 'distance_regression_filled.xlsx'), overwrite = T)
