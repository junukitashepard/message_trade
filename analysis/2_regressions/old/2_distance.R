############################## 
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
input <-    paste0(wd, "output/analysis/regress")
output <-   paste0(wd, "output/analysis/regress/distance_regress")
temp <-     paste0(wd, "temp/")

##############################
# Import file
trade <- readRDS(file.path(input, "regdf.rds"))

# Simple regression function
ols_regress <- function(variable, export.region = "all", import.region = "all", energy.type = "all",
                        full.summary = FALSE) {
  
  assign('regdf', trade)
  
  assign('form', paste0("var_cost ~ ", variable, " + factor(iso.i) + factor(iso.j) + factor(year)"))
  
  if (export.region != "all") {regdf <- subset(regdf, msg.region.i %in% export.region)}
  if (import.region != "all") {regdf <- subset(regdf, msg.region.j %in% import.region)}
  if (energy.type != "all") {regdf <- subset(regdf, energy %in% energy.type)}
  
  if (energy.type == "all") {
    form <- paste0(form, " + factor(energy)")
  }
  
  regdf <- subset(regdf, !is.na(var_cost) & !is.infinite(var_cost) & !is.nan(var_cost)) # Keep only if non-missing Y variable
  regdf <- subset(regdf, var_cost < 50)
  regdf$distance <- regdf$distance/(1000) # in thousand km
  
  assign('m', lm(as.formula(form), data = regdf))
  assign('coef', summary(m)$coefficients[variable,])
  
  # Add mean(Y)
  assign('meany', median(regdf$var_cost))
  assign('ar2', summary(m)$adj.r.squared)
  
  coef <- c(coef, meany, ar2)
  
  if (full.summary == TRUE) {
    return(summary(m))
  } else {
    return(coef)
  }
  
}

# Run function with different specifications and compile table
##############################################################
# Function: run for all regions and by region
run_reg <- function(variable = variable, exporters = "all", importers = "all") {
  
  assign('tab.names', c('base'))
  
  assign('model_base', ols_regress(variable = variable, export.region = exporters, import.region = importers))
  
  for (e in c('oil', 'coal', 'foil', 'LNG')) {
    assign('d.coef', ols_regress(variable = variable, export.region = exporters, import.region = importers, energy.type = e))
    model_base <- rbind(model_base, d.coef)
    tab.names <- c(tab.names, e)
  }
  rownames(model_base) <- tab.names
  return(model_base)
}

wb <- loadWorkbook(file.path(output, 'distance_regression.xlsx'))

# All regions
all_regions <- run_reg(variable = 'distance')
all_regions <- as.data.frame(all_regions)

writeData(wb, sheet = "All regions", all_regions, 
          startRow = 3, startCol = 2,
          colNames = FALSE, rowNames = FALSE)

# By importing region
region.list <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'SAS', 'WEU')

i <- 3
for (r in region.list) {
  print(paste0("Running regression for importer = ", r))
  assign(paste0('M_mat'), run_reg(variable = 'distance', importers = r))
  M_mat <- as.data.frame(M_mat)
  writeData(wb, sheet = "Importing region", M_mat,
            startRow = i, startCol = 2,
            colNames = FALSE, rowNames = FALSE)
  i <- i + 8
}

# By exporting region
i <- 3
for (r in region.list) {
  print(paste0("Running regression for exporter = ", r))
  assign(paste0('X_mat'), run_reg(variable = 'distance', exporters = r))
  X_mat <- as.data.frame(X_mat)
  writeData(wb, sheet = "Exporting region", X_mat,
            startRow = i, startCol = 2,
            colNames = FALSE, rowNames = FALSE)
  i <- i + 8
}

saveWorkbook(wb, file.path(output, 'distance_regression_filled.xlsx'), overwrite = T)
