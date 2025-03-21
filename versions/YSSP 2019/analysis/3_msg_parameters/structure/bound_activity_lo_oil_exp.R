# PARAMETER = "bound_activity_lo"

parname <- 'bound_activity_lo'
varlist <- c('node_loc', 'technology', 'year_act', 'mode', 'time', 'value', 'unit')

# Technology in R script
value <- 1 # Change later
unit <- 'GWa'

# Node and year columns
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
node_loc <- c('R14_AFR', 'R14_CAS', 'R14_CPA', 'R14_EEU', 'R14_LAM', 'R14_MEA', 'R14_PAO', 'R14_PAS', 'R14_RUS', 'R14_SAS', 'R14_SCS', 'R14_UBM', 'R14_WEU')

# For value and unit
value_constant <- FALSE
value_unit.df <- read.csv()
  
# Auxiliary columns
mode <-'M1'
time <- 'year'
