# PARAMETER = "output"

parname <- 'output'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'node_dest', 'commodity', 'level', 'time', 'time_dest', 'value', 'unit')
# Technology in R script
value <- 1 # Change later
unit <- 'GWa'

# Node and year columns
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
year_vtg <- c(seq(1990, 2060, by = 5), seq(2070, 2110, by = 10))
node_loc <- c('R14_AFR', 'R14_CAS', 'R14_CPA', 'R14_EEU', 'R14_LAM', 'R14_MEA', 'R14_PAO', 'R14_PAS', 'R14_RUS', 'R14_SAS', 'R14_SCS', 'R14_UBM', 'R14_WEU')
node_dest <- 'R14_GLB'

# For value and unit
value_constant <- TRUE

# Auxiliary columns
mode <-'M1'
time <- 'year'
time_dest <- 'year'
commodity <- 'crudeoil'
level <- 'shipped-oil'  