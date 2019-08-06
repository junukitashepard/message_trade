# PARAMETER = "output"

parname <- 'output'
varlist <- c('node_loc', 'technology', 'year_act', 'mode', 'node_dest', 'commodity', 'level', 'time', 'time_dest', 'value', 'unit')
# Technology in R script
value <- 1 # Change later
unit <- 'GWa'

# Node and year columns
year_act <- c(seq(1990, 2055, by = 5), seq(2060, 2110, by = 10))
# year_vtg info in R script
# node info in R script

# For value and unit
value_constant <- TRUE

# Auxiliary columns
mode <-'M1'
time <- 'year'
time_dest <- 'year'
commodity <- 'coal'
level <- 'primary'  