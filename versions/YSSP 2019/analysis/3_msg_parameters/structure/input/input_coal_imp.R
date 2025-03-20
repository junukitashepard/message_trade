# PARAMETER = "input"

parname <- 'input'
varlist <- c('node_loc', 'technology', 'year_act', 'mode', 'node_origin', 'commodity', 'level', 'time', 'time_origin', 'value', 'unit')
# Technology in R script
value <- 1.0 # Change later
unit <- 'GWa'

# Node and year columns
year_act <- MESSAGE.years
node_loc <- paste0(region.number, '_', toupper(region.list)) 
node_origin <- paste0(region.number, '_GLB')

# For value and unit
value_constant <- TRUE

# Auxiliary columns
mode <-'M1'
time <- 'year'
time_origin <- 'year'
commodity <- 'coal'
level <- 'shipped-coal'  