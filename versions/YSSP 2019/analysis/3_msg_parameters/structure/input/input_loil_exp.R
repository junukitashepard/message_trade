# PARAMETER = "input"

parname <- 'input'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'node_origin', 'commodity', 'level', 'time', 'time_origin', 'value', 'unit')
# Technology in R script
value <- 1.1 # Change later
unit <- 'GWa'

# Node and year columns
year_act <- MESSAGE.years
year_vtg <- MESSAGE.years
node_loc <- node_origin <- paste0(region.number, '_', toupper(region.list))

# For value and unit
value_constant <- TRUE

# Auxiliary columns
mode <-'M1'
time <- 'year'
time_origin <- 'year'
commodity <- 'lightoil'
level <- 'secondary'  