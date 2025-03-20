# PARAMETER = "output"

parname <- 'output'
varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'node_dest', 'commodity', 'level', 'time', 'time_dest', 'value', 'unit')
# Technology in R script
value <- 1 # Change later
unit <- 'GWa'

# Node and year columns
year_act <- MESSAGE.years
year_vtg <- MESSAGE.years
node_loc <- paste0(region.number, '_', toupper(region.list))
node_dest <- paste0(region.number, '_GLB')

# For value and unit
value_constant <- TRUE

# Auxiliary columns
mode <-'M1'
time <- 'year'
time_dest <- 'year'
commodity <- 'lh2'
level <- 'shipped-lh2'  