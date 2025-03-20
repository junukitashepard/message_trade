############################################
# Build parameters for shipping technology #
############################################
# Import functions 
###################
source(paste0(repo, 'analysis/3_msg_parameters/scale_msg_parameter/functions.R'))
source(paste0(repo, 'analysis/3_msg_parameters/build_parameters.R'))
source(paste0(repo, 'analysis/3_msg_parameters/activity_parameters/build_activity.R'))

# Import data files 
####################
# Import regionally aggregated trade data
trade.df <- read.csv(file.path(input, 'derived/trade/regional_trade.csv'), stringsAsFactors = F)

# MESSAGE regions
web_countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F)

msg_region <- read.csv(file.path(raw, "UserInputs/regional_specification.csv"), stringsAsFactors = F)
names(msg_region) <- c('iso', 'msgregion')

unctad_region <- read.csv(file.path(raw, "UserInputs/message_unctad_regional_specification.csv"), stringsAsFactors = F)
names(unctad_region) <- c('msgregion', 'unctadregion')

iea_region <- read.csv(file.path(raw, 'UserInputs/message_iea_regional_specification.csv'), stringsAsFactors = F)
names(iea_region) <- c('msgregion', 'iearegion')

# Import IEA heating values 
ncv <- readRDS(file.path(temp, 'ncv_convert.rds'))

ncv <- left_join(ncv, iea_region, by = c('region' = 'iearegion'))

ncv$energy[ncv$energy == 'COAL'] <- 'coal'
ncv$energy[ncv$energy == 'PET'] <- 'loil.foil'
ncv$energy[ncv$energy == 'CRU'] <- 'oil'
ncv$energy[ncv$energy == 'NG'] <- 'LNG'
ncv$energy[ncv$energy == 'METH'] <- 'meth'
ncv$energy[ncv$energy == 'ETH'] <- 'eth'

ncv <- subset(ncv, energy %in% c('coal', 'loil.foil', 'oil', 'LNG', 'meth', 'eth') & !is.na(msgregion))
ncv <- group_by(ncv, msgregion, energy) %>% summarise(ncv = mean(ncv, na.rm = T))

ncv$value <- (ncv$ncv * 0.2778) # from TJ/ton to GWh/ton
ncv$value <- ncv$value/8760 # from GWh/ton to GWa/ton
ncv$value <- ncv$value*10^9 # from GWa/ton to GWa/bton
ncv$value <- 1/ncv$value # to ton/GWa

# Import distances
distances <- readRDS(file.path(input, 'derived/nodes/ij_ports.rds'))
distances <- unique(distances[c('port1.country', 'port2.country', 'distance')])
isid('distances', c('port1.country', 'port2.country'))

distances <- left_join(distances, web_countries[c('iso.country', 'web.country')], by = c('port1.country' = 'web.country'))
distances <- left_join(distances, web_countries[c('iso.country', 'web.country')], by = c('port2.country' = 'web.country'))
names(distances) <- c('country1', 'country2', 'distance', 'iso1', 'iso2')

distances <- left_join(distances, msg_region, by = c('iso1' = 'iso'))
distances <- left_join(distances, msg_region, by = c('iso2' = 'iso'))
names(distances) <- c('country1', 'country2', 'distance', 'iso1', 'iso2', 'msgregion1', 'msgregion2')

distances <- group_by(distances, msgregion1, msgregion2) %>% summarise(mean_distance = mean(distance, na.rm = T))
distances <- subset(distances, !is.na(msgregion1) & !is.na(msgregion2) & 
                      msgregion1 != "" & msgregion2 != "" &
                      msgregion1 != msgregion2)
distances$msgregion1 <- paste0(region.number, '_', distances$msgregion1)
distances$msgregion2 <- paste0(region.number, '_', distances$msgregion2)

# Function to fix vintage years
fix_vintage <- function(param, no.vintage = FALSE) {
  
  assign('df', param)
  
  if (no.vintage == FALSE) {
    df$diff <- df$year_act - df$year_vtg
    df <- subset(df, diff > 0 & diff < shipping_technical_lifetime)
    df$diff <- NULL
  } else {
    df$year_vtg <- df$year_act
    df <- unique(df)
  }
  return(df)
}

# Build parameters!
####################
# capacity_factor
build_capacity_factor <- function(technology.in, no.vintage = FALSE) {
  
  parname <- 'capacity_factor'
  varlist <-  c('node_loc', 'technology', 'year_vtg', 'year_act', 'time', 'value', 'unit')
  year_act <- MESSAGE.years
  year_vtg <- MESSAGE.years
  value <- 1
  unit <- '%'
  time <- 'year'
  
  par.capacity_factor <-
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_act = year_act, year_vtg = year_vtg,
                    time = time, 
                    value = value, unit = unit) %>%
    fix_vintage(no.vintage = no.vintage)
  
  return(par.capacity_factor)
}

# fix_cost
build_fix_cost <- function(technology.in, value.in, no.vintage = FALSE) {
  
  parname <- 'fix_cost'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'value', 'unit')
  unit <- 'USD/bton-km-y'
  year_act <- MESSAGE.years
  year_vtg <- year_act
  
  par.fix_cost <- 
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_loc = paste0(region.number,'_', toupper(region.list)),
                    year_act = year_act, year_vtg = year_vtg,
                    value = value.in, unit = unit) %>%
    fix_vintage(no.vintage = no.vintage)
  
  return(par.fix_cost)
}

# inv_cost
build_inv_cost <- function(technology.in, value.in, cost_scenario = 'BAU', 
                           diesel_liquid_ic = 5.86e-8, diesel_solid_ic = 7.95e-8, diesel_LNG_ic = 1.3e-7) {
  
  parname <- 'inv_cost'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
  unit <- 'USD/GWa'
  year_vtg <- MESSAGE.years
  
  par.inv_cost <- 
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_vtg = year_vtg,
                    value = value.in, unit = unit)
  
  if (grepl('shipping_elec', technology.in) | grepl('shipping_LNG', technology.in)) {
    
    if (grepl('liquid_shipping', technology.in)) {dc = diesel_liquid_ic}
    if (grepl('solid_shipping', technology.in)) {dc = diesel_solid_ic}
    if (grepl('LNG_shipping', technology.in)) {dc = diesel_LNG_ic}
    
    # Business as usual (BAU): investment costs constant over time
    if (cost_scenario == 'BAU') {}
    
    # Competitive long-term (CLT): costs start very high but reach diesel costs by 2110 linearly
    if (cost_scenario == 'CLT') {
      
      ic <- as.data.frame(min(year_vtg):max(year_vtg))
      names(ic) <- 'year_vtg'
      ic$inv_cost[ic$year_vtg < 2020] <- value.in
      ic$inv_cost[ic$year_vtg == 2110] <- dc
      ic$inv_cost <- zoo::na.approx(ic$inv_cost)
      
      par.inv_cost <- left_join(par.inv_cost, ic, by = c('year_vtg'))
      par.inv_cost$value <- par.inv_cost$inv_cost
      par.inv_cost$inv_cost <- NULL
      
    }
  }
  return(par.inv_cost)
}

# input
build_input <- function(technology.in, fuel_list.in, value.in, no.vintage = FALSE) {
  
  parname <- 'input'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'node_origin', 'commodity', 'level', 'time', 'time_origin', 'value', 'unit')
  unit <- 'GWa'
  year_act <- MESSAGE.years
  year_vtg <- year_act
  mode <- 'M1'
  time <- 'year'
  time_origin <- 'year'
  level <- 'secondary'
  
  par.input <- data.frame()
  
  for (fuel in fuel_list.in) {
    assign('df',
           build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                           node_loc = 'dummy', node_origin = paste0(region.number, '_', toupper(region.list)),
                           year_act = year_act, year_vtg = year_vtg,
                           mode = mode, time = time, time_origin = time_origin,
                           commodity = fuel, level = level,
                           value = value.in, unit = unit) %>%
             fix_vintage(no.vintage = no.vintage))
    
    df$node_loc <- df$node_origin
    df <- unique(df)
    
    par.input <- rbind(as.data.frame(par.input), as.data.frame(df))
  }
  
  return(par.input)
}

# output
build_output <- function(technology.in, no.vintage = FALSE) {
  
  parname <- 'output'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'node_dest', 'commodity', 'level', 'time', 'time_dest', 'value', 'unit')
  value <- 1 # t-km-y
  unit <- 'bton-km-y'
  year_act <- MESSAGE.years
  year_vtg <- year_act
  mode <- 'M1'
  time <- 'year'
  time_dest <- 'year'
  level <- 'export'
  
  par.output <- 
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_dest = paste0(region.number, '_GLB'), node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_act = year_act, year_vtg = year_vtg,
                    mode = mode, time = time, time_dest = time_dest,
                    commodity = 'shipping_capacity', level = level,
                    value = value, unit = unit) %>%
    fix_vintage(no.vintage = no.vintage)
  return(par.output)
}

# technical_lifetime
build_technical_lifetime <- function(technology.in, value.in) {
  
  parname <- 'technical_lifetime'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
  unit <- 'y'
  year_vtg <- MESSAGE.years
  
  par.technical_lifetime <- 
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_vtg = year_vtg,
                    value = value.in, unit = unit)
  return(par.technical_lifetime)
}

# emission_factor
build_emission_factor <- function(technology.in, value.in, no.vintage = FALSE) {
  parname <- 'emission_factor'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'emission', 'value', 'unit')
  mode <- 'M1'
  unit <- 'kg/bton-km-y'
  year_act <- MESSAGE.years
  year_vtg <- year_act
  time <- 'year'
  emission <- 'CO2'
  
  par.emission_factor <-
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_vtg = year_vtg, year_act = year_act,
                    mode = mode, emission = emission,
                    value = value.in, unit = unit) %>%
    fix_vintage(no.vintage = no.vintage)
  
  return(par.emission_factor)
}

# var_cost (exports)
build_var_cost <- function(technology.in, value.in, no.vintage = FALSE) {
  
  parname <- 'var_cost'
  varlist <- c('node_loc', 'technology', 'year_vtg', 'year_act', 'mode', 'time', 'value', 'unit')
  mode <- 'M1'
  unit <- 'USD/bton-km-y'
  year_act <- MESSAGE.years
  year_vtg <- year_act
  time <- 'year'
  mode <- 'M1'
  
  par.var_cost <- 
    build_parameter(parname = parname, varlist = varlist, technology = technology.in,
                    node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_vtg = year_vtg, year_act = year_act, 
                    mode = mode, time = time,
                    value = value.in, unit = unit) %>%
    fix_vintage(no.vintage = no.vintage)
  
  return(par.var_cost)
}
