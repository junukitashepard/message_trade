#######################################
# Build historical activity parameter #
#######################################
# Import UNCTAD data
region_seatrade <- read.csv(file.path(raw, "UNCTAD/us_seabornetrade_94081473295027.csv"), stringsAsFactors = F)
  names(region_seatrade)[1:2] <- c('economy', 'cargotype')
global_seatrade <- read.csv(file.path(raw, "UNCTAD/global_seaborne_trade_btonmiles.csv"), stringsAsFactors = F)
  names(global_seatrade)[1] <- 'cargotype'

# Reshape long
reshape_unctad <- function(year_list, infile, var_list) {
 
  df <- data.frame()
  assign('indf', infile)
  
  for (y in year_list) {
    
    assign('ydf', indf[c(var_list, paste0('X', y))])
    names(ydf) <- c(var_list, 'value')
    ydf$year <- y
    df <- rbind(df, ydf)
  }
  
  return(df)
}

global_seatrade <- reshape_unctad(infile = global_seatrade, year_list = 2000:2018, var_list = 'cargotype')
region_seatrade <- reshape_unctad(infile = region_seatrade, year_list = 1970:2017, var_list = c('economy', 'cargotype'))

# Get regional shares going back to 2000 (Only >2006 are filled for all regions)
region_seatrade <- subset(region_seatrade, year > 1999) 
region_seatrade <- subset(region_seatrade, economy %in% economy_list)

suppressWarnings(region_seatrade$value <- as.numeric(region_seatrade$value)) # missing will be NA

# Combine by solid vs. liquids
region_seatrade$type[region_seatrade$cargotype %in% c('Crude oil loaded')] <- 'liquid_shipping'
region_seatrade$type[region_seatrade$cargotype %in% c('Dry cargo loaded')] <- 'solid_shipping'
region_seatrade$type[region_seatrade$cargotype %in% c('Petroleum product and gas loaded')] <- 'LNG_shipping'

region_seatrade <- group_by(region_seatrade, economy, type, year) %>%
                   summarize(value = sum(value, na.rm = T))

global_seatrade$type[global_seatrade$cargotype %in% c('Oil', 'Chemicals')] <- 'liquid_shipping'
global_seatrade$type[global_seatrade$cargotype %in% c('Main bulks')] <- 'solid_shipping'
global_seatrade$type[global_seatrade$cargotype %in% c('Gas')] <- 'LNG_shipping'

global_seatrade <- group_by(global_seatrade, type, year) %>%
                   summarize(value = sum(value, na.rm = T))

global_seatrade$value <- global_seatrade$value / 0.621371 # billion ton-miles to ton-km

# Interpolate 2000-2006 based on linear interpolation
region_seatrade$value[region_seatrade$year < 2006] <- NA

value_approx <- data.frame()
for (r in unique(region_seatrade$economy)) {
  
  for (s in unique(region_seatrade$type)) {
  
    assign('df', subset(region_seatrade, economy == r & type == s))
 
    df <- arrange(df, desc(year))
    df$value_approx <- na.approx(df$value, rule = 2)
    
    value_approx <- rbind(as.data.frame(value_approx), as.data.frame(df))
  }
}

region_seatrade <- left_join(region_seatrade, value_approx, by = c('economy', 'type', 'year', 'value'))
region_seatrade$value <- region_seatrade$value_approx
region_seatrade$value_approx <- NULL

# Calculate share
region_seatrade <- group_by(region_seatrade, year, type) %>% mutate(value_tot = sum(value))
region_seatrade$share <- region_seatrade$value/region_seatrade$value_tot

# Plot
liquid_plot <- 
  ggplot(aes(x = year, y = share, fill =  economy), 
         data = subset(region_seatrade, year > 2005 & type == 'liquid_shipping')) +
    geom_bar(stat = 'identity') + 
    labs(x = "Year", y = "Share of seaborne trade, billion ton-miles", fill = 'Economy', 
         title = "Liquid shipping", subtitle = "Includes: Crude oil loaded") + 
    theme(text = element_text(size = 15), legend.position = 'bottom')

solid_plot <- 
  ggplot(aes(x = year, y = share, fill =  economy), 
         data = subset(region_seatrade, year > 2005 & type == 'solid_shipping')) +
  geom_bar(stat = 'identity') + 
  labs(x = "Year", y = "Share of seaborne trade, billion ton-miles", fill = 'Economy', 
       title = "Solid shipping", subtitle = "Includes: Dry cargo loaded") +
  theme(text = element_text(size = 15), legend.position = 'bottom')

LNG_plot <- 
  ggplot(aes(x = year, y = share, fill =  economy), 
         data = subset(region_seatrade, year > 2005 & type == 'LNG_shipping')) +
  geom_bar(stat = 'identity') + 
  labs(x = "Year", y = "Share of seaborne trade, billion ton-miles", fill = 'Economy', 
       title = "LNG shipping", subtitle = "Includes: Petroleum and gas product loaded") +
  theme(text = element_text(size = 15), legend.position = 'bottom')

# Combine files
region_seatrade <- region_seatrade[c('economy', 'type', 'year', 'share')]
isid('region_seatrade', c('economy', 'type', 'year'))

seatrade <- inner_join(region_seatrade, global_seatrade, by = c('type', 'year'))
seatrade$value <- seatrade$value * seatrade$share
seatrade$share <- NULL

# Assign MESSAGE regions by disaggregating based on trade data
trade.df <- read.csv(file.path(input, 'trade/regional_trade.csv'), stringsAsFactors = F)
trade.df$type[trade.df$energy %in% c('foil', 'oil')] <- 'liquid_shipping'
trade.df$type[trade.df$energy %in% c('coal')] <- 'solid_shipping'
trade.df$type[trade.df$energy %in% c('LNG')] <- 'LNG_shipping'

trade.df <- group_by(trade.df, year, msg_region1, type) %>% summarize(region_exports = sum(region_trade, na.rm = T))
trade.df <- group_by(trade.df, year, type) %>% mutate(tot_exports = sum(region_exports, na.rm = T))
trade.df$share_exports <- trade.df$region_exports/trade.df$tot_exports
trade.df$region_exports <- trade.df$tot_exports <- NULL

trade.df <- left_join(trade.df, unctad_region, by = c('msg_region1' = 'msgregion'))

seatrade <- inner_join(trade.df, seatrade, by = c('year', 'unctadregion' = 'economy', 'type'))

seatrade$value <- seatrade$value * seatrade$share_exports
seatrade$share_exports <- NULL

# Plot historical activity
seatrade$type_lab <- stringr::str_replace(seatrade$type, "_", " ")
seatrade$type_lab <- paste0(toupper(substr(seatrade$type_lab, 1, 1)), substr(seatrade$type_lab, 2, nchar(seatrade$type_lab)))

ggplot(aes(x = year, y = value, colour = msg_region1), data = seatrade) +
  geom_line(size = 1) +
  facet_grid(~type_lab) +
  labs(x = 'Year', y = 'bton-km (loaded)', colour = 'MESSAGE region') +
  theme(text = element_text(size = 15), legend.position = 'bottom') 
  
seatrade$type <- paste0(seatrade$type, "_diesel")

# Reformat to MESSAGE-compatible parameter format
reformat_activity <- function(shipping) {
  
  assign('df', subset(seatrade, type == shipping))
  
  df <- df[c('msg_region1', 'year', 'value')]
  df$technology <- shipping
  df$mode <- "M1"
  df$time <- "year"
  df$unit <-  'bton-km-y'
  
  df <- df[c('msg_region1', 'technology', 'year', 'mode', 'time', 'value', 'unit')]
  names(df) <- c('node_loc', 'technology', 'year_act', 'mode', 'time', 'value', 'unit')
  
  df$node_loc <- paste0('R14_', df$node_loc)
  
  df <- subset(df, year_act %in% msg_years)
  write.csv(df, file.path(output, paste0('historical_activity/', shipping, '.csv')))
  
}

