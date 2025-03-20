#######################################
# Build historical capacity parameter #
#######################################
# Import UNCTAD data
country_liquid <- read.csv(file.path(raw, "UNCTAD/us_tankerfleet_97380838377977.csv"), stringsAsFactors = F)
country_solid <- read.csv(file.path(raw, "UNCTAD/us_solidfleet_97382058110137.csv"), stringsAsFactors = F)
country_LNG <- read.csv(file.path(raw, "UNCTAD/us_otherfleet_97384610144034.csv"), stringsAsFactors = F)

# Reshape long
reshape_unctad <- function(year_list, infile) {
  
  df <- data.frame()
  assign('indf', infile)
  names(indf)[1] <- 'economy'
  
  indf[indf == ".."] <- NA
  
  for (y in year_list) {
    
    assign('ydf', indf[c('economy', paste0('X', y))])
    names(ydf) <- c('economy', 'value')
    ydf$year <- y
    df <- rbind(df, ydf)
  }
  
  suppressWarnings(df$value <- as.numeric(df$value))
  df <- subset(df, !is.na(value))
  return(df)
}

# Combine files
sc <- data.frame()

for (type in c('liquid', 'solid', 'LNG')) {
  
  longdf <- reshape_unctad(year_list = 1980:2018, infile = get(paste0('country_', type)))
  longdf$type <- type
  sc <- rbind(as.data.frame(sc), as.data.frame(longdf))
}

# Fix country names
sc$economy <- trimws(sc$economy)

  sc$economy[sc$economy == 'Belgium'] <- 'Belgium-Luxembourg'
  sc$economy[sc$economy == 'Dominican Republic'] <- 'Dominican Rep.'
  sc$economy[sc$economy == 'Ethiopia (...1991)'] <- 'Ethiopia'
  sc$economy[sc$economy == 'Germany, Democratic Republic of'] <- 'Germany'
  sc$economy[sc$economy == 'Germany, Federal Republic of'] <- 'Germany'
  sc$economy[sc$economy == 'Indonesia (...2002)'] <- 'Indonesia'
  sc$economy[sc$economy == 'Iran (Islamic Republic of)'] <- 'Iran'
  sc$economy[sc$economy == "Korea, Dem. People's Rep. of"] <- "Dem. People's Rep. of Korea"
  sc$economy[sc$economy == 'Korea, Republic of'] <- 'Rep. of Korea'
  sc$economy[sc$economy == 'Luxembourg'] <- 'Belgium-Luxembourg'
  sc$economy[sc$economy == 'Panama, excluding Canal Zone'] <- 'Panama'
  sc$economy[sc$economy == 'South Africa'] <- 'So. African Customs Union'
  sc$economy[sc$economy == 'Switzerland, Liechtenstein'] <- 'Switzerland'
  sc$economy[sc$economy == 'United Republic of Tanzania'] <- 'United Rep. of Tanzania'
  sc$economy[sc$economy == 'United States of America'] <- 'USA'
  sc$economy[sc$economy == 'Venezuela (Bolivarian Rep. of)'] <- 'Venezuela'
  sc$economy[sc$economy == 'Yemen, Arab Republic'] <- 'Yemen'

  
# Link to MESSAGE regions
sc <- left_join(sc, web_countries[c('web.country', 'iso.country')], by = c('economy' = 'web.country'))
sc <- inner_join(sc, msg_region, by = c('iso.country' = 'iso'))
  
sc <- group_by(sc, msgregion, type, year) %>% summarise(value = sum(value, na.rm = T))
  
sc <- subset(sc, msgregion != '')

# Convert thousand DWT to bton-km-y
sc$value <- sc$value/10^6 #thousand ton to billion ton
sc$value <- sc$value * 13423 # average distance traveled (km) per year

# Get difference by year (capacity additions)
sc <- arrange(sc, type, msgregion, year)
sc <- group_by(sc, type, msgregion) %>% mutate(lag.value = lag(value))

sc$new_capacity <- sc$value - sc$lag.value
sc$new_capacity[sc$new_capacity < 0] <- 0

sc <- subset(sc, year %in% c(seq(1970, 1995, by = 5), MESSAGE.years))
sc <- subset(sc, !is.na(new_capacity))

# Plot historical new capacity
sc$type_lab <- paste0(toupper(substr(sc$type, 1, 1)), substr(sc$type, 2, nchar(sc$type)))

ggplot(aes(x = year, y = new_capacity, fill = msgregion), data = sc) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~type_lab) +
  labs(x = 'Year', y = 'bton-km-y', colour = 'MESSAGE region', title = 'New shipping capacity') +
  theme(text = element_text(size = 15), legend.position = 'bottom') 

sc$type <-paste0(sc$type, '_shipping_diesel')

# Reformat to MESSAGE-compatible parameter format
reformat_capacity <- function(shipping) {
  
  assign('df', subset(sc, type == shipping))
  
  df <- df[c('msgregion', 'year', 'value')]
  df$technology <- shipping
  df$unit <- 'bton-km'
  
  df <- df[c('msgregion', 'technology', 'year', 'value', 'unit')]
  names(df) <- c('node_loc', 'technology', 'year_vtg', 'value', 'unit')
  
  df$node_loc <- paste0(region.number, '_', df$node_loc)
  
  df <- unique(df)
  
  write.csv(df, file.path(output, paste0('analysis/msg_parameters/historical_new_capacity/', shipping, '.csv')))
}

