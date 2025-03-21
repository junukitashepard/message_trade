#######################################################
# Subset trade data to energy commodities in TJ units #
#######################################################
# Import trade and port file
trade <- readRDS(file.path(input, 'derived/trade/trade.rds'))
ijports <- readRDS(file.path(input, 'derived/nodes/ij_ports.rds'))
path <- readRDS(file.path(input, 'derived/nodes/shortest_paths.rds'))

# Import country definition
web.countries <- read.csv(file.path(raw, 'ConversionTables/web_countries.csv'), stringsAsFactors = F)

# Link country names to ports
#############################
# Function fix_names: fix country names in ijports
fix_names <- function(in.name, out.name) {
  assign('df', ijports)
  df$port1.country[df$port1.country == in.name] <- out.name
  df$port2.country[df$port2.country == in.name] <- out.name
  assign('ijports', df, envir = parent.frame())
}

ijports$port1.country <- trimws(ijports$port1.country)
ijports$port2.country <- trimws(ijports$port2.country)

# Import CSV of name corrections
country.correction <- read.csv(file.path(raw, "UserInputs/country_correction_ports.csv"), stringsAsFactors = F)
  names(country.correction) <- c('wrong', 'right')
  
for (i in 1:nrow(country.correction)) {
  fix_names(country.correction$wrong[i], country.correction$right[i])
}

ijports <- left_join(ijports, web.countries[c('web.country', 'iso.country')], 
                     by = c('port1.country' = 'web.country'))
ijports <- left_join(ijports, web.countries[c('web.country', 'iso.country')], 
                     by = c('port2.country' = 'web.country'))
names(ijports)[11:12] <- c('port1.iso', 'port2.iso')

ijports <- subset(ijports, !is.na(port1.iso) & !is.na(port2.iso))

saveRDS(ijports, file.path(output, 'derived/nodes/ij_ports_fixednames.rds'))

# Link region names to countries 
#################################
regional.spec <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
  names(regional.spec) <- c('iso', 'msg.region')

ijports <- left_join(ijports, regional.spec, by = c('port1.iso' = 'iso'))
ijports <- left_join(ijports, regional.spec, by = c('port2.iso' = 'iso'))

names(ijports)[13:14] <- c('msg_region1', 'msg_region2')

# Subset to only include obs with regions
ijports <- subset(ijports, !is.na(msg_region1) & !is.na(msg_region2) &
                           msg_region1 != "" & msg_region2 != "" &
                           msg_region1 != msg_region2)

# Link to energy and collapse by region
########################################
# Function, energy2ports: link trade flow to port
energy2ports <- function(energy.type, allports = F) {
  
  environment(assert) <- environment(isid) <- environment()
  
  print("######################")
  print(paste0('Running: ', energy.type))
  print("######################")
  
  assign('t.df', subset(trade, energy == energy.type))
    t.df <- t.df[c('i','j', 'iso.i', 'iso.j', 'year', 'q_e')]
    isid('t.df', c('iso.i', 'iso.j', 'year'))
    
  assign('p.df', left_join(ijports, t.df, 
                           by = c('port1.iso'='iso.i', 'port2.iso'='iso.j')))

  # Total imports/exports to/from each port
  assign('p.df.M', dplyr::group_by(p.df, year, port2, port2.country, port2.iso, port2.long, port2.lat,
                                   msg_region1, msg_region2) %>%
                   dplyr::summarize(port_imports = sum(q_e, na.rm = T)))
  assign('p.df.X', dplyr::group_by(p.df, year, port1, port1.country, port1.iso, port1.long, port1.lat,
                                   msg_region1, msg_region2) %>%
                   dplyr::summarize(port_exports = sum(q_e, na.rm = T)))

  # Total region-level trade flows
  assign('p.df.r', dplyr::group_by(p.df, year, msg_region1, msg_region2) %>%
                   dplyr::summarize(region_trade = sum(q_e, na.rm = T)))

  # Link total trade flows to port-level flows
  p.df.M <- left_join(p.df.M, p.df.r, by = c('year', 'msg_region1', 'msg_region2'))
  p.df.X <- left_join(p.df.X, p.df.r, by = c('year', 'msg_region1', 'msg_region2'))
  
  # Share of port-level flow over overall flow
  p.df.M$share <- p.df.X$share <- 0
  p.df.M$share[p.df.M$region_trade > 0] <- p.df.M$port_imports[p.df.M$region_trade > 0]/p.df.M$region_trade[p.df.M$region_trade > 0]
  p.df.X$share[p.df.X$region_trade > 0] <- p.df.X$port_exports[p.df.X$region_trade > 0]/p.df.X$region_trade[p.df.X$region_trade > 0]
  
  assert('max(p.df.M$share) <= 1')
  assert('max(p.df.X$share) <= 1')

  # By year-region-region: select ports
  p.df.M <- dplyr::group_by(p.df.M, year, msg_region1, msg_region2) %>%
            dplyr::mutate(max.share = max(share, na.rm = T), rown = row_number())
  p.df.X <- dplyr::group_by(p.df.X, year, msg_region1, msg_region2) %>%
            dplyr::mutate(max.share = max(share, na.rm = T), rown = row_number())
  
  if (allports == FALSE) {
    p.df.M <- unique(subset(p.df.M, max.share == share & max.share != 0))
    p.df.X <- unique(subset(p.df.X, max.share == share & max.share != 0))
    
    if (energy.type == 'BIO') {
      p.df.M <- group_by(p.df.M, msg_region1, msg_region2, year) %>% mutate(count = row_number())
      p.df.M <- subset(p.df.M, count == 1) # should drop 1 obs
      p.df.M$count <- NULL
    }
    
    # isid('p.df.M', c('msg_region1', 'msg_region2', 'year'))
    # isid('p.df.X', c('msg_region1', 'msg_region2', 'year'))
  
    p.df.M$max.share <- p.df.X$max.share <- NULL
  }
  p.df.M$energy <- energy.type
  p.df.X$energy <- energy.type
  
  assign(paste0(energy.type, '.imports'), p.df.M, envir = parent.frame())
  assign(paste0(energy.type, '.exports'), p.df.X, envir = parent.frame())
}

# Run programs
x.df <- data.frame(year = numeric(0), 
                   port1 = numeric(0), port1.country = character(0), port1.iso = character(0), 
                   port1.long = numeric(0), port1.lat = numeric(0),
                   msg_region1 = character(0), msg_region2 = character(0),
                   port_exports = numeric(0), region_trade = numeric(0), share = numeric(0), energy = character(0))
m.df <- data.frame(year = numeric(0), 
                   port2 = numeric(0), port2.country = character(0), port2.iso = character(0),
                   port2.long = numeric(0), port2.lat = numeric(0),
                   msg_region1 = character(0), msg_region2 = character(0),
                   port_imports = numeric(0), region_trade = numeric(0), share = numeric(0), energy = character(0))

# Run 1: Include all paths (even longer ones) #
###############################################
for (e in energy.types.BACI) {
  energy2ports(e, allports = T)
  
  assign('x.in', get(paste0(e, '.exports')))
  assign('m.in', get(paste0(e, '.imports')))

  x.df <- rbind(as.data.frame(x.df), as.data.frame(x.in))
  m.df <- rbind(as.data.frame(m.df), as.data.frame(m.in))
}

# Plot weighted distance by country and region
x.df.co <- x.df[c('year', 'energy', 'msg_region1', 'msg_region2', 
                  'port1', 'port1.country', 'port1.iso', 'port1.long', 'port1.lat', 'region_trade', 
                  'share', 'port_exports')]

m.df.co <- m.df[c('year', 'energy', 'msg_region1', 'msg_region2', 
                  'port2', 'port2.country', 'port2.iso', 'port2.long', 'port2.lat')]

co <- inner_join(x.df.co, m.df.co, by = c('year', 'energy', 'msg_region1', 'msg_region2'))
co <- inner_join(co, unique(ijports[c('port1', 'port2', 'distance')]), by = c('port1', 'port2'))

co_byregion <- dplyr::group_by(co, msg_region1, msg_region2, year, energy) %>% 
               dplyr::summarise(wtd_distance = weighted.mean(distance, port_exports, na.rm = T))
co_byregion <- dplyr::group_by(co_byregion, msg_region1, msg_region2, energy) %>%
               dplyr::mutate(constant_mean = mean(wtd_distance, na.rm = T))
co_byregion$diff_from_mean <- (co_byregion$wtd_distance-co_byregion$constant_mean)/co_byregion$constant_mean

# Plot example of MEA to NAM
co_byregion <- subset(co_byregion, !is.na(year) & 
                        msg_region1 == 'MEA' & msg_region2 == 'NAM')
co_byregion$energy <- toupper(co_byregion$energy)

plot <- ggplot(aes(x = year, y = diff_from_mean, colour = energy), data = co_byregion) + 
        geom_line(size = 1.5) + 
        labs(x = "Year", y = "% deviation from time-constant mean", 
             title = 'Difference between time-variant distance and mean over time', subtitle = 'MEA to NAM', colour = 'Energy') + 
        theme(legend.position = 'bottom', text = element_text(size = 20)) +
        scale_color_brewer(palette = 'Paired')

# Aggregate to regional level
all.trade <- unique(x.df[c('year', 'msg_region1', 'msg_region2', 'energy', 'region_trade')])
assert("mean(all.trade$region_trade) == mean(unique(m.df[c('year', 'msg_region1', 'msg_region2', 'energy', 'region_trade')])$region_trade)")

write.csv(all.trade, file.path(output, "derived/trade/regional_trade.csv"))

# Write files
write.csv(x.df, file.path(output, "derived/nodes/export_paths_all.csv"))
write.csv(m.df, file.path(output, "derived/nodes/import_paths_all.csv"))

# Run 2: Only include shortest path (even longer ones) #
########################################################
x.df <- data.frame(year = numeric(0), 
                   port1 = numeric(0), port1.country = character(0), port1.iso = character(0), 
                   port1.long = numeric(0), port1.lat = numeric(0),
                   msg_region1 = character(0), msg_region2 = character(0),
                   port_exports = numeric(0), region_trade = numeric(0), share = numeric(0), energy = character(0))
m.df <- data.frame(year = numeric(0), 
                   port2 = numeric(0), port2.country = character(0), port2.iso = character(0),
                   port2.long = numeric(0), port2.lat = numeric(0),
                   msg_region1 = character(0), msg_region2 = character(0),
                   port_imports = numeric(0), region_trade = numeric(0), share = numeric(0), energy = character(0))

for (e in energy.types.BACI) {
  energy2ports(e, allports = FALSE)
  
  assign('x.in', get(paste0(e, '.exports')))
  assign('m.in', get(paste0(e, '.imports')))
  
  x.df <- rbind(as.data.frame(x.df), as.data.frame(x.in))
  m.df <- rbind(as.data.frame(m.df), as.data.frame(m.in))
}

# Write files
write.csv(x.df, file.path(output, "derived/nodes/export_paths_sp.csv"))
write.csv(m.df, file.path(output, "derived/nodes/import_paths_sp.csv"))

# Add coordinates and intermediate nodes
x.df.co <- x.df[c('year', 'energy', 'msg_region1', 'msg_region2', 
                  'port1', 'port1.country', 'port1.iso', 'port1.long', 'port1.lat', 'region_trade', 'share')]

m.df.co <- m.df[c('year', 'energy', 'msg_region1', 'msg_region2', 
                  'port2', 'port2.country', 'port2.iso', 'port2.long', 'port2.lat')]

co.out <- inner_join(x.df.co, m.df.co, by = c('year', 'energy', 'msg_region1', 'msg_region2'))

co.out <- inner_join(co.out, path[c('port1', 'port2', 'node', 'node.long', 'node.lat', 'route_id', 'step_id')],
                     by = c('port1', 'port2'))
                     
write.csv(co.out, file.path(output, 'derived/nodes/regional_paths.csv'))