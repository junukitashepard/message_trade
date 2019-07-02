###################################################
# Scale parameters based on trade volume (in GWa) #
# Functions #
###################################################
# Function: convert TJ to GWa
convert_tj_gwa <- function(input) {
  x <- input * 277778 * (8760^-1) * (10^-6)
  return(x)
}

# Function: scale export technologies
scale_exp_parameter <- function(parname, msg.technology, tra.energy, varlist) {
  
  assign('df', read.csv(file.path(input, paste0('parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  
  assign('tradf', subset(trade.df, energy == tra.energy))
  
  if (nrow(tradf) == 0) {
    if (grepl('oil', msg.technology)) {
      assign('tradf', subset(trade.df, energy == 'oil')) # Export behavior for oil products ~ crude oil (when data missing)
    }
  }
  
  # Share of exports/imports to/from each region (disaggregate)
  if (grepl('exp', msg.technology)) {
    tradf <- group_by(tradf, msg_region1) %>% mutate(total_trade = sum(region_trade, na.rm = T))
  } else {
    stop("No trade technology specified (exp only for scaling)!")
  }
  
  tradf$share <- tradf$region_trade/tradf$total_trade
  tradf <- tradf[c('year', 'msg_region1', 'msg_region2', 'share')]
  
  tradf$msg_region1 <- paste0('R14_', tradf$msg_region1)
  tradf$msg_region2 <- paste0('R14_', tradf$msg_region2)
  
  if (grepl('exp', msg.technology)) {
    df <- left_join(df, tradf, by = c('node_loc' = 'msg_region1', 'year_act' = 'year'))
    df$technology <- paste0(msg.technology, '_', tolower(substr(df$msg_region2, 5, 7)))
  } 
  
  df <- subset(df, !is.na(share)) # Remove years with missing trade data
  
  df$value <- df$value * df$share

  df <- df[varlist] # Keep relevant variables
  
  return(df)
}

# Function: expand import technologies
expand_imp_parameter <- function(parname, msg.technology, tra.energy, varlist,
                                 figure.path = paste0(repo, 'figures/scale_msg_parameter/')) {
  
  assign('df', read.csv(file.path(input, paste0('parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  
  assign('tradf', subset(trade.df, energy == tra.energy))
  
  if (nrow(tradf) == 0) {
    if (grepl('oil', msg.technology)) {
      assign('tradf', subset(trade.df, energy == 'oil')) # Export behavior for oil products ~ crude oil (when data missing)
    }
  }
  
  tradf <- group_by(tradf, year, msg_region2) %>% summarise(imports = sum(region_trade, na.rm = T))
  tradf$imports <- convert_tj_gwa(tradf$imports)
  
  # Compare existing MESSAGE parameter with expanded parameter
  assign('msgdf', df[c('node_loc', 'year_act', 'value')])
  msgdf$node_loc <- substr(msgdf$node_loc, 5, 7)
  msgdf <- left_join(msgdf, tradf, by = c('year_act' = 'year', 'node_loc' = 'msg_region2'))
  msgdf <- subset(msgdf, !is.na(imports))
  
  assign('msgdf1', msgdf[c('node_loc', 'year_act', 'value')])
    msgdf1$source <- 'MESSAGE'
  assign('msgdf2', msgdf[c('node_loc', 'year_act', 'imports')])
    names(msgdf2) <- c('node_loc', 'year_act', 'value')
    msgdf2$source <- 'BACI'
  msgdf <- rbind(msgdf1, msgdf2)
  names(msgdf) <- c('Node', 'Year', 'Value', 'Source')
  assign('plot', ggplot(aes(x = Year, y = Value, colour = Node, group = Source), 
                        data = msgdf))
  plot <- plot + geom_point(aes(shape = Source, size = 2)) + 
          labs(title = paste0('Compare [', parname, '] values, MESSAGE vs. BACI'), 
               subtitle = paste0('Trade technology: ', msg.technology)) + 
          guides(size = FALSE)
  ggsave(file.path(figure.path, paste0('MSGvsBACI_', parname, '_', msg.technology, '.png')))
  
  # Reset trade dataframe (tradf) to be new parameter
  environment(build_parameter) <- environment(isid) <- environment()
  
  tradf$year_act <- tradf$year
  tradf$node_loc <- tradf$msg_region2
  tradf$value <- tradf$imports
  tradf <- subset(tradf, !is.na(year_act) & !is.na(node_loc) & !is.na(value))
  tradf <- tradf[c('year_act', 'node_loc', 'value')]
  isid('tradf', c('year_act', 'node_loc'))

  assign('par.out',
    build_parameter(parname = parname, varlist = varlist, technology = msg.technology,
                    unique_identifiers = c('node_loc', 'year_act'), value = tradf, value_constant = FALSE, unit = 'GWa',
                    node_loc = unique(tradf$node_loc), year_act = unique(tradf$year_act),
                    mode = 'M1', time = 'year'))
  return(par.out)
}
