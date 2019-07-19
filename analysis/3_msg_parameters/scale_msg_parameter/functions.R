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
  
  print(paste0("Trade technology = ", msg.technology))
  
  assign('df', read.csv(file.path(input, paste0('parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  
  df <- subset(df, node_loc %in% paste0('R14_', toupper(regions)))
  
  assign('msgdf_base', df)
  assign('tradf', subset(trade.df, energy == tra.energy))
  
  if (nrow(tradf) == 0) {
    if (grepl('oil', msg.technology)) {
      assign('tradf', subset(trade.df, energy == 'oil')) # Export behavior for oil products ~ crude oil (when data missing)
    }
  }
  
  # Share of exports/imports to/from each region (disaggregate)
  if (grepl('exp', msg.technology)) {
    tradf <- group_by(tradf, msg_region1, year) %>% mutate(total_trade = sum(region_trade, na.rm = T))
  } else {
    stop("No trade technology specified (exp only for scaling)!")
  }
  
  tradf$share <- tradf$region_trade/tradf$total_trade
  tradf <- tradf[c('year', 'msg_region1', 'msg_region2', 'share')]
  
  tradf$msg_region1 <- paste0('R14_', tradf$msg_region1)
  tradf$msg_region2 <- paste0('R14_', tradf$msg_region2)
  
  tradf$share[is.nan(tradf$share)] <- 0
  
  if (grepl('exp', msg.technology)) {
    
    if ('year_act' %in% varlist) {
      
      for (y in unique(df$year_act[!(df$year_act %in% tradf$year)])) {
        if (y < min(tradf$year, na.rm = T)) {
          assign('miny', min(tradf$year, na.rm = T))
          assign('oldtradf', subset(tradf, year == miny))
          oldtradf$year <- y
          tradf <- rbind(tradf, oldtradf)
        } 
      }
      df <- left_join(df, tradf, by = c('node_loc' = 'msg_region1', 'year_act' = 'year'))
      
      if (any(unique(msgdf_base$year_act[!(msgdf_base$year_act %in% tradf$year)]) > max(tradf$year, na.rm = T))) {
        environment(add_future_year_act) <- environment()
        add_future_year_act()
        df <- rbind(df, msgdf_future)
      }
      
    } else if ('year_vtg' %in% varlist) {
      
      for (y in unique(df$year_vtg[!(df$year_vtg %in% tradf$year)])) {
        if (y < min(tradf$year, na.rm = T)) {
          assign('miny', min(tradf$year, na.rm = T))
          assign('oldtradf', subset(tradf, year == miny))
          oldtradf$year <- y
          tradf <- rbind(tradf, oldtradf)
        }
      }

      df <- left_join(df, tradf, by = c('node_loc' = 'msg_region1', 'year_vtg' = 'year'))
    }
    df$technology <- paste0(msg.technology, '_', tolower(substr(df$msg_region2, 5, 7)))
  } 
  
  df <- subset(df, !is.na(share)) # Remove years with missing trade data
  
  df$value <- df$value * df$share

  df <- df[varlist] # Keep relevant variables
  
  return(df)
}

# Function: Add future years (where year_act > trade year)
add_future_year_act <- function() {
  assign('tradf_future', group_by(tradf, msg_region1, msg_region2) %>% summarize(mean(share, na.rm = T)))
  
  assign('msgdf_future', subset(msgdf_base, year_act > max(tradf$year, na.rm = T)))
  
  assign('msgdf_future_years', sort(unique(msgdf_future$year_act)))
  
  assign('msgdf_future_base', expand.grid(unique(msgdf_base$node_loc), paste0(msg.technology, '_', regions)))
  names(msgdf_future_base) <- c('node_loc', 'technology')
  
  msgdf_future_years <- rep(msgdf_future_years, nrow(msgdf_future_base))
  
  msgdf_future_base <- msgdf_future_base[rep(seq_len(nrow(msgdf_future_base)), length(unique(msgdf_future_years))),]
  msgdf_future_base <- arrange(msgdf_future_base, node_loc, technology)
  
  msgdf_future_base$year_act <- msgdf_future_years
  msgdf_future_base$node_loc <- as.character(msgdf_future_base$node_loc)
  msgdf_future_base$technology <- as.character(msgdf_future_base$technology)
  
  msgdf_future_base$msg_region2 <- paste0('R14_', 
                                          toupper(substr(msgdf_future_base$technology, 
                                                         nchar(msgdf_future_base$technology) - 2,
                                                         nchar(msgdf_future_base$technology))))
  
  tradf_future <- left_join(msgdf_future_base, tradf_future, by = c('node_loc'= 'msg_region1',
                                                                    'msg_region2'))
  names(tradf_future) <- c('node_loc', 'technology', 'year_act', 'msg_region2', 'mean_share')
  tradf_future <- subset(tradf_future, node_loc != msg_region2 & !is.nan(mean_share))
  tradf_future <- tradf_future[c('node_loc', 'technology', 'year_act', 'mean_share', 'msg_region2')]
  
  msgdf_future <- msgdf_future[c('node_loc', 'year_act', 'mode', 'time', 'value', 'unit')]
  msgdf_future <- left_join(msgdf_future, tradf_future, by = c('node_loc', 'year_act'))
  msgdf_future <- subset(msgdf_future, !is.na(technology))
  msgdf_future$share <- msgdf_future$mean_share
  
  msgdf_future <- msgdf_future[c('node_loc', 'technology', 'year_act', 'mode', 
                                 'time', 'value', 'unit','msg_region2', 'share')]
  
  # Where value == 0, make it the minimum of non-zero values
  assign('min_value', min(msgdf_future$value[msgdf_future$value > 0], na.rm = T))
    if (is.infinite(min_value)) {min_value <- 10}
  
  msgdf_future$value[msgdf_future$value == 0] <- min_value
  assign('msgdf_future', msgdf_future, envir = parent.frame())
  
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
  tradf$node_loc <- paste0('R14_', tradf$msg_region2)
  tradf$value <- tradf$imports
  tradf <- subset(tradf, !is.na(year_act) & !is.na(node_loc) & !is.na(value))
  tradf <- tradf[c('year_act', 'node_loc', 'value')]
  isid('tradf', c('year_act', 'node_loc'))

  assign('df_future', subset(unique(df[c('year_act', 'node_loc', 'value')]), year_act > max(tradf$year_act, na.rm = T)))
  tradf <- rbind(tradf, df_future)
  isid('tradf', c('year_act', 'node_loc', 'value'))
  
  assign('par.out',
    build_parameter(parname = parname, varlist = varlist, technology = msg.technology,
                    unique_identifiers = c('node_loc', 'year_act'), value = tradf, value_constant = FALSE, unit = 'GWa',
                    node_loc = unique(tradf$node_loc), year_act = unique(tradf$year_act),
                    mode = 'M1', time = 'year'))
  return(par.out)
}
