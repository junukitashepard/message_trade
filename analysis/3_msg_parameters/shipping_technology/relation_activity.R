#####################################
# Build relation activity parameter #
#####################################
build_relation_activity <- function(general_shipping_type, energy_list, year_act) {
  
  # Build right hand side (shipping activity)
  ###########################################
  parname <- 'shipping_activity'
  varlist <- c('relation', 'node_loc', 'technology', 'year_act', 'mode', 'value', 'unit') # add year_rel, node_rel ex-post
  value <- -1
  unit <- '???'
  mode <- 'M1'
  
  shipping_activity <- data.frame()
  
  for (f in c('diesel', 'LNG', 'elec')) {
    sa.in <- 
      build_parameter(parname = parname, varlist = varlist, technology = paste0(general_shipping_type, '_', f),
                      node_loc = paste0('R14_', toupper(regions)),
                      year_act = year_act, 
                      mode = mode, relation = paste0('lim_', general_shipping_type),
                      value = value, unit = unit)
    sa.in$year_rel <- sa.in$year_act
    sa.in$node_rel <- sa.in$node_loc
    
    shipping_activity <- rbind(as.data.frame(shipping_activity), as.data.frame(sa.in))
  }
  
  # Build left hand side (trade activity)
  #######################################
  parname <- 'trade_activity'
  varlist <- c('relation', 'node_loc', 'technology', 'year_act', 'mode', 'value', 'unit') # add year_rel, node_rel ex-post
  value <- 0 # change ex-post
  unit <- '???'
  mode <- 'M1'
  
  trade_activity <- data.frame()
  for (r in regions) {
    
    for (e in energy_list) {
      
      assign('tec.in', paste0(e, '_exp_', r))
      assign('tec.node', r)
      
      assign('ta.in',  
             build_parameter(parname = parname, varlist = varlist, technology = tec.in,
                             node_loc = paste0('R14_', toupper(regions)),
                             year_act = year_act, 
                             mode = mode, relation = paste0('lim_', general_shipping_type),
                             value = value, unit = unit))
      
      ta.in$year_rel <- ta.in$year_act
      ta.in$node_rel <- ta.in$node_loc
      
      trade_activity <- rbind(as.data.frame(trade_activity), as.data.frame(ta.in))
    }
  }
  
  trade_activity$node_loc <- as.character(trade_activity$node_loc)
  trade_activity$tec_node <- paste0('R14_', toupper(substr(trade_activity$technology, 
                                                           nchar(trade_activity$technology)-2, 
                                                           nchar(trade_activity$technology))))
  
  trade_activity <- subset(trade_activity, node_loc != tec_node) # don't allow node_loc to export to itself
  
  # Generate V = ('energy'_heat_rate)^-1 * mean(distance_'exporter'_'importer')
  ncv$msgregion_new <- paste0('R14_', ncv$msgregion)
  
  trade_activity_out <- data.frame()
  for (e in energy_list) {
    
    assign('df', subset(trade_activity, substr(technology, 1, nchar(technology) - 8) == e))
    
    assign('ncv.df', subset(ncv, grepl(e, energy))[c('msgregion_new', 'energy', 'value')])
    if (e == 'oil') {ncv.df <- subset(ncv.df, energy == 'oil')}
    names(ncv.df) <- c('msgregion', 'energy', 'ncv')
    
    df <- inner_join(df, ncv.df, by = c('node_loc' = 'msgregion'))
    df <- inner_join(df, distances, by = c('node_loc' = 'msgregion1', 'tec_node' = 'msgregion2'))
    df$value <- df$ncv * df$mean_distance
    df$ncv <- df$mean_distance <- df$tec_node <- df$energy <- NULL
    
    trade_activity_out <- rbind(as.data.frame(trade_activity_out), as.data.frame(df))
  }
  
  # Combine shipping activity and trading activity for relation_activity
  ######################################################################
  shipping_activity <- shipping_activity[c('relation', 'technology', 'mode', 'value', 'unit', 
                                           'year_act', 'node_loc', 'year_rel', 'node_rel')]
  trade_activity <- trade_activity_out[c('relation', 'technology', 'mode', 'value', 'unit', 
                                         'year_act', 'node_loc', 'year_rel', 'node_rel')]
  
  relation_activity <- rbind(shipping_activity, trade_activity)
  
  return(relation_activity)
}
