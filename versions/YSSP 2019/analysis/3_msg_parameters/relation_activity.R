###################################################
# Expand parameters: relation_activity
###################################################
# Function: build historical_new_capacity parameter
build_relation_activity <- function(energy) {
  
  environment(assert) <- environment()
  print(paste0('Building parameter [relation_activity]'))
  
  # EXPORTS
  print(paste0('Technology = ', energy, '_exp'))
  assign('msg.technology', paste0(energy, '_exp'))
  assign('parname', 'relation_activity')
  
  assign('df', read.csv(file.path(input, paste0('derived/parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  df <- subset(df, node_loc %in% paste0(region.number, '_', toupper(region.list)))
  
  # Expand existing technologies
  assign('basedf', expand.grid(paste0(msg.technology, '_', region.list)))
  basedf <- as.data.frame(basedf[rep(seq_len(nrow(basedf)), nrow(df)),])
  basedf[, 1] <- as.character(basedf[,1])
  
  repdf <- df[rep(seq_len(nrow(df)), each = length(region.list)),]
  
  assert('nrow(basedf) == nrow(repdf)')
  
  # Change technology to destination specific exports
  repdf$technology <- NULL
  names(basedf) <- 'technology'
  repdf <- cbind(repdf, basedf)
  
  # Remove destination-specific exports = node_loc
  repdf$tec_node <- paste0(region.number, '_', 
                           toupper(substr(repdf$technology, nchar(repdf$technology)-2, nchar(repdf$technology))))
  
  repdf <- subset(repdf, tec_node != node_loc & tec_node != node_rel)
  repdf$tec_node <- NULL
  
  # Add a relation to get 'resource'_trd = sum of all exports
  ############################################################
  assign('varlist',  c('relation', 'node_loc', 'technology', 'year_act', 'mode', 'value', 'unit')) # add year_rel, node_rel ex-post
  
  if (energy == 'lh2') {
    energy.in <- 'liquidh2'
  } else {
    energy.in <- energy
  }
  
  # All exports
  assign('region_exports', 
         build_parameter(parname = 'all_exports', varlist = varlist, technology = paste0(energy, '_exp'),
                         node_loc = paste0(region.number, '_', toupper(region.list)),
                         year_act = year_act, 
                         mode = 'M1', relation = paste0('lim_', energy.in, '_trd'),
                         value = -1, unit = '???'))
  
  region_exports$year_rel <- region_exports$year_act
  region_exports$node_rel <- paste0(region.number, '_GLB')
  
  # Global trade
  assign('global_trade', 
         build_parameter(parname = 'all_exports', varlist = varlist, technology = paste0(energy.in, '_trd'),
                         node_loc = paste0(region.number, '_GLB'),
                         year_act = year_act, 
                         mode = 'M1', relation = paste0('lim_', energy.in, '_trd'),
                         value = 1, unit = '???'))
  
  global_trade$year_rel <- global_trade$year_act
  global_trade$node_rel <- paste0(region.number, '_GLB')

  glb_relation <- rbind(region_exports, global_trade)
  
  # Add a relation to get total exports = sum of all bilateral exports
  ######################################################################
  assign('varlist',  c('relation', 'node_loc', 'technology', 'year_act', 'mode', 'value', 'unit')) # add year_rel, node_rel ex-post

  # All exports
  assign('all_exports', 
    build_parameter(parname = 'all_exports', varlist = varlist, technology = paste0(energy, '_exp'),
                    node_loc = paste0(region.number, '_', toupper(region.list)),
                    year_act = year_act, 
                    mode = 'M1', relation = paste0('lim_', energy, '_exp'),
                    value = -1, unit = '???'))
  
  all_exports$year_rel <- all_exports$year_act
  all_exports$node_rel <- all_exports$node_loc
  
  # Bilateral exports
  assign('bilateral_exports', data.frame())
  
  for (r in c(region.list, 'glb')) {
      
    assign('tec.in', paste0(energy, '_exp_', r))
      
    assign('ta.in',  
            build_parameter(parname = 'bilateral_exports', varlist = varlist, technology = paste0(energy, '_exp_', r),
                            node_loc = paste0(region.number, '_', toupper(region.list)),
                            year_act = year_act, 
                            mode = 'M1', relation = paste0('lim_', energy, '_exp'),
                            value = 1, unit = '???'))
      
    ta.in$year_rel <- ta.in$year_act
    ta.in$node_rel <- ta.in$node_loc
      
    bilateral_exports <- rbind(as.data.frame(bilateral_exports), as.data.frame(ta.in))
    }
  
  assign('export_relation', rbind(all_exports, bilateral_exports))
         
  # Remove destination-specific exports = node_loc
  export_relation$tec_node <- paste0(region.number, '_', 
                           toupper(substr(export_relation$technology, 
                                          nchar(export_relation$technology)-2, 
                                          nchar(export_relation$technology))))
  
  export_relation <- subset(export_relation, tec_node != node_loc & tec_node != node_rel)
  export_relation$tec_node <- NULL
  
  repdf <- rbind(repdf, export_relation, glb_relation)
  
  saveRDS(repdf, file.path(output, paste0('analysis/msg_parameters/relation_activity/', energy, '_exp.rds')))
  write.csv(repdf, file.path(output, paste0('analysis/msg_parameters/relation_activity/', energy, '_exp.csv')))
  
  # IMPORTS (KEEP SAME)
  print(paste0('Technology = ', energy, '_imp'))
  assign('msg.technology', paste0(energy, '_imp'))
  assign('parname', 'relation_activity')
  
  assign('outdf', read.csv(file.path(input, paste0('derived/parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  outdf <- subset(outdf, node_loc %in% paste0(region.number, '_', toupper(region.list)))
  
  saveRDS(outdf, file.path(output, paste0('analysis/msg_parameters/relation_activity/', energy, '_imp.rds')))
  write.csv(outdf, file.path(output, paste0('analysis/msg_parameters/relation_activity/', energy, '_imp.csv')))
  
}

# Run program
for (e in energy.types) {
  build_relation_activity(e)
}

# Build upper and lower limits
build_relation_limits <- function(relation, value, node_rel, year_rel) {
  
  assign('df', as.data.frame(expand.grid(year_rel, node_rel)))
  
  df$relation <- relation
  df$unit <- "???"
  df$value <- 0
         
  names(df) <- c('year_rel', 'node_rel', 'relation', 'unit', 'value')
         
  return(df)
}

for (e in energy.types) {
  if (e == 'lh2') {
    e.in <- 'liquidh2'
  } else {
    e.in <- e
  }
  ul_export_relation <- build_relation_limits(relation = paste0('lim_', e, '_exp'), 
                                       value = 0, 
                                       node_rel = paste0(region.number, '_', toupper(region.list)),
                                       year_rel = year_act)
  
  ll_export_relation <- build_relation_limits(relation = paste0('lim_', e, '_exp'), 
                                       value = 0, 
                                       node_rel = paste0(region.number, '_', toupper(region.list)),
                                       year_rel = year_act)
  ul_glb_relation <- build_relation_limits(relation = paste0('lim_', e.in, '_trd'), 
                                              value = 0, 
                                              node_rel = paste0(region.number, '_', toupper(region.list)),
                                              year_rel = year_act)
  
  ll_glb_relation <- build_relation_limits(relation = paste0('lim_', e.in, '_trd'), 
                                              value = 0, 
                                              node_rel = paste0(region.number, '_', toupper(region.list)),
                                              year_rel = year_act)
  
  upper_limit <- rbind(ul_export_relation, ul_glb_relation)
  lower_limit <- rbind(ll_export_relation, ll_glb_relation)
  
  write.csv(upper_limit, file.path(output, paste0('analysis/msg_parameters/relation_upper/', e, '_exp.csv')))
  write.csv(lower_limit, file.path(output, paste0('analysis/msg_parameters/relation_lower/', e, '_exp.csv')))
}