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
  
  assign('df', read.csv(file.path(input, paste0('parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  df <- subset(df, node_loc %in% paste0('R14_', toupper(regions)))
  
  # Expand existing technologies
  assign('basedf', expand.grid(paste0(msg.technology, '_', regions)))
  basedf <- as.data.frame(basedf[rep(seq_len(nrow(basedf)), nrow(df)),])
  basedf[, 1] <- as.character(basedf[,1])
  
  repdf <- df[rep(seq_len(nrow(df)), each = length(regions)),]
  
  assert('nrow(basedf) == nrow(repdf)')
  
  # Change technology to destination specific exports
  repdf$technology <- NULL
  names(basedf) <- 'technology'
  repdf <- cbind(repdf, basedf)
  
  saveRDS(repdf, file.path(output, paste0('relation_activity/', energy, '_exp.rds')))
  write.csv(repdf, file.path(output, paste0('relation_activity/', energy, '_exp.csv')))
  
  # IMPORTS (KEEP SAME)
  print(paste0('Technology = ', energy, '_imp'))
  assign('msg.technology', paste0(energy, '_imp'))
  assign('parname', 'relation_activity')
  
  assign('outdf', read.csv(file.path(input, paste0('parameters/', parname, '_', msg.technology, '.csv')), stringsAsFactors = F))
  outdf <- subset(outdf, node_loc %in% paste0('R14_', toupper(regions)))
  
  saveRDS(outdf, file.path(output, paste0('relation_activity/', energy, '_imp.rds')))
  write.csv(outdf, file.path(output, paste0('relation_activity/', energy, '_imp.csv')))
  
}

# Run program
for (e in energy_list) {
  build_relation_activity(e)
}