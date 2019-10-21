##################################
# Build relation upper parameter #
##################################
build_relation_upper <- function(relation, value, year_rel) {
  
  assign('df', as.data.frame(expand.grid(year_rel, toupper(region.list))))
  
  df$relation <- relation
  df$unit <- "???"
  df$value <- 0
  
  names(df) <- c('year_rel', 'node_rel', 'relation', 'unit', 'value')
  
  df$node_rel <- paste0(region.number, '_', df$node_rel)
  
  return(df)
}
