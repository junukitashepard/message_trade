###############################################################
# Build parameters: growth_activity_lo and growth_activity_up #
###############################################################
# Run programs: growth_activity
scale_growth_activity <- function(msg.technology, lo_or_up, input_values = FALSE) {
  
  print(paste0("Trade technology = ", msg.technology))
  assign('df', read.csv(file.path(input, paste0('derived/parameters/growth_activity_', lo_or_up, '_', msg.technology, '.csv')), stringsAsFactors = F))
  df$technology <- NULL
  
  if (input_values == TRUE & lo_or_up == 'lo') {df$value <- value_lo}
  if (input_values == TRUE & lo_or_up == 'up') {df$value <- value_up}
  
  # Where missing (NA), take mean
  mean.df <- group_by(df, year_act) %>% summarize(value = mean(value, na.rm = T))
  value.missing <- data.frame()
  for (r in region.list) {
    if (!(paste0(region.number, '_', toupper(r)) %in% unique(df$node_loc))) {
      assign('indf', mean.df)
      indf$node_loc <- paste0(region.number, '_', toupper(r))
      indf$time <- 'year'
      indf$unit <- '%'
      value.missing <- rbind(value.missing, indf)
    }
  }
  
  if (nrow(value.missing) != 0) {
    value.missing <- value.missing[names(df)]
  }
  
  df <- rbind(df, value.missing)
  
  # EXPORTS
  if (grepl('exp', msg.technology) == T) {
    par <- expand.grid(paste0(msg.technology, '_', region.list), paste0(region.number, '_', toupper(region.list)))
    names(par) <- c('technology', 'node_loc')
    par <- subset(par, substr(node_loc, 5, 7) != toupper(gsub(paste0(msg.technology, '_'), '', technology)))
    par <- suppressWarnings(left_join(par, df, by = c('node_loc')))
  } else {
    # IMPORTS
    par <- df
  }
  
  saveRDS(par, file.path(output, paste0('analysis/msg_parameters/growth_activity_', lo_or_up, '/', msg.technology, '.rds')))
  write.csv(par, file.path(output, paste0('analysis/msg_parameters/growth_activity_', lo_or_up, '/', msg.technology, '.csv')))
}

for (tec in c(export_technologies, import_technologies)) {
    scale_growth_activity(tec, 'lo', input_values = TRUE)
    scale_growth_activity(tec, 'up', input_values = TRUE)
  }
  
clean_up()
