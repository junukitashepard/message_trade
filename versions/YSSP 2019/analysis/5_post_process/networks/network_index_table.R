###############################
# Table of network indices #
# Variable = ACT #
###############################
rm(list = ls())
wd <- "C:/Users/jus3/message_trade"
repo <- "C:/Users/jus3/message_trade"
msg_dir <- "C:/Users/jus3/message_trade/gdx_files"
output <- paste0(repo, 'tables')
setwd(wd)

igdx('C:/GAMS/win64/29.1') # Set GDX path

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)
library('openxlsx')

library('RColorBrewer')
ncolors <- 14
mycolors <- colorRampPalette(brewer.pal(8, 'Paired'))(ncolors)

# Conversion factor
gwa_to_ej <- 8760*3600*(10^-9)

# Plot GLOBAL baseline #
########################
add_scenario <- function(scenario_name, message_version) {
  
  df <- read_MESSAGE(msg_scenario = scenario_name, msg_version = message_version, msg_variable = 'ACT') # Baseline (global schema)

  df <- subset(df, grepl('_exp_', tec))
  
  df$exporter <- gsub('R14_', '', df$node)
  df$importer <- toupper(gsub('.*_exp_', '', df$tec))
  
  df$energy <- gsub('_exp_.*', '', df$tec)
  df$year <- df$year_all
  
  df <- subset(df, energy != 'gas')
  
  df <- unique(df[c('exporter', 'importer', 'energy', 'year', 'value')])
  df$scenario <- scenario_name
  
  assign('activity', rbind(activity, df), envir = parent.frame())
}

# Build base activity df
activity <- data.frame()

add_scenario('baseline', 5)
add_scenario('tariff_high', 4)
add_scenario('tariff_low', 4)
add_scenario('CO2_tax_baseline', 2)
add_scenario('CO2_tax_tariff_high', 2)
add_scenario('CO2_tax_tariff_low', 2)

activity <- subset(activity, importer != 'GLB' & exporter != 'GLB')

activity$year <- as.numeric(activity$year)
activity$value <- as.numeric(activity$value) * gwa_to_ej

scenario_list <- c('baseline', 'tariff_high', 'tariff_low', 
                   'CO2_tax_baseline', 'CO2_tax_tariff_high', 'CO2_tax_tariff_low') 

# Build table of network statistics
build_table <- function(table_year) {

  df <- subset(activity, year == table_year)
  
  basemat <- matrix(nrow = 23, ncol = 6)
  colnames(basemat) <- scenario_list
  rownames(basemat) <- c('N_exporter', 'N_importer', 'N_link', 'total_trade',
                         'mean_trade', 'sd_trade', 'min_trade', 'min_pair', 'max_trade', 'max_pair',
                         'mean_links', 'sd_links', 'outdegree', 'sd_outdegree', 'indegree', 'sd_indegree',
                         'space', 'coal', 'eth', 'foil', 'loil', 'LNG', 'oil')
  
  # Fill in for each scenario
  for (scen in colnames(basemat)) {
    
    invec <- vector()
    vecdf <- subset(df, scenario == scen)
    vecdf <- subset(vecdf, value > 0)
  
    invec['N_exporter'] <- length(unique(vecdf$exporter))
    invec['N_importer'] <- length(unique(vecdf$importer))
    invec['N_link'] <- nrow(unique(vecdf))
    invec['total_trade'] <- round(sum(vecdf$value),2)
    invec['mean_trade'] <- round(mean(vecdf$value),2)
    invec['sd_trade'] <- round(sd(vecdf$value),2)
    invec['min_trade'] <- round(min(vecdf$value),2)
    invec['min_pair'] <- paste0(vecdf$exporter[vecdf$value == min(vecdf$value)], ' to ', vecdf$importer[vecdf$value == min(vecdf$value)])
    invec['max_trade'] <- round(max(vecdf$value),2)
    invec['max_pair'] <- paste0(vecdf$exporter[vecdf$value == max(vecdf$value)], ' to ', vecdf$importer[vecdf$value == max(vecdf$value)])
  
    # Number of links by node
    linkdf1 <- vecdf[c('importer', 'exporter')]
    linkdf2 <- vecdf[c('exporter', 'importer')]
    names(linkdf1) <- names(linkdf2) <- c('region1', 'region2')
    linkdf <- rbind(linkdf1, linkdf2)
    
    linkdf$n <- 1
    linkdf <- group_by(linkdf, region1) %>% summarize(nlinks = sum(n))
    
    invec['mean_links'] <- round(mean(linkdf$nlinks), 2)
    invec['sd_links'] <- round(sd(linkdf$nlinks), 2)
  
    # Centrality
    degdf <- vecdf
    degdf$n <- 1
    degdf <- group_by(degdf, exporter) %>% mutate(outdegree = sum(n))
    degdf <- group_by(degdf, importer) %>% mutate(indegree = sum(n))
    
    max_degree <- (length(unique(c(vecdf$importer, vecdf$exporter)))-1) * length(unique(vecdf$energy))
    
    degdf$outdegree_centrality <- degdf$outdegree/max_degree
    degdf$indegree_centrality <- degdf$indegree/max_degree
    
    invec['outdegree'] <- round(mean(degdf$outdegree_centrality), 2)
    invec['sd_outdegree'] <- round(sd(degdf$outdegree_centrality), 2)
    invec['indegree'] <- round(mean(degdf$indegree_centrality), 2)
    invec['sd_indegree'] <- round(sd(degdf$indegree_centrality), 2)
    
    # Energy-specific trade
    invec['space'] <- ''
    
    for (e in unique(vecdf$energy)) {
      edf <- subset(vecdf, energy == e)
      invec[e] <- round(sum(edf$value), 2)
    }
    
    invec <- invec[rownames(basemat)]
    
    basemat[, scen] <- invec
  }
  
  return(basemat)
}

table_2020 <- build_table(2020)
table_2050 <- build_table(2050)

# Insert table into spreadsheet (Excel)
wb <- loadWorkbook(file.path(output, 'network_indices.xlsx'))

writeData(wb, sheet = "Network_Index_2020", table_2020, 
          startRow = 5, startCol = 3,
          colNames = FALSE, rowNames = FALSE)

writeData(wb, sheet = "Network_Index_2050", table_2050, 
          startRow = 5, startCol = 3,
          colNames = FALSE, rowNames = FALSE)

###########################################
# Distributions of trade flow and degrees #
###########################################
distdf <- activity
distdf <- subset(distdf, importer != 'GLB' & exporter != 'GLB')

max_degree <- (length(unique(c(distdf$exporter, distdf$importer)))-1)*length(unique(distdf$energy))

distdf$n <- 1
distdf <- group_by(distdf, exporter, scenario, year) %>% mutate(outdegree = sum(n))
distdf <- group_by(distdf, importer, scenario, year) %>% mutate(indegree = sum(n))

distdf$outdegree_centrality <- distdf$outdegree/max_degree
distdf$indegree_centrality <- distdf$indegree/max_degree

ggplot(data = subset(distdf, year == 2050 & energy == 'LNG' & value > 1),
       aes(x = value, fill = scenario)) +
  geom_density(alpha = 0.2)

##################################
# Ranking by centrality and flow #
##################################
rankdf <- distdf

rank_by <- 'outdegree_centrality'
rank_year <- 2020
rank_region <- 'exporter'

build_ranking <- function(rank_by, rank_year, rank_region) {
  
  rankmat <- matrix(nrow = 5, ncol = 0)
  
  for (s in scenario_list) {
    
    df <- subset(rankdf, scenario == s & year == rank_year)
    
    df[, 'rankby'] <- df[, rank_by]
    df[, 'rankregion'] <- df[, rank_region]
    
    if (rank_by == 'value') {
      df <- group_by(df, rankregion) %>% summarize(rankby = sum(rankby))
    } else {
      df <- unique(df[c('rankregion', 'rankby')])
    }

    df <- arrange(df, desc(rankby))
    
    df <- df[1:5,]
    
    vec <- paste0(df$rankregion, ' [', round(df$rankby,3), ']')
    rankmat <- cbind(rankmat, vec)
  }
  
  colnames(rankmat) <- scenario_list

  return(rankmat)
}

# Build rankings
for (year in c(2020, 2050)) {
  rank_outdegree <- build_ranking('outdegree_centrality', year, 'exporter')
  rank_indegree <- build_ranking('indegree_centrality', year, 'importer')
  rank_exports <- build_ranking('value', year, 'exporter')
  rank_imports <- build_ranking('value', year, 'importer')
  
  space_holder <- matrix(nrow = 2, ncol = 6)
  space_holder[] <- ''
  rank_out <- rbind(rank_outdegree, space_holder,
                    rank_indegree, space_holder,
                    rank_exports, space_holder,
                    rank_imports, space_holder)
  assign(paste0('rank_', year), rank_out, envir = parent.frame())
}

# Insert table into spreadsheet (Excel)
writeData(wb, sheet = "Degree_Centrality_2020", rank_2020, 
          startRow = 5, startCol = 3,
          colNames = FALSE, rowNames = FALSE)

writeData(wb, sheet = "Degree_Centrality_2050", rank_2050, 
          startRow = 5, startCol = 3,
          colNames = FALSE, rowNames = FALSE)

# Save compiled table
saveWorkbook(wb, file.path(output, 'network_indices_FORMATTED.xlsx'), overwrite = T)

#########################################################
# Correlation between export-outdegree, import-indegree #
#########################################################
exportdf <- group_by(rankdf, exporter, scenario, year, outdegree_centrality) %>% summarize(exports = sum(value))
  isid('exportdf', c('exporter', 'scenario', 'year'))

importdf <- group_by(rankdf, importer, scenario, year, indegree_centrality) %>% summarize(imports = sum(value))
  isid('importdf', c('importer', 'scenario', 'year')) 

for (year in c(2020, 2050)) {
  print('******')
  print(paste0('*', year, '*'))
  print('******')
  
  print('Cor(indegree_centrality|imports)')
  cordf.imports <- subset(importdf, year == 2020)  
  print(cor(cordf.imports$indegree_centrality, cordf.imports$imports))
  
  print('Cor(outdegree_centrality|exports)')
  cordf.exports <- subset(exportdf, year == 2020)  
  print(cor(cordf.exports$outdegree_centrality, cordf.exports$exports))
}