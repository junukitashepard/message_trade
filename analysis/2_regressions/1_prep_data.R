####################### 
# Regression analysis #
# Pre-process data    #
#######################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output/derived")
output <-   paste0(wd, "output/analysis/regress")
temp <-     paste0(wd, "temp/")

##############################
# Import files
trade <- readRDS(file.path(input, 'trade/trade.rds'))
  isid('trade', c('iso.i', 'iso.j', 'energy', 'year'))
  
ijports <- readRDS(file.path(input, 'nodes/ij_ports_fixednames.rds'))
  ijports <- unique(ijports[c('port1.iso', 'port2.iso', 'port1', 'port2', 'distance')])
  isid('ijports', c('port1.iso', 'port2.iso'))
  
regional_spec <- read.csv(file.path(raw, 'UserInputs/regional_specification.csv'), stringsAsFactors = F)
  names(regional_spec) <- c('iso', 'msg.region')
  regional_spec <- subset(regional_spec, iso != "" & !is.na(iso))
  isid('regional_spec', c('iso'))

disputes <- readRDS(file.path(input, "trade/trade_disputes.rds"))
  names(disputes) <- c('i', 'j', 'year', 'ds_outcome', 'lag1.ds_energy', 'lag1.ds_any')
  all.outcomes <- c('Dropped', 'In progress', 'MAS', 'Ruling', 'Withdrawn')
  td.outcomes <- all.outcomes # make flexible, change if only keeping one kind of ruling
  disputes <- subset(disputes, ds_outcome %in% td.outcomes)[c('i', 'j', 'year', 'lag1.ds_energy', 'lag1.ds_any')]
  
conflicts <- readRDS(file.path(input, 'conflicts/armed_conflicts.rds'))

# Link regions to trade
trade <- left_join(trade, regional_spec, by = c('iso.i' = 'iso'))
trade <- left_join(trade, regional_spec, by = c('iso.j' = 'iso'))
names(trade)[10:11] <- c('msg.region.i', 'msg.region.j')

# Calculate energy price ("variable cost") in $1000/GWa
trade$q_e_gwa <- (((trade$q_e*1e12)/(3.6*1e6))*8760)/10e9
trade$var_cost <- (trade$v)/trade$q_e_gwa

# Use leading year for conflict and disputes
disputes$lead1.year = disputes$year + 1
disputes$year <- NULL

conflicts$lead1.year <- conflicts$year + 1
conflicts$year <- NULL
names(conflicts) <- c('i', 'j', 'lag1.minorconflict', 'lag1.war', 'lead1.year')

# Make disputes/conflicts unique by i-j-year
disputes <- dplyr::group_by(disputes, i, j, lead1.year) %>%
            dplyr::summarise(lag1.ds_energy = sum(lag1.ds_energy),
                             lag1.ds_any = sum(lag1.ds_any))
isid('disputes', c('i', 'j', 'lead1.year'))

# Compile embedded disputes and armed conflicts #
#################################################
# Function: embedded disputes/armed conflict
embed_var <- function(infile, embed.vlist, y) {

  # Set up base file with indicator for i-j trade
  assign('clist', unique(c(unique(trade$iso.i), unique(trade$iso.j))))
  assign('base', expand.grid(clist, clist))
    base$Var1 <- as.character(base$Var1)
    base$Var2 <- as.character(base$Var2)
    names(base) <- c('i', 'j')
   
  assign('trind.df', unique(subset(trade, year == y & q_e > 0)[c('iso.i', 'iso.j')]))
    trind.df$trade_ind <- 1
  
  base <- left_join(base, trind.df, by = c('i' = 'iso.i', 'j' = 'iso.j'))
  base <- left_join(base, trind.df, by = c('j' = 'iso.i', 'i' = 'iso.j'))
    base$trade_ind <- base$trade_ind.x[is.na(base$trade_ind.x)] <- base$trade_ind.y[is.na(base$trade_ind.y)] <- 0
    base$trade_ind[base$trade_ind.x == 1] <- base$trade_ind.x[base$trade_ind.x == 1]
    base$trade_ind[base$trade_ind.y == 1] <- base$trade_ind.y[base$trade_ind.y == 1]
    base$trade_ind.x <- base$trade_ind.y <- NULL
    base <- subset(base, !is.na(i) & !is.na(j))
    
  assign('indf', get(infile)) # Import infile
    names(indf)[names(indf) == 'i'] <- 'k1'
    names(indf)[names(indf) == 'j'] <- 'k2'
    indf <- subset(indf, year == y)[c('k1', 'k2', embed.vlist)]
    assign('indf2', indf)
      names(indf2) <- c('k2', 'k1', embed.vlist)
      indf2 <- indf2[c('k1', 'k2', embed.vlist)]
    indf <- unique(rbind(indf, indf2))
  
  for (v in c('i', 'j')) {
    assign('df', base)
    
    names(df)[names(df) == v] <- 'v'
    if (v == 'i') {v2 <- 'j'} else {v2 <- 'i'}
    
    df <- left_join(df, indf, by = c('v' = 'k1'))
    
    df <- subset(df, trade_ind == 1 & !is.na(k2))
    df <- df[c(v2, 'k2', embed.vlist)]
    names(df) <- c('i', 'j', paste0('embed.', embed.vlist))
    assign(paste0('edf.', v), df, envir = parent.frame())
  }
    
  assign('edf', unique(rbind(as.data.frame(edf.i), as.data.frame(edf.j))))
  assign('outdf', left_join(base, edf, by = c('i', 'j'))[c('i', 'j', paste0('embed.', embed.vlist))])
    outdf$year <- y
    outdf <- subset(outdf, i != j)
  return(outdf)
}

# Embedded trade disputes
print('Embedding trade disputes')

mdisputes <- disputes
names(mdisputes)[names(mdisputes) == 'lead1.year'] <- 'year'

embedded.disputes <- data.frame(i = character(0), j = character(0), 
                                embed.lag1.ds_energy = numeric(0), embed.lag1.ds_any = numeric(0), 
                                year = numeric(0))

for (year in 1995:2012) {
  print(year)
  assign('emdf', embed_var(infile = 'mdisputes', embed.vlist = c('lag1.ds_energy', 'lag1.ds_any'), y = year))
  embedded.disputes <- rbind(embedded.disputes, emdf)
}
embedded.disputes <- subset(embedded.disputes, !is.na(embed.lag1.ds_energy) & !is.na(embed.lag1.ds_any))
embedded.disputes <- dplyr::group_by(embedded.disputes, i, j, year) %>%
                     dplyr::summarise(embed.lag1.ds_energy = sum(embed.lag1.ds_energy),
                                      embed.lag1.ds_any = sum(embed.lag1.ds_any))

# Embedded armed conflict
print('Embedding armed conflict variables')

mconflicts <- conflicts
names(mconflicts)[names(mconflicts) == 'lead1.year'] <- 'year'

embedded.conflict <- data.frame(i = character(0), j = character(0),
                                embed.minor = numeric(0), embed.war = numeric(0),
                                year = numeric(0))
for (year in 1995:2014) {
  print(year)
  assign('emdf', embed_var(infile = 'conflicts', embed.vlist = c('lag1.minorconflict', 'lag1.war'), y = year))
  embedded.conflict <- rbind(embedded.conflict, emdf)
}
embedded.conflict <- subset(embedded.conflict, !is.na(embed.lag1.minorconflict) & !is.na(embed.lag1.war))
embedded.conflict <- dplyr::group_by(embedded.conflict, i, j, year) %>%
                     dplyr::summarise(embed.lag1.minorconflict = sum(embed.lag1.minorconflict),
                                      embed.lag1.war = sum(embed.lag1.war))
rm(list = c('edf.i', 'edf.j', 'emdf'))

# Link other parameters to underlying trade data #
##################################################
# Link ports to trade data
df <- left_join(trade, ijports, by = c('iso.i' = 'port1.iso', 
                                          'iso.j' = 'port2.iso'))
isid('df', c('iso.i', 'iso.j', 'year', 'energy'))

# Link trade disputes in t-1
df <- left_join(df, disputes, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'lead1.year'))
df$lag1.ds_any[is.na(df$lag1.ds_any)] <- df$lag1.ds_energy[is.na(df$lag1.ds_energy)] <- 0
isid('df', c('iso.i', 'iso.j', 'year', 'energy'))

# Link armed conflicts in t-1
df <- left_join(df, conflicts, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'lead1.year'))
df$lag1.minorconflict[is.na(df$lag1.minorconflict)] <- 0
df$lag1.war[is.na(df$lag1.war)] <- 0
isid('df', c('iso.i', 'iso.j', 'year', 'energy'))

# Get second-degree conflict and trade dispute in t-1
df <- left_join(df, embedded.disputes, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'year'))
df <- left_join(df, embedded.conflict, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'year'))
assert('!is.na(df$i) & !is.na(df$j)')
isid('df', c('iso.i', 'iso.j', 'year', 'energy'))

# Write file
saveRDS(df, file.path(output, "regdf.rds"))


