#############################
# Post-process model output #
#############################
#rm(list = ls())
repo <- "H:/message_trade/"
wd <- "H:/data/"

library('shiny')
library('tidyr')
library('plyr')
library('magrittr')
library('ggplot2')
library('readxl')
#library('gdxrrw') # install from github (lolow/gdxtools)
library('circlize')
library('RColorBrewer')
library('dplyr')
library('chorddiag') # install from github (mattflor/chorddiag)
library('gridExtra')
library('htmltools')
library('plotly')

msg_dir <- "data/"

#setwd(repo)
#config <- reticulate::import('config')

# Set colors
color_regions <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3',
                   '#ff7f00', '#ffff33', '#a65628', '#f781bf',
                   '#999999', '#fb9a99', '#cab2d6')
names(color_regions) <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM',
                          'PAO', 'PAS', 'RUS', 'SAS', 'WEU')

# MESSAGE versions
vsn_baseline <- 5
vsn_tariff_low <- 3
vsn_tariff_high <- 3
vsn_CO2_tax_baseline <- 2
vsn_CO2_tax_low <- 1
vsn_CO2_tax_high <- 1
vsn_low_ef_baseline <- 1

# Energy commodities modeled
energy.types <- c('coal', 'foil', 'loil', 'LNG', 'oil', 'meth', 'eth', 'lh2')

# Conversion factor
gwa_to_ej <- 8760*3600*(10^-9)

# Read MESSAGE gdx
read_MESSAGE <- function(msg_scenario, msg_version, msg_variable, model = 'MESSAGE_TRADE') {

  assign('var_out', rgdx(file.path(msg_dir, paste0('MsgOutput_', model, '__', msg_scenario, '__v', msg_version, '.gdx')),
                         list(name = msg_variable)))
  var_out <- rgdx.var(var_out)

  i <- sapply(var_out, is.factor)
  var_out[i] <- lapply(var_out[i], as.character)

  return(var_out)
}

# Check unique identifiers
isid <- function(data, variables, not_id = FALSE) {
  assign('d', get(data))
  u <- unique(d[variables])
  id <- eval(nrow(d) == nrow(u))

  varlist <- ""
  for (v in variables) {
    varlist <- paste0(varlist, " ", v)
  }
  if (id == TRUE & not_id == FALSE) {
    print(paste0("Dataframe ", data, " is uniquely ID'd by:"))
    print(paste0("    ", varlist))
  } else if (id == TRUE & not_id == TRUE){
    stop(paste0("Dataframe ", data, " is uniquely ID'd! (And you don't want it to be)"))
  } else if (id == FALSE & not_id == FALSE) {
    stop(paste0("Dataframe ", data, " is NOT uniquely ID'd! (And you want it to be)"))
  } else if (id == FALSE & not_id == TRUE) {
    print(paste0("Dataframe ", data, " is not uniquely ID'd, and this is what you want"))
  }
}

# Written by user Renger on https://forum.gamsworld.org/viewtopic.php?t=9966
rgdx.var <- function(varname) {
  var.data <- data.frame(varname$val)
  var.dim <- length(varname$uels)
  domains <- varname$domains
  for (j in (1:(var.dim))) {
    if (domains[j] == "*") {
      domains[j] <- paste("X", j, sep = "")
    }
  }
  for (i in 1:var.dim) {
    dim           <- data.frame(varname$uels[[i]])
    dim$id        <- seq_along(dim[, 1])
    index         <- varname$domains[i]
    colnames(dim) <- c(index, "id")
    var.colname   <- paste("X", i, sep = "")
    var.data      <- merge(dim, var.data, by.x = "id", by.y = var.colname)
    var.data      <- var.data[, -which(colnames(var.data) == "id")]
  }
  var.data <- var.data[, c(var.dim:1, var.dim + 1)]
  colnames(var.data)[var.dim + 1]  <- c("value")
  colnames(var.data)[var.dim]      <- "field"
  attributes(var.data)$domains     <- varname$domains
  attributes(var.data)$type        <- "variable"
  attributes(var.data)$symName     <- varname$name
  attributes(var.data)$description <- varname$description
  return(var.data)
}

# Function: plot chord diagram
chord_activity <- function(indata, plot.energy, plot.year) {

  assign('df', indata)

  df <- subset(df, as.numeric(year_all) > 2015)
  df <- subset(df, grepl('_exp_', tec))

  df$importer <- toupper(substr(df$tec, nchar(df$tec) - 2, nchar(df$tec)))
  df$exporter <- substr(df$node, 5, 7)
  df$energy <- sub('_exp_.*', '', df$tec)

  df <- subset(df, energy != 'gas') # exclude gas

  plotdf <- subset(df, year_all == plot.year)[c('exporter', 'importer', 'energy', 'value')]
  plotdf <- subset(plotdf, !(exporter %in% c('GLB', 'CAS', 'SCS')) &
                     !(importer %in% c('GLB', 'CAS', 'SCS')))

  plotdf$value <- plotdf$value *  (8760*3600*(10^-9)) # convert to EJ

  # Subset based on energy
  if (plot.energy == 'all') {
    plotdf <- group_by(plotdf, exporter, importer) %>% summarize(value = sum(value, na.rm = T))
  } else {
    plotdf <- subset(plotdf, energy == plot.energy)
    if(nrow(plotdf) == 0) {
      stop('No trade network detected')
    }
    plotdf$energy <- NULL
  }

  # Reshape as matrix
  plotmat <- expand.grid(unique(c(plotdf$exporter, plotdf$importer)), unique(c(plotdf$exporter, plotdf$importer)))
  names(plotmat) <- c('exporter', 'importer')
  plotmat <- left_join(plotmat, plotdf, by = c('exporter', 'importer'))
  plotmat <- spread(plotmat, importer, value)
  rnames <- plotmat$exporter
  plotmat <- as.matrix(plotmat)
  rownames(plotmat) <- rnames
  plotmat <- plotmat[, 2:ncol(plotmat)]
  plotmat[is.na(plotmat)] <- 0
  class(plotmat) <- 'numeric'
  plotmat[] <- round(plotmat[], 3)

  chorddiag(plotmat, type = 'directional', palette = 'Paired',
            showTicks = F, groupnameFontsize = 16, groupnamePadding = 10, margin = 90,
            tooltipFontsize = 14,
            width = 1200)

}

# Function get trade network size
network_size <- function(indata, plot.energy, plot.year) {

  environment(isid) <- environment()

  assign('df', indata)

  df <- subset(df, grepl('_exp_', tec))

  df$importer <- toupper(substr(df$tec, nchar(df$tec) - 2, nchar(df$tec)))
  df$exporter <- substr(df$node, 5, 7)
  df$energy <- sub('_exp_.*', '', df$tec)

  df <- subset(df, energy != 'gas') # exclude gas

  plotdf <- subset(df, year_all == plot.year)[c('exporter', 'importer', 'energy', 'value')]
  plotdf <- subset(plotdf, !(exporter %in% c('GLB', 'CAS', 'SCS')) &
                     !(importer %in% c('GLB', 'CAS', 'SCS')))

  plotdf$value <- plotdf$value *  (8760*3600*(10^-9)) # convert to EJ

  if (plot.energy == 'all') {
    plotdf <- plotdf
  } else {
    plotdf <- subset(plotdf, energy == plot.energy)
  }

  return(paste0("Network size: ", round(sum(plotdf$value), 3), " EJ"))
}
