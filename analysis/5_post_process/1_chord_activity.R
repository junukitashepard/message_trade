##############################
# Interactive chord diagrams #
##############################
baseline <- read_MESSAGE(msg_scenario = 'baseline', msg_version = vsn_baseline, msg_variable = 'ACT') # Baseline (global schema)
baseline$vintage <- as.numeric(baseline$vintage)
baseline$year_all <- as.numeric(baseline$year_all)

co2_tax <- read_MESSAGE(msg_scenario = 'CO2_tax_baseline', msg_version = vsn_CO2_tax_baseline, msg_variable = 'ACT') # Baseline (global schema)
co2_tax$vintage <- as.numeric(co2_tax$vintage)
co2_tax$year_all <- as.numeric(co2_tax$year_all)

# Function: plot chord diagram
chord_activity <- function(indata, plot.energy, plot.year) {

  environment(isid) <- environment()

  assign('df', indata)

  df <- subset(df, as.numeric(year_all) > 2015)
  df <- subset(df, grepl('_exp_', tec))

  df$importer <- toupper(substr(df$tec, nchar(df$tec) - 2, nchar(df$tec)))
  df$exporter <- substr(df$node, 5, 7)
  df$energy <- sub('_exp_.*', '', df$tec)

  df <- subset(df, energy != 'gas') # exclude gas
  isid('df', c('exporter', 'importer', 'year_all', 'energy'))

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

# Create Shiny app
ui <-
  fluidPage(
    titlePanel('Comparison of baseline and CO2 tax scenarios'),
    br(),
    br(),
    radioButtons('energy', 'Energy Resource', inline = TRUE,
                 choices = c('All', 'Coal', 'Crude Oil', 'Ethanol', 'Fuel Oil',
                             'Light Oil', 'Liquid Hydrogen', 'LNG', 'Methanol'),
                 selected = 'All'),
    mainPanel("",
     fluidRow(column("Baseline", width = 6, chorddiagOutput('baselinePlot')),
              column("CO2 Tax", width = 6, chorddiagOutput('CO2taxPlot'))))
    #chorddiagOutput('baselinePlot', height = 600)
)

server <- function(input, output) {

  output$baselinePlot <- renderChorddiag({

    if (input$energy == 'All') {
      chord_activity(baseline, 'all', 2050)
    } else if (input$energy == 'Coal') {
      chord_activity(baseline, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      chord_activity(baseline, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      chord_activity(baseline, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      chord_activity(baseline, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      chord_activity(baseline, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      chord_activity(baseline, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      chord_activity(baseline, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      chord_activity(baseline, 'Methanol', 2050)
    }

  })

  output$CO2taxPlot <- renderChorddiag({

    if (input$energy == 'All') {
      chord_activity(co2_tax, 'all', 2050)
    } else if (input$energy == 'Coal') {
      chord_activity(co2_tax, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      chord_activity(co2_tax, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      chord_activity(co2_tax, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      chord_activity(co2_tax, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      chord_activity(co2_tax, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      chord_activity(co2_tax, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      chord_activity(co2_tax, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      chord_activity(co2_tax, 'Methanol', 2050)
    }
  })
}

shinyApp(ui = ui, server = server)
