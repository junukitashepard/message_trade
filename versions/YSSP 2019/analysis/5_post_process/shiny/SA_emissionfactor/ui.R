##############################
# Interactive chord diagrams #
##############################
co2_tax <- readRDS('data/co2_tax.rds')
low_ef <- readRDS('data/low_ef2.rds')
baseline <- readRDS('data/baseline.rds')

source('config_post_process.R')

# Create Shiny app
shinyUI(
  fluidPage(
    titlePanel('Sensitivity Analysis: MEA emission factor'),
    br(),
    br(),
    radioButtons('energy', 'Energy Resource', inline = TRUE,
                 choices = c('All', 'Coal', 'Crude Oil', 'Ethanol', 'Fuel Oil',
                             'Light Oil', 'Liquid Hydrogen', 'LNG', 'Methanol'),
                 selected = 'All'),
    mainPanel("",
              fluidRow(column("Baseline", width = 4, chorddiagOutput('baselinePlot')),
                       column("CO2 Tax", width = 4, chorddiagOutput('CO2taxPlot')),
                       column("CO2 Tax: Low MEA emission factor", width = 4, chorddiagOutput('lowEFPlot'))))
    #chorddiagOutput('baselinePlot', height = 600)
  ))
