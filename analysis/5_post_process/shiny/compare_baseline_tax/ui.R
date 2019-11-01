##############################
# Interactive chord diagrams #
##############################
library('shiny')
library('ggplot2')
library('chorddiag')

source('config_post_process.R')

baseline <- readRDS('data/baseline.rds')
co2_tax <- readRDS('data/co2_tax.rds')

# Create Shiny app
shinyUI(
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
  ))
