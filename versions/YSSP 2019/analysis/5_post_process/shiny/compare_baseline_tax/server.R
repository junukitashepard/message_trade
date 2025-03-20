##############################
# Interactive chord diagrams #
##############################
library('shiny')
library('ggplot2')
library('chorddiag')

source('config_post_process.R')

baseline <- readRDS('data/baseline.rds')
co2_tax <- readRDS('data/co2_tax.rds')

shinyServer(function(input, output) {

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
})
