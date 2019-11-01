#######################################################
# Interactive chord diagrams for sensitivity analysis #
#######################################################
co2_tax <- readRDS('data/co2_tax.rds')
lowco2_tax <- readRDS('data/lowco2_tax.rds')
highco2_tax <- readRDS('data/highco2_tax.rds')

source('config_post_process.R')

# Create Shiny app
shinyServer(function(input, output) {

  # Network sizes
  output$co2tax_size <- renderText({
    if (input$energy == 'All') {
      network_size(co2_tax, 'all', 2050)
    } else if (input$energy == 'Coal') {
      network_size(co2_tax, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      network_size(co2_tax, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      network_size(co2_tax, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      network_size(co2_tax, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      network_size(co2_tax, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      network_size(co2_tax, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      network_size(co2_tax, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      network_size(co2_tax, 'Methanol', 2050)
    }
  })

  output$lowco2tax_size <- renderText({
    if (input$energy == 'All') {
      network_size(lowco2_tax, 'all', 2050)
    } else if (input$energy == 'Coal') {
      network_size(lowco2_tax, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      network_size(lowco2_tax, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      network_size(lowco2_tax, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      network_size(lowco2_tax, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      network_size(lowco2_tax, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      network_size(lowco2_tax, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      network_size(lowco2_tax, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      network_size(lowco2_tax, 'Methanol', 2050)
    }
  })

  output$highco2tax_size <- renderText({
    if (input$energy == 'All') {
      network_size(highco2_tax, 'all', 2050)
    } else if (input$energy == 'Coal') {
      network_size(highco2_tax, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      network_size(highco2_tax, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      network_size(highco2_tax, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      network_size(highco2_tax, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      network_size(highco2_tax, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      network_size(highco2_tax, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      network_size(highco2_tax, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      network_size(highco2_tax, 'Methanol', 2050)
    }
  })

  # Chord diagrams
  output$co2tax <- renderChorddiag({

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

  output$lowco2tax <- renderChorddiag({

    if (input$energy == 'All') {
      chord_activity(lowco2_tax, 'all', 2050)
    } else if (input$energy == 'Coal') {
      chord_activity(lowco2_tax, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      chord_activity(lowco2_tax, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      chord_activity(lowco2_tax, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      chord_activity(lowco2_tax, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      chord_activity(lowco2_tax, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      chord_activity(lowco2_tax, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      chord_activity(lowco2_tax, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      chord_activity(lowco2_tax, 'Methanol', 2050)
    }
  })

  output$highco2tax <- renderChorddiag({

    if (input$energy == 'All') {
      chord_activity(highco2_tax, 'all', 2050)
    } else if (input$energy == 'Coal') {
      chord_activity(highco2_tax, 'coal', 2050)
    } else if (input$energy == 'Crude Oil') {
      chord_activity(highco2_tax, 'oil', 2050)
    } else if (input$energy == 'Ethanol') {
      chord_activity(highco2_tax, 'eth', 2050)
    } else if (input$energy == 'Fuel Oil') {
      chord_activity(highco2_tax, 'foil', 2050)
    } else if (input$energy == 'Light Oil') {
      chord_activity(highco2_tax, 'loil', 2050)
    } else if (input$energy == 'Liquid Hydrogen') {
      chord_activity(highco2_tax, 'lh2', 2050)
    } else if (input$energy == 'LNG') {
      chord_activity(highco2_tax, 'LNG', 2050)
    } else if (input$energy == 'Methanol') {
      chord_activity(highco2_tax, 'Methanol', 2050)
    }
  })
})

