#######################################################
# Interactive chord diagrams for sensitivity analysis #
#######################################################
co2_tax <- read_MESSAGE(msg_scenario = 'CO2_tax_baseline', msg_version = vsn_CO2_tax_baseline, msg_variable = 'ACT') # Baseline (global schema)
co2_tax$vintage <- as.numeric(co2_tax$vintage)
co2_tax$year_all <- as.numeric(co2_tax$year_all)

lowco2_tax <- read_MESSAGE(msg_scenario = 'low_CO2_tax_baseline', msg_version = vsn_CO2_tax_low, msg_variable = 'ACT') # Baseline (global schema)
lowco2_tax$vintage <- as.numeric(lowco2_tax$vintage)
lowco2_tax$year_all <- as.numeric(lowco2_tax$year_all)

highco2_tax <- read_MESSAGE(msg_scenario = 'high_CO2_tax_baseline', msg_version = vsn_CO2_tax_high, msg_variable = 'ACT') # Baseline (global schema)
highco2_tax$vintage <- as.numeric(highco2_tax$vintage)
highco2_tax$year_all <- as.numeric(highco2_tax$year_all)

# Create Shiny app
ui <-
  fluidPage(
    titlePanel('Sensitivity Analysis: CO2 tax rates'),
    br(),
    br(),
    radioButtons('energy', 'Trade Network', inline = TRUE,
                 choices = c('All', 'Coal', 'Crude Oil', 'Ethanol', 'Fuel Oil',
                             'Light Oil', 'Liquid Hydrogen', 'LNG', 'Methanol'),
                 selected = 'All'),

    fluidRow(column(div(style = "height:20px; font-size:16px;","Tax = $30/tCO2"),
                    width = 4, chorddiagOutput('co2tax')),
             column(div(style = "height:20px; font-size:16px;","Tax = $15/tCO2"),
                    width = 4, chorddiagOutput('lowco2tax')),
             column(div(style = "height:20px; font-size:16px;","Tax = $60/tCO2"),
                    width = 4, chorddiagOutput('highco2tax'))),
    fluidRow(column(div(style = "height:20px; font-size:14px;",
                        textOutput('co2tax_size')),
                    width = 4),
             column(div(stype = "height:20px; font-size:14px;",
                        textOutput('lowco2tax_size')),
                    width = 4),
             column(div(stype = "height:20px; font-size:14px;",
                        textOutput('highco2tax_size')),
                    width = 4))
  )

server <- function(input, output) {

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
}

shinyApp(ui = ui, server = server)
