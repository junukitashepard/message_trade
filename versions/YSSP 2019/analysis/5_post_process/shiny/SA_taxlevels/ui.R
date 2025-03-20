#######################################################
# Interactive chord diagrams for sensitivity analysis #
#######################################################
co2_tax <- readRDS('data/co2_tax.rds')
lowco2_tax <- readRDS('data/lowco2_tax.rds')
highco2_tax <- readRDS('data/highco2_tax.rds')

source('config_post_process.R')

# Create Shiny app
shinyUI(
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
  ))
