###########
# Globals #
###########
rm(list = ls())
library('xlsx')

# Define working directory
gwd <- "H:/message_trade/"
setwd(gwd)

# Import user inputs
input.files <- read.xlsx(file.path(wd, "user_inputs/User_Inputs.xlsx"), 
                         sheetName = "Files", startRow = 4)
input.files[, 1:4] <- lapply(input.files[, 1:4], function(x) as.character(x))

input.values <- read.xlsx(file.path(wd, "user_inputs/User_Inputs.xlsx"),
                          sheetName = "Values", startRow = 4)
input.values[, 1:2] <- lapply(input.values[, 1:2], function(x) as.character(x))

# Assign global values
node_interval <- input.values$Value[1]

# Assign global paths to files
assign_path <- function(name) {
  assign('p', subset(input.files, Name == name))
  if (is.na(p$Path)) {
    return(p$DefaultPath[1])
  } else {
    return(p$DefaultPath[1])
  }
}

major_ports.path <- assign_path('major_ports')
country_correction_ports.path <- assign_path('country_correction_ports')
regional_specification.path <- assign_path('regional_specification')