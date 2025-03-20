#####################################
# Run regression and compile tables #
#####################################
# Import file
trade <- readRDS(file.path(input, "analysis/regress/regdf.rds"))
trade <- subset(trade, var_cost < 1000) # less than $1b/GWa

# Function to compile tables
compile_tables <- function (variable.in) {
  
  wb <- loadWorkbook(file.path(output, 'analysis/regress/tables/base_regression.xlsx'))

  # All regions
  all_regions <- run_reg(variable.rr = variable.in)
  all_regions <- as.data.frame(all_regions)

  writeData(wb, sheet = "All regions", all_regions, 
            startRow = 3, startCol = 2,
            colNames = FALSE, rowNames = FALSE)

  # By importing region
  i <- 3
  for (r in region.list.trade) {
    print(paste0("Running regression for importer = ", r))
    assign(paste0('M_mat'), run_reg(variable.rr = variable.in, importers = r))
    M_mat <- as.data.frame(M_mat)
    writeData(wb, sheet = "Importing region", M_mat,
              startRow = i, startCol = 2,
              colNames = FALSE, rowNames = FALSE)
    i <- i + 8
  }

  # By exporting region
  i <- 3
  for (r in region.list.trade) {
    print(paste0("Running regression for exporter = ", r))
    assign(paste0('X_mat'), run_reg(variable.rr = variable.in, exporters = r))
    X_mat <- as.data.frame(X_mat)
    writeData(wb, sheet = "Exporting region", X_mat,
              startRow = i, startCol = 2,
              colNames = FALSE, rowNames = FALSE)
    i <- i + 8
  }
  
  # Save compiled table
  saveWorkbook(wb, file.path(output, paste0('analysis/regress/tables/', variable.in, '_regressions.xlsx')), overwrite = T)
}

# Run program
compile_tables(variable.in = 'distance')