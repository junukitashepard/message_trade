############################ 
# Functions for regression #
############################
# Simple regression function
ols_regress <- function(variable, export.region = "all", import.region = "all", energy.type = "all",
                        full.summary = FALSE) {
  
  assign('regdf', trade)
  
  assign('form', "var_cost ~ 
                  distance + 
                  gdp.i + gdp.j + pop.i + pop.j + 
                  contiguity + common_language + i_colony_of_j + j_colony_of_i + 
                  sanction_imposition +  
                  factor(iso.i) + factor(iso.j) + factor(year)")
  
  if (export.region != "all") {regdf <- subset(regdf, msg.region.i %in% export.region)}
  if (import.region != "all") {regdf <- subset(regdf, msg.region.j %in% import.region)}
  if (energy.type != "all") {regdf <- subset(regdf, energy %in% energy.type)}
  
  if (energy.type == "all") {
    form <- paste0(form, " + factor(energy)")
  }
  
  regdf <- subset(regdf, !is.na(var_cost) & !is.infinite(var_cost) & !is.nan(var_cost)) # Keep only if non-missing Y variable
  regdf <- subset(regdf, var_cost < 50)
  regdf$distance <- regdf$distance/(1000) # in thousand km
  
  assign('m', lm(as.formula(form), data = regdf))
  assign('coef', summary(m)$coefficients[variable,])
  
  # Add mean(Y)
  assign('meany', median(regdf$var_cost))
  assign('ar2', summary(m)$adj.r.squared)
  
  coef <- c(coef, meany, ar2)
  
  if (full.summary == TRUE) {
    return(summary(m))
  } else {
    return(coef)
  }
  
}

# Function: run for all regions and by region
run_reg <- function(variable = variable, energy_list = energy_list, exporters = "all", importers = "all") {
  
  assign('tab.names', c('base'))
  
  assign('model_base', ols_regress(variable = variable, export.region = exporters, import.region = importers))
  
  for (e in energy_list) {
    assign('d.coef', ols_regress(variable = variable, export.region = exporters, import.region = importers, energy.type = e))
    model_base <- rbind(model_base, d.coef)
    tab.names <- c(tab.names, e)
  }
  rownames(model_base) <- tab.names
  return(model_base)
}
