############################ 
# Functions for regression #
############################
# Simple regression function
ols_regress <- function(variable, export.region = "all", import.region = "all", energy.type = "all",
                        full.summary = FALSE) {
  
  assign('regdf', trade)

  if (export.region != "all") {regdf <- subset(regdf, msg.region.i %in% export.region)}
  if (import.region != "all") {regdf <- subset(regdf, msg.region.j %in% import.region)}
  if (energy.type != "all") {regdf <- subset(regdf, energy %in% energy.type)}
  
  regdf <- subset(regdf, !is.na(var_cost) & !is.infinite(var_cost) & !is.nan(var_cost)) # Keep only if non-missing Y variable
  regdf <- subset(regdf, var_cost < 1000)
  regdf$distance <- regdf$distance/(1000) # in thousand km
  
  # Assign formula
  assign('form', paste0("var_cost ~ ", variable, " + 
         gdp.i + gdp.j + pop.i + pop.j + 
         contiguity + common_language +
         factor(iso.j) + factor(year) + "))
  
  # Add energy FE if all energy types included 
  if (energy.type == "all") {
    form <- paste0(form, " + factor(energy)")
  }
  
  # Add exporter FE if MESSAGE region 1 (exporter) is not RUS (which only includes one level, Russia)
  if (export.region != 'RUS') {form <- paste0(form, ' + factor(iso.i)')}
  
  # Add gravity terms if factor levels > 0
  for (q in c('contiguity', 'common_language', 'i_colony_of_j', 'j_colony_of_i')) {
    assign('minv', min(regdf[q], na.rm = T))
    assign('maxv', max(regdf[q], na.rm = T))
    if (minv != maxv) {form <- paste0(form, ' + ', q)}
  }
  
  assign('m', lm(as.formula(form), data = regdf))
  
  if(!(variable %in% rownames(summary(m)$coefficients))) {
    assign('coef', rep(NA, 4))
    names(coef) <- c('Estimate', 'Std. Error', 't value', 'Pr(>|t|)')
  } else {
    assign('coef', summary(m)$coefficients[variable,])
  }
  
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
run_reg <- function(variable.rr, energy_list.rr = energy_list, exporters = "all", importers = "all") {
  
  assign('tab.names', c('base'))
  
  assign('model_base', ols_regress(variable.rr, export.region = exporters, import.region = importers))
  
  for (e in energy_list.rr) {
    assign('d.coef', ols_regress(variable.rr, export.region = exporters, import.region = importers, energy.type = e))
    model_base <- rbind(model_base, d.coef)
    tab.names <- c(tab.names, e)
  }
  rownames(model_base) <- tab.names
  return(model_base)
}
