#############################################
# Visualize regression results for distance #
#############################################
rm(list = ls())
wd.data <- "H:/data/"
wd <- 'H:/message_trade/analysis/2_regressions/'
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('maptools')
library('jsfunctions')
library('ggplot2')

raw <-      paste0(wd.data, "raw")
input <-    paste0(wd.data, "output/analysis/regress/")
output <-   paste0(wd.data, "output/analysis/regress/tables")
temp <-     paste0(wd.data, "temp/")

source(paste0(wd, '4_regress.R'))

##############################
# Import file
trade <- readRDS(file.path(input, "regdf.rds"))

energy_list <- c('oil', 'coal', 'foil', 'LNG')
region_list <- c('AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'SAS', 'WEU')

# Distribution of variable cost
trade <- subset(trade, var_cost < 1000) # Less than $1b/GWa
ggplot(aes(x = var_cost), data = trade) + 
  geom_histogram(fill = 'blue', colour = 'darkblue', alpha = 0.3) + 
  labs(x = 'Trade cost ($/GWa)', y = 'Count', title = 'Distribution of trade costs ($/GWa)') + 
  theme(text = element_text(size = 20))

# Plot regression heterogeneity by region
plot_heterog <- function(in.variable, y_axis_label) {
  
  # All regions
  df <- run_reg(variable.rr = in.variable)
  df <- as.data.frame(df)
  df$region <- 'ALL'
  df$model <- rownames(df)

  # By region
  for (r in region_list) {
    print(paste0("Running regression for importer = ", r))
    assign(paste0('mat'), run_reg(variable.rr = in.variable, exporters = r))
    mat <- as.data.frame(mat)
    mat$region <- r
    mat$model <- rownames(mat)
    df <- rbind(df, mat)
  }

  # Compile plot data
  names(df) <- c('coef', 'se', 't', 'p', 'meany', 'ar2', 'region', 'model')
  df$model <- toupper(df$model)
  
  df$coef_min <- df$coef-(df$se*1.96)
  df$coef_max <- df$coef+(df$se*1.96)

  # Plot
  df$model[df$model == 'BASE'] <- '.ALL ENERGY'
  df$region[df$region == 'ALL'] <- '.ALL'
  df$region_factor <- as.factor(df$region)

  plot <- 
  ggplot(aes(x = region_factor, y = coef, fill = region_factor), data = df) +
    geom_bar(stat = 'identity') + 
    geom_errorbar(aes(ymin = coef_min, ymax = coef_max), width = 0.2, position = position_dodge(0.2)) +
    facet_wrap(~model, nrow = 1) + 
    theme(legend.position = 'bottom', text = element_text(size = 24), axis.text.x = element_text(angle = 90)) + 
    labs(x = '', y = y_axis_label, fill = 'Exporting Region') + 
    scale_fill_brewer(palette = 'Paired')
  
  return(plot)
}

# Run programs
distance_plot <- plot_heterog('distance', 'Effect of 1000km increase on trade cost ($M/GWa)')
sanction_plot <- plot_heterog('sanction_imposition', 'Effect of additional sanction on trade cost ($M/GWa')

