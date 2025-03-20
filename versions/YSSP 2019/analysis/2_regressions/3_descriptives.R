####################### 
# Regression analysis #
# Descriptives        #
#######################
# Import file
regdf <- readRDS(file.path(input, 'analysis/regress/regdf.rds'))
regdf <- subset(regdf, energy %in% energy.types)
regdf <- subset(regdf, q_e > 0.1) # Keep only if trade is > 0.1TJ
regdf <- subset(regdf, var_cost < 1000) # var_cost less than $b/GWa

# Histograms of energy cost ($m/GWa) by energy type
makehist <- function(e) {
  plotdf <- subset(regdf, energy == e)
  plot <- ggplot(data = plotdf, 
                 aes(plotdf$var_cost)) +
          geom_histogram(fill = 'blue',
                         color = 'darkblue',
                         alpha = 0.3) +
          labs(x = "Trade Cost ($/GWa)",
               y = "Frequency",
               title = e) + 
          theme(text = element_text(size = 18))
  return(plot)  
}

for (e in energy.types) {
  assign(paste0('hist.', e), makehist(e), envir = parent.frame())
}

