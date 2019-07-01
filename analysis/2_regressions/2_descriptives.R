####################### 
# Regression analysis #
# Descriptives        #
#######################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('ggplot2')
library('jsfunctions')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output/analysis/regress")
output <-   paste0(wd, "output/analysis/regress")
temp <-     paste0(wd, "temp/")
##############################
energy.types <- c('PET', 'COAL', 'CRU', 'NG', 'BIO')

# Import file
regdf <- readRDS(file.path(input, 'regdf.rds'))
regdf <- subset(regdf, energy %in% energy.types)
regdf <- subset(regdf, q_e > 0.1) # Keep only if trade is > 0.1TJ
regdf <- subset(regdf, var_cost < 100) # Keep only if variable cost < 100 ($1000/GWa)
regdf$var_cost = regdf$var_cost * 1000 # in $/GWa

# Histograms of energy cost ($1000/GWa) by energy type
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

regdf <- dplyr::group_by(regdf, j, year, energy) %>%
         dplyr::mutate(e_demand = sum(q_e))

regdf <- subset(regdf, energy == 'PET' & msg.region.i == 'MEA' & msg.region.j == 'WEU')
m <- lm(var_cost ~ distance + lag1.ds_energy + 
          embed.lag1.ds_energy + embed.lag1.ds_any + embed.lag1.war + embed.lag1.minorconflict +
          e_demand + q_e + factor(i) + factor(j) + factor(year), data = regdf)
summary(m)
