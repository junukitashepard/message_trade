###############################
# Post process MESSAGE output #
###############################
rm(list = ls())
wd <- "H:/data/"
repo <- "H:/message_trade/"
msg_dir <- "C:/ProgramData/Anaconda3/Lib/site-packages/message_ix/model/output"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('readxl')
library('gdxrrw') # install from github (lolow/gdxtools)

library('RColorBrewer')
ncolors <- 14
mycolors <- colorRampPalette(brewer.pal(8, 'Paired'))(ncolors)

# Import file using gdxrrw
read_MESSAGE <- function(msg_version, msg_variable) {
  
  assign('var_out', rgdx(file.path(msg_dir, paste0('MsgOutput_MESSAGEix_TRADE__trade_parameters__v', msg_version, '.gdx')),
                      list(name = msg_variable)))
  var_out <- rgdx.var(var_out)
  
  i <- sapply(var_out, is.factor)
  var_out[i] <- lapply(var_out[i], as.character)
  
  return(var_out)
}

activity <- read_MESSAGE(msg_version = 70, msg_variable = 'ACT')

assign('activity', read_xlsx(file.path(input, 'activity.xlsx'), sheet = 1))
names(activity) <- c('node', 'technology', 'year_act', 'year_vtg', 'mode', 'time', 'level', 'marginal', 'lower')

activity <- subset(activity, grepl('oil_exp_', technology))
activity <- subset(activity, as.numeric(year_act) > 2015)

activity$level <- as.numeric(activity$level)
activity$year_act <- as.numeric(activity$year_act)

activity$importer <- toupper(substr(activity$technology, nchar(activity$technology) - 2, nchar(activity$technology)))
activity$exporter <- substr(activity$node, 5, 7)

activity$level[is.na(activity$level)] <- 0

# Plot
assign('regions', unique(activity$importer))

for (r in regions) {
  assign (paste0('plot.', r), 
          ggplot(aes(x = year_act, y = level, fill = exporter), 
                 data = subset(activity, importer == r)) + 
            geom_bar(stat = 'identity') + 
            labs(x = 'Year', y = paste0('Oil', ' imports (GWa)'), fill = 'Exporting region', title = paste0('Region: ', r)) +
            theme(legend.position = 'bottom') + 
            scale_fill_manual(values = mycolors),
          envir = parent.frame())
}

ggpubr::ggarrange(plot.AFR, plot.CAS, plot.CPA, plot.EEU, nrow = 2, ncol = 2)
ggpubr::ggarrange(plot.LAM, plot.MEA, plot.NAM, plot.PAO, nrow = 2, ncol = 2)
ggpubr::ggarrange(plot.PAS, plot.RUS, plot.SAS, plot.WEU, nrow = 2, ncol = 2)


