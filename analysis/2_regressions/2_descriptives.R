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
library('maptools')
library('jsfunctions')

raw <-      paste0(wd, "raw")
input <-    paste0(wd, "output/derived")
output <-   paste0(wd, "output/analysis/regress")
temp <-     paste0(wd, "temp/")
##############################
# Import file
