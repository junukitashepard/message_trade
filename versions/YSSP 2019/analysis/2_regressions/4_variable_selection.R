##################################
# Variable selection using LASSO #
##################################
rm(list = ls())
wd.data <- "H:/data/"
wd <- 'H:/message_trade/analysis/2_regressions/'
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('jsfunctions')
library('ggplot2')
library('glmnet')

raw <-      paste0(wd.data, "raw")
input <-    paste0(wd.data, "output/analysis/regress/")
output <-   paste0(wd.data, "output/analysis/regress/")
temp <-     paste0(wd.data, "temp/")

source(paste0(wd, '4_regress.R'))
#######################################################
# Import file
trade <- readRDS(file.path(input, "regdf.rds"))
trade$i[trade$i == '(Other)'] <- NA
trade$j[trade$j == '(Other)'] <- NA

# Convert to factor to create dummies
trade$i <- as.factor(trade$i)
trade$j <- as.factor(trade$j)
trade$year <- as.factor(trade$year)
trade$energy <- as.factor(trade$energy)

# Make full dataset
trade <- subset(trade, !is.na(distance)) # Drops landlocked countries

for (v in c('v', 'q', 'q_e', 'iso.i', 'iso.j', 'msg.region.i', 'msg.region.j',
            'port1', 'port2', 'embed.lag1.ds_any', 'embed.lag1.ds_energy',
            'lag1.q_e_gwa', 'lag1.agree_cu')) {
  trade[v] <- NULL
}

for (v in names(trade)) {
  names(trade)[names(trade) == v] <- 'var'
  df <- subset(trade, is.na(var) | is.infinite(var))
  print(paste0('Dropping ', v, ' will drop ', nrow(df), ' observations'))
  trade <- subset(trade, !is.na(var) & !is.infinite(var))
  names(trade)[names(trade) == 'var'] <- v
  rm(df)
}

# Standardize variables
for (v in names(trade)) {
  print(paste0('Standardizing ', v))
  names(trade)[names(trade) == v] <- 'var'
  if (is.numeric(trade$var)) {
    if (min(trade$var) != 0 & max(trade$var) != 1 & any(!is.na(trade$var))) {
      trade$var <- as.data.frame(scale(trade$var))[,1]
    }
  }
  names(trade)[names(trade) == 'var'] <- v
}

# Run LASSO #
#############
run_lasso <- function(energy) {
  
  df <- subset(trade, energy == energy)
  df$energy <- NULL
  df$q_e_gwa_p2 <- df$q_e_gwa_p3 <- NULL

  x <- model.matrix(q_e_gwa~., df)[,-1]
  y <- df$q_e_gwa

  lasso_object <- glmnet(x, y)
  assign(paste0('plot_coef.', energy),
         plot(lasso_object),
         envir = parent.frame())

  cv_output <- cv.glmnet(x, y, alpha = 1)
  assign(paste0('plot_cv.', energy),
         plot(cv_output),
         envir = parent.frame())

  assign(paste0('coef.', energy),
         coef(cv_output, lambda = cv_output$lambda.min),
         envir = parent.frame())
}

run_lasso('coal')
run_lasso('oil')
