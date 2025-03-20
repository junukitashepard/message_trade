################################################################## 
# Regression analysis #
# Pre-process data: binary for whether trade occured (for logit) #
##################################################################
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

# Make binary dataset for whether or not trade occurs #
#######################################################
all.countries <- unique(c(unique(trade$iso.i), unique(trade$iso.j)))
energy.types <- c('PET', 'COAL', 'CRU', 'NG', 'BIO')

base <- expand.grid(all.countries, all.countries)
base$Var1 <- as.character(base$Var1)
base$Var2 <- as.character(base$Var2)
names(base) <- c('iso.i', 'iso.j')

base.y <- data.frame(iso.i = character(0), iso.j = character(0), year = numeric(0), energy = character(0))

for (e in energy.types) {
  for (y in 1995:2012) {
    assign('indf', base)
    indf$year <- y
    indf$energy <- e
    base.y <- rbind(base.y, indf)
  }
}
base.y <- subset(base.y, iso.i != iso.j)

trade.out <- left_join(base.y, trade, by = c('iso.i', 'iso.j', 'year', 'energy'))
trade.out$any_trade <- 0
trade.out$any_trade[trade.out$v > 0] <- 1
trade.out <- trade.out[c('iso.i', 'iso.j', 'year', 'energy', 'any_trade')]
isid('trade.out', c('iso.i', 'iso.j', 'year', 'energy'))

# Link other covariates
#######################
# Regions
trade.out <- left_join(trade.out, regional_spec, by = c('iso.i' = 'iso'))
trade.out <- left_join(trade.out, regional_spec, by = c('iso.j' = 'iso'))
names(trade.out)[(ncol(trade.out)-1):ncol(trade.out)] <- c('msg.region.i', 'msg.region.j')

# Distance
trade.out <- left_join(trade.out, ijports, by = c('iso.i' = 'port1.iso', 'iso.j' = 'port2.iso'))

# Trade disputes
trade.out <- left_join(trade.out, disputes, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'lead1.year'))
trade.out <- left_join(trade.out, embedded.disputes, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'year'))

# Conflicts
trade.out <- left_join(trade.out, conflicts, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'lead1.year'))
trade.out <- left_join(trade.out, embedded.conflict, by = c('iso.i' = 'i', 'iso.j' = 'j', 'year' = 'year'))

# Subset 
########
df <- subset(trade.out, !is.na(distance) & msg.region.i != "" & msg.region.j != "")
df$lag1.ds_any[is.na(df$lag1.ds_any)] <- df$lag1.ds_energy[is.na(df$lag1.ds_energy)] <- 0
  df$lag1.minorconflict[is.na(df$lag1.minorconflict)] <- df$lag1.war[is.na(df$lag1.war)] <- 0
  df$embed.lag1.ds_any[is.na(df$embed.lag1.ds_any)] <- df$embed.lag1.ds_energy[is.na(df$embed.lag1.ds_energy)] <- 0
  df$embed.lag1.minorconflict[is.na(df$embed.lag1.minorconflict)] <- df$embed.lag1.war[is.na(df$embed.lag1.war)] <- 0

df <- dplyr::group_by(df, iso.i, iso.j, energy) %>%
      dplyr::mutate(any_trade_ever = max(any_trade))
isid('df', c('iso.i', 'iso.j', 'year', 'energy'))

df.lag1 <- dplyr::group_by(df, iso.i, iso.j, year) %>%
           dplyr::summarise(any_trade = max(any_trade))
df.lag1$year <- df.lag1$year+1
names(df.lag1)[names(df.lag1) == 'any_trade'] <- 'lag1.trade'

df <- left_join(df, df.lag1, by = c('iso.i', 'iso.j', 'year'))
df$lag1.trade[is.na(df$lag1.trade)] <- 0

##
regdf <- subset(df, energy == 'PET')
regdf <- subset(regdf, any_trade_ever == 1)
m <- glm(any_trade ~ lag1.trade + log(distance) + lag1.ds_energy + lag1.ds_any +
          embed.lag1.ds_energy + embed.lag1.ds_any + embed.lag1.war + embed.lag1.minorconflict +
          factor(iso.i) + factor(iso.j) + factor(year), 
         data = regdf, family = 'binomial')

m.out <- exp(cbind(OR = exp(coef(m)), confint(m)))
