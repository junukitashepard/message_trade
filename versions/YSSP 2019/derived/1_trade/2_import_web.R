#########################################################
# Import WEB datasets and keep only imports and exports #
#########################################################
# Import from SQL database
for (c in c('1990_2008', '2009_2016')) {
  sql_statement <- paste0("SELECT * FROM WEB.WEB_", c, " ",
                          "WHERE (FLOW = 'Exports' OR FLOW = 'Imports') AND UNIT = 'TJ'")
  import_sql(statement = sql_statement,
             user = 'root',
             password = 'SEAmonst3r!',
             dbname = 'BACI_TRADE',
             outdf = paste0('web.', c))
}

# Append files
web.1990_2008 <- web.1990_2008[c('IEA_UNIT', 'IEA_COUNTRY', 'Product', 'Flow', 'Time', 'Value')]
web.2009_2016 <- web.2009_2016[c('IEA_UNIT', 'IEA_COUNTRY', 'Product', 'Flow', 'Time', 'Value')]
web <- unique(rbind(web.1990_2008, web.2009_2016))
isid('web', c('IEA_COUNTRY', 'Product', 'Time', 'Flow'))

# Clean up string (Product)
web$Product <- stringr::str_replace_all(web$Product, '\"', '')

# Update energy definitions
BIO <- c('Biofuels and waste', 'Peat and peat products')
COAL <- c('Coal and coal products')
CRU <- c('Crude, NGL and feedstocks', 'Oil shale and oil sands')
PET <- c('Oil products')
NUC <- c('Nuclear')
NG <- c('Natural gas')

web$energy <- NA
for (c in c('BIO', 'COAL', 'CRU', 'PET', 'NUC', 'NG')) {
  assign('elist', get(c))
  web$energy[web$Product %in% elist] <- c
}

web <- subset(web, !is.na(energy))

names(web) <- c('unit', 'iso.country', 'product', 'flow', 'year', 'value', 'energy')

# Collapse by energy type
web <- dplyr::group_by(web, iso.country, flow, year, energy) %>%
       dplyr::summarise(value = sum(value, na.rm = T))

assert('web$value[web$flow == "Exports"] <= 0')
web$value[web$flow == "Exports"] <- -1*web$value[web$flow == "Exports"]

# Save file
saveRDS(web, file.path(temp, 'iea_web_trade.rds'))
