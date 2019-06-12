###########################
# Seaports pre-processing #
###########################
rm(list = ls())
wd <- "H:/data/"
setwd(wd)

library('plyr')
library('dplyr')
library('magrittr')
library('stringr')

raw <-      paste0(wd, "raw")
output <-   paste0(wd, "output")
temp <-     paste0(wd, "temp")

#################
# Import files
major.ports <- read.csv(file.path(raw, "Ports/seaports_of_the_world.csv"), stringsAsFactors = F)

# Clean major seaports
names(major.ports) <- c("country", "port_name", "code", "latlong")

major.ports <- subset(major.ports, latlong != '' & 
                        !is.na(latlong) & grepl('Long', latlong) == F &
                        country != "Country")

long <- str_replace(major.ports$latlong, ".* ", "") 
lat <- str_replace(major.ports$latlong, " .*", "")

for (i in c('long', 'lat')) {
  assign('chi', get(i))
  
  chd <- as.numeric(str_replace(chi, "d.*", ""))
  chm <- str_replace(chi, ".*d", "") %>%
         str_replace("'[EWNS]", "") %>%
         as.numeric()
  
  chi.dec <- chd + (chm/60)
  chi.dec[grepl('[WS]', chi)] <- -1 * chi.dec[grepl('[WS]', chi)]
  
  assign(i, chi.dec)
}

major.ports$long <- long
major.ports$lat <- lat

major.ports <- major.ports[c('country', 'port_name', 'long', 'lat')]

# Write csv
write.csv(major.ports, file.path(temp, 'major_ports.csv'))