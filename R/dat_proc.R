# processing data 

library(tidyverse)
library(sf)
library(lubridate)

# biological data linked to nhd
biodat <- st_read('//172.16.1.5/Data/JennyTaylor/Data Visualization/Species_NHD.shp')
biodat <- biodat %>% 
  dplyr::select(FID_1, date, name, occurrence, source) %>% 
  mutate(
    date = ifelse(date == as.Date('2028-07-07'), as.Date('2018-07-07'), date),
    date = as.Date(date, origin = "1970-01-01"),
    yr = year(date), 
    mo = month(date),
    occurrence = factor(occurrence, levels = c(0, 1), labels = c('absent', 'present')),
    occol = ifelse(occurrence == 'present', 'lightgreen', 'tom ato1'),
    occex = ifelse(occurrence == 'present', 7, 3)
  )
save(biodat, file = 'data/biodat.RData', compress = 'xz')

# reaches in LA/Ventura counties, RB4
rchdat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Data/RawData/Environmental Data/Rivers/RB4_Rivers.shp') %>% 
  st_simplify(dTolerance = 30, preserveTopology = T)

save(rchdat, file = 'data/rchdat.RData', compress = 'xz')

