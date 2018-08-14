# processing data 

library(tidyverse)
library(sf)
library(lubridate)

# epsg code
prj <- 26911

######
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
    occol = ifelse(occurrence == 'present', 'lightgreen', 'tomato1'),
    occex = ifelse(occurrence == 'present', 7, 3)
  )
save(biodat, file = 'data/biodat.RData', compress = 'xz')

######
# reaches in LA/Ventura counties, RB4
rchdat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Data/RawData/Environmental Data/Rivers/RB4_Rivers.shp') %>% 
  st_simplify(dTolerance = 30, preserveTopology = T) %>% 
  st_transform(prj)

save(rchdat, file = 'data/rchdat.RData', compress = 'xz')

######
# pour points and watersheds with modelled historic flow

# reaches in LA/Ventura counties, RB4
prsdat <- st_read('ignore/PP_20180405and0414.shp') %>% 
  st_transform(prj) %>% 
  rename(unq_id = unq_ID) %>% 
  mutate(unq_id = as.character(unq_id))

save(prsdat, file = 'data/prsdat.RData', compress = 'xz')

# catchment data
shddat1 <- st_read('ignore/Catchments_Merge_20180405.shp') %>% 
  st_transform(prj) %>% 
  select(unq_ID) %>% 
  rename(unq_id = unq_ID)
shddat2 <- st_read('ignore/ws_20180414.shp') %>% 
  st_transform(prj) 
shddat <- rbind(shddat1, shddat2)

save(shddat, file = 'data/shddat.RData', compress = 'xz')

#######
# modelled historic flow data for each pour point in prsdat

flodat <- read.csv('ignore/From Kelly_MERGE_DailyAverages_ws001to048.csv') %>% 
  rename(date = X) %>% 
  mutate(
    date = as.character(date),
    date = as.Date(date, format = '%m/%d/%Y %H:%M')
    ) %>% 
  gather('unq_id', 'flo', -date) 

save(flodat, file = 'data/flodat.RData', compress = 'xz')
