# processing data 

library(tidyverse)
library(sf)
library(lubridate)
library(proj4shortcut)

# epsg code
prj <- 26911
prjutm <- utm_wgs84(11)

# biology data ------------------------------------------------------------

# biological data linked to nhd
biodat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Marcus/data_and_scripts/species_occurrence_unique_NHDFlowline_JOIN_NAD83.shp')
biodat <- biodat %>% 
  dplyr::select(FID_1, date, name, occurrence, COMID) %>% 
  mutate(
    date = as.character(date),
    date = ifelse(date == as.Date('2028-07-07'), as.Date('2018-07-07'), date),
    date = as.Date(date, origin = "1970-01-01"),
    name = as.character(name), 
    yr = year(date), 
    mo = month(date),
    occurrence = factor(occurrence, levels = c(0, 1), labels = c('absent', 'present')),
    occol = ifelse(occurrence == 'present', 'lightgreen', 'tomato1'),
    occex = ifelse(occurrence == 'present', 7, 3)
  ) %>% 
  filter(name %in% c('arroyo chub', 'arroyo toad', 'least bell\'s vireo', 'rainbow trout', 'santa ana sucker', 'southwestern pond turtle')) %>% 
  st_transform(prj)

save(biodat, file = 'data/biodat.RData', compress = 'xz')

# Reach shapefile ---------------------------------------------------------

# ##
# # evalue file size and plot times for difference tolerance values
# toeval <- tibble(
#   tols = seq(0, 1000, by = 10),
#   size = NA, 
#   plttm = NA
# )
# 
# rchdat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Marcus/data_and_scripts/Flowline.shp') %>% 
#   st_transform(prjutm) 
# 
# for(i in 1:nrow(toeval)){
#   
#   cat(i, '\t')
#   
#   tolvl <- toeval[i, ] %>% pull(tols)
#   
#   tosv <- rchdat %>%   
#     st_simplify(dTolerance = tolvl, preserveTopology = T) %>%
#     st_transform(prj)
#   
#   save(tosv, file = 'data/tosv.RData', compress = 'xz')
#   
#   flsz <- file.info('data/tosv.RData')$size / 1000
#   
#   strt <- Sys.time()
#   mapview(tosv)
#   elaps <- Sys.time() - strt
#   
#   toeval[i, 'size'] <- flsz
#   toeval[i, 'plttm'] <- as.numeric(elaps)
#   
# }
# 
# file.remove('data/tosv.RData')
# 
# plot(size ~ tols, toeval, type = 'l')
# plot(plttm ~ tols, toeval, type = 'l')

rchdat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Marcus/data_and_scripts/Flowline.shp') %>% 
  st_transform(prjutm) %>% 
  st_simplify(dTolerance = 220, preserveTopology = T) %>%
  st_transform(prj)

save(rchdat, file = 'data/rchdat.RData', compress = 'xz')

# Metrics -----------------------------------------------------------------

# flow metrics calculated where biology was observed
file.copy('../flowmetrics/data/bioflowmetest.RData', 'data/')
