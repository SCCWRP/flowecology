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

# Flow metrics -----------------------------------------------------------------

# flow metrics calculated where biology was observed
file.copy('../flowmetrics/data/bioflowmetest.RData', 'data/')

# baseline flow metrics, all COMIDs
file.copy('../flowmetrics/data/bsflowmetest.RData', 'data/')

# RF models for flow metrics ----------------------------------------------

# chub
file.copy('../data_and_scripts/mod_chub_rf.RData', 'data/')

# sucker
file.copy('../data_and_scripts/mod_sucker_rf.RData', 'data/')

# toad
file.copy('../data_and_scripts/mod_toad_rf.RData', 'data/')

# trout
file.copy('../data_and_scripts/mod_trout_rf.RData', 'data/')

# turle
file.copy('../data_and_scripts/mod_turtle_rf.RData', 'data/')

# vireo
file.copy('../data_and_scripts/mod_vireo_rf.RData', 'data/')

# glms for temperature ----------------------------------------------------

# chub
file.copy('../../Jenny/AirTemp/Modeling/chub_mdl.rda', 'raw/')

# sucker
file.copy('../../Jenny/AirTemp/Modeling/suc_mdl.rda', 'raw/')

# toad
file.copy('../../Jenny/AirTemp/Modeling/toad_mdl.rda', 'raw/')

# trout
file.copy('../../Jenny/AirTemp/Modeling/trout_mdl.rda', 'raw/')

# turle
file.copy('../../Jenny/AirTemp/Modeling/turtle_mdl.rda', 'raw/')

# vireo
file.copy('../../Jenny/AirTemp/Modeling/vireo_mdl.rda', 'raw/')

# Extract model fitting data for temp GLMs --------------------------------

fls <- list.files('raw', pattern = '\\_mdl\\.rda$', full.names = T)
  
biotmpmet <- fls %>% 
  enframe('ind', 'value') %>% 
  mutate(dat = purrr::map(value, function(x){
      
      # load 
      load(file = x)
      nm <- basename(x) %>% gsub('\\.rda', '', .)
      obj <- get(nm) %>% .$data
      
      return(obj)
      
    })
  ) %>% 
  unnest %>% 
  select(-ind, -value)

save(biotmpmet, file = 'data/biotmpmet.RData', compress = 'xz')


# baseline temperature estimates for each COMID ---------------------------

load(file = '../../Jenny/AirTemp/Modeling/baseline_stream_temp.RData')

bstmpmetest <- baseline_stream_temp

save(bstmpmetest, file = 'data/bstmpmetest.RData', compress = 'xz')