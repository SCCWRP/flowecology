# processing data 

library(tidyverse)
library(sf)
library(lubridate)
library(proj4shortcut)
library(randomForest)
library(here)

# epsg code
prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
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
  mutate(name = 
          case_when(
            name %in% 'arroyo chub' ~ 'chub',
            name %in% 'arroyo toad' ~ 'toad',
            name %in% "least bell's vireo" ~ 'vireo',
            name %in% 'rainbow trout' ~ 'trout',
            name %in% 'santa ana sucker' ~ 'sucker',
            name %in% 'southwestern pond turtle' ~ 'turtle'
        )) %>% 
  rename(spp = name) %>% 
  st_transform(prj)

save(biodat, file = 'data/biodat.RData', compress = 'xz')

# watershed polygon -------------------------------------------------------

wshed <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Jenny/RB4/WorkingData_3-16-18/RB4WatershedBoundaty.shp') %>% 
  st_transform(prjutm) %>% 
  st_simplify(dTolerance = 100, preserveTopology = T) %>%
  st_transform(prj) %>% 
  dplyr::select(shed = NAME)

save(wshed, file = here("data", "wshed.RData"), compress = 'xz')

# reach shapefile ---------------------------------------------------------

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

# waterhsed data to join
data(wshed)

rchdat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Marcus/data_and_scripts/Flowline.shp') %>% 
  st_transform(prjutm) %>% 
  st_simplify(dTolerance = 220, preserveTopology = T) %>%
  st_transform(prj) %>% 
  st_intersection(wshed)

save(rchdat, file = 'data/rchdat.RData', compress = 'xz')

# all reaches, filtered by locations for future predictions ---------------

# waterhsed data to join
data(wshed)

allrchdat <- st_read('//172.16.1.5/Biology/Flow ecology and climate change_ES/Jenny/RB4/WorkingData_3-16-18/NHDFlowline_Clip_NAD1983_UTMzone11.shp') %>% 
  st_zm() %>% 
  filter(FTYPE %in% 'StreamRiver') %>% 
  select(COMID) %>% 
  st_simplify(dTolerance = 220, preserveTopology = T) %>%
  st_transform(prj) %>% 
  st_intersection(wshed)

save(allrchdat, file = 'data/allrchdat.RData', compress = 'xz')

# flow and temperature metrics with observed bio presence/absence -------------------------

# original from JT script L:\Flow ecology and climate change_ES\Jenny\RB4\FlowModel\BiologicalFlowModel.R

## flow 

#read in dataframe that has list of COMIDS in unaltered clusters, remove the unaltered COMID's that also happen to be gages
clusters<- st_read("../../Jenny/RB4/StreamCat/COMID clustering.shp") %>% 
  data.frame() %>% 
  select(COMID, clstrCt, dam) %>% 
  filter(clstrCt == 1 & dam == 0 )

#load baseline predictor.  Need to remove 
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/bsflowmetest.RData")
bsflowmetest <- bsflowmetest %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est) %>% 
  dplyr::filter(COMID %in% clusters$COMID) %>% 
  select(-tenyr)

#biological models
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/bioflowmetest.RData")
bioflowmetest<- bioflowmetest%>% 
  dplyr::filter(COMID %in% clusters$COMID) %>% 
  select(-4, -tenyr)
occurrence<- st_read("../../Jenny/RB4/WorkingData_3-16-18/species_occurrence_unique_NHDFlowline_JOIN_NAD83.shp") %>% 
  data.frame() %>% 
  select(name, date, COMID, occurrence)
occurrence$date<-ymd(occurrence$date)
occurrence$name<-as.character(occurrence$name)

dat<- left_join(occurrence, bioflowmetest, by = c("name", "date", "COMID")) %>% 
  unique()

dat <- dat %>% 
  filter(COMID %in% clusters$COMID)


#read in modeled flow metric dataframe and keep only the ones from the gages/ SCR tribs
gages <- read.csv("../../Jenny/RB4/WorkingData_3-16-18/FlowMetrics_JT.csv") %>% 
  filter(COMID %in% c(948070372, 22521721, 17585808, 17575785, 17572259, 17567911, 17572335, 22514774,
                      17574717, 17574705, 17574697, 17574691, 17574683, 17574649, 17574613)) %>% 
  select(name, date, COMID, occurrence, 8:164, 166 ) 

#bind dat with gages to have final model building dataset
names(gages) = gsub(pattern = "yr", replacement = "", x = names(gages))
names(gages)[158]<- "X3yrRBI"
names(gages)[159]<- "X5yrRBI"
names(gages)[160]<- "X10yrRBI"
names(gages)[161]<- "fivyr"
names(gages)[162]<- "twoyr"
gages$date<- ymd(gages$date)
gages$name<- as.character(gages$name)
gages$COMID<- as.numeric(gages$COMID)
gages$occurrence<- as.numeric(gages$occurrence)
gages$x3_MaxMonth <- as.numeric(gages$x3_MaxMonth)
gages$x3_MinMonth <- as.numeric(gages$x3_MinMonth)
gages$x5_MaxMonth <- as.numeric(gages$x5_MaxMonth)
gages$x5_MinMonth <- as.numeric(gages$x5_MinMonth)
gages$all_MaxMonth <- as.numeric(gages$all_MaxMonth)
gages$all_MinMonth <- as.numeric(gages$all_MinMonth)
gages$x10_MaxMonth <- as.numeric(gages$x10_MaxMonth)
gages$x10_MinMonth <- as.numeric(gages$x10_MinMonth)
gages$all_MedianNoFlowDays <- as.numeric(gages$all_MedianNoFlowDays)
gages$all_HighNum  <- as.numeric(gages$all_HighNum )
#gages <- gages[complete.cases(gages),]

test<- bind_rows(dat, gages)

data<- test

#want to remove duplicate values from the same month (ie if a vireo was found on two separate days in the same month, we dont want to count that twice)
data$month<-month(data$date)
data$month<-as.integer(data$month)
data$year<-year(data$date)
data<-select(data, -date) %>% 
  unique

data<-select(data, c(1:3, 162:163, 4:161))

#remove 0 occurrence obs in same month and COMID as a 1 occurrence obs
data <- data %>% 
  group_by(name, COMID, year, month) %>% 
  nest %>% 
  mutate(
    nozero = map(data, function(x){
      
      # do nothing if all occurrence values are 1 or all are 0
      if(!any(x$occurrence == 0) | !any(x$occurrence == 1))
        return(x)
      
      # sort by occurrence
      x <- x %>% 
        arrange(-occurrence)
      
      # duplicated
      dups <- x %>% 
        select(-occurrence) %>% 
        duplicated
      
      # filter duplicates and zero occurrence
      x <- x %>% 
        filter(!(occurrence == 0 & dups))
      
      return(x)
      
    })
  ) %>% 
  select(-data) %>% 
  unnest %>% 
  ungroup

dat <-data %>% 
  data.frame()

turtle<- dat %>% filter(name == "southwestern pond turtle")#need to get this b4 the complete cases bc can only use the 'all year scenario'
dat <- dat[complete.cases(dat),] %>% 
  filter(!name %in% 'southwestern pond turtle') %>% 
  bind_rows(turtle)

bioflomet <- dat %>% 
  rename(
    spp = name
  ) %>% 
  mutate(
    spp = case_when(
      spp %in% 'arroyo chub' ~ 'chub',
      spp %in% 'arroyo toad' ~ 'toad',
      spp %in% "least bell's vireo" ~ 'vireo',
      spp %in% 'rainbow trout' ~ 'trout',
      spp %in% 'santa ana sucker' ~ 'sucker',
      spp %in% 'southwestern pond turtle' ~ 'turtle'
    ),
    mettypobs = 'Flow'
  ) %>% 
  dplyr::select(-month) %>% 
  gather('met', 'val', -spp, -COMID, -year, -occurrence, -mettypobs)

## temperature

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
  select(-ind, -value) %>% 
  rename(
    spp = name
  ) %>% 
  mutate(
    spp = case_when(
      spp %in% 'arroyo chub' ~ 'chub',
      spp %in% 'arroyo toad' ~ 'toad',
      spp %in% "least bell's vireo" ~ 'vireo',
      spp %in% 'rainbow trout' ~ 'trout',
      spp %in% 'santa ana sucker' ~ 'sucker',
      spp %in% 'southwestern pond turtle' ~ 'turtle'
    ),
    mettypobs = 'Temperature'
  ) %>% 
  gather('met', 'val', -spp, -COMID, -year, -occurrence, -mettypobs)

obsbiomet <- bioflomet %>% 
  bind_rows(biotmpmet)

save(obsbiomet, file = 'data/obsbiomet.RData', compress = 'xz')

# get baseline predictions ------------------------------------------------

# original from JT script L:\Flow ecology and climate change_ES\Jenny\RB4\FlowModel\BiologicalFlowModel.R

load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/bsflowmetest.RData")
bsflowmetest <- bsflowmetest %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est) %>% 
  dplyr::filter(COMID %in% clusters$COMID) %>% 
  select(-tenyr)

load(file= "../../Jenny/RB4/FlowModel/mod_chub_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_trout_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_sucker_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_toad_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_turtle_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_vireo_rf.RData")

# predictions based on baseline random forest flow metrics
trout_bs_flow <- (predict(mod_trout_rf, newdata = bsflowmetest, type = "prob"))[,2]
chub_bs_flow <- (predict(mod_chub_rf, newdata = bsflowmetest, type = "prob"))[,2]
suc_bs_flow <- (predict(mod_sucker_rf, newdata = bsflowmetest, type = "prob"))[,2]
toad_bs_flow <- (predict(mod_toad_rf, newdata = bsflowmetest, type = "prob"))[,2]
turtle_bs_flow <- (predict(mod_turtle_rf, newdata = bsflowmetest, type = "prob"))[,2]
vireo_bs_flow <- (predict(mod_vireo_rf, newdata = bsflowmetest, type = "prob"))[,2]

NHD <- st_read("../../Jenny/RB4/WorkingData_3-16-18/NHDFLowline_Clip.shp")  %>% 
  filter(FTYPE == "StreamRiver") %>% 
  select(COMID) %>% 
  st_zm()

wtrshd_bndry<- st_read("../../Jenny/RB4/WorkingData_3-16-18/RB4watershedBoundaty.shp") %>% 
  select(NAME) %>% 
  st_zm()

spcs_occrrnce <- st_read("../../Jenny/RB4/WorkingData_3-16-18/species_occurrence_unique_NHDFlowline_JOIN_NAD83.shp") %>% 
  filter(name %in% c("rainbow trout", "arroyo chub", "santa ana sucker", "arroyo toad", "southwestern pond turtle", "least bell's vireo")) %>% 
  filter(occurrence == 1) %>% 
  select(name, occurrence, COMID, date) %>% 
  st_zm()
#clip species data to just the points within the watershed boundaries
spcs_occrrnce<- spcs_occrrnce[wtrshd_bndry,]

clusters<- st_read("../../Jenny/RB4/StreamCat/COMID clustering.shp") %>% 
  data.frame() %>% 
  dplyr::select(COMID, clstrCt, dam) %>% 
  filter(clstrCt == 1 & dam == 0 )

baseline <- cbind(bsflowmetest, trout_bs_flow, chub_bs_flow, suc_bs_flow, toad_bs_flow, turtle_bs_flow, vireo_bs_flow)


#predictions based on baseline temperature logistic regression  
vireo_temp<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/vireo.shp")%>% 
  filter(COMID %in% NHD$COMID)
sucker_temp<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/sucker.shp")%>% 
  filter(COMID %in% NHD$COMID)
trout_temp<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/trout.shp") %>% 
  filter(COMID %in% NHD$COMID)
chub_temp<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/chub.shp")%>% 
  filter(COMID %in% NHD$COMID)
toad_temp<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/toad.shp")%>% 
  filter(COMID %in% NHD$COMID)
turtle_temp<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/turtle.shp")%>% 
  filter(COMID %in% NHD$COMID)

baseline$year <- year(baseline$dtsl)

#prep dataframes
chub_syn<- left_join(chub_temp, baseline, by = c("COMID", "year")) %>% 
  select(COMID, year, chub, chub_bs_flow) %>% 
  filter(COMID %in% clusters$COMID) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    syn = pmin(chub_bs_flow, chub),
    syn = as.numeric(syn),
    spp = 'chub'
  ) %>% 
  select(
    COMID, 
    spp, 
    dts = year, 
    flow = chub_bs_flow, 
    temp = chub,
    syn = syn
  )

trout_syn <- left_join(trout_temp, baseline, by = c("COMID", "year")) %>% 
  select(COMID, year, trout, trout_bs_flow) %>% 
  filter(COMID %in% clusters$COMID) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    syn = pmin(trout_bs_flow, trout),
    syn = as.numeric(syn),
    spp = 'trout'
  ) %>% 
  select(
    COMID, 
    spp, 
    dts = year, 
    flow = trout_bs_flow, 
    temp = trout,
    syn = syn
  )

suc_syn<- left_join(sucker_temp, baseline, by = c("COMID", "year")) %>% 
  select(COMID, year, suckr, suc_bs_flow) %>% 
  filter(COMID %in% clusters$COMID) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    syn = pmin(suc_bs_flow, suckr),
    syn = as.numeric(syn),
    spp = 'sucker'
  ) %>% 
  select(
    COMID, 
    spp, 
    dts = year, 
    flow = suc_bs_flow, 
    temp = suckr,
    syn = syn
  )

vireo_syn<- left_join(vireo_temp, baseline, by = c("COMID", "year")) %>% 
  select(COMID, year, vireo, vireo_bs_flow) %>% 
  filter(COMID %in% clusters$COMID) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    syn = pmin(vireo_bs_flow, vireo),
    syn = as.numeric(syn),
    spp = 'vireo'
  ) %>% 
  select(
    COMID, 
    spp, 
    dts = year, 
    flow = vireo_bs_flow, 
    temp = vireo,
    syn = syn
  )

toad_syn<- left_join(toad_temp, baseline, by = c("COMID", "year")) %>% 
  select(COMID, year, toad, toad_bs_flow) %>% 
  filter(COMID %in% clusters$COMID) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    syn = pmin(toad_bs_flow, toad),
    syn = as.numeric(syn),
    spp = 'toad'
  ) %>% 
  select(
    COMID, 
    spp, 
    dts = year, 
    flow = toad_bs_flow, 
    temp = toad,
    syn = syn
  )

turtle_syn<- left_join(turtle_temp, baseline, by = c("COMID", "year")) %>% 
  select(COMID, year, turtl, turtle_bs_flow) %>% 
  filter(COMID %in% clusters$COMID) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    syn = pmin(turtle_bs_flow, turtl),
    syn = as.numeric(syn),
    spp = 'turtle'
  ) %>% 
  select(
    COMID, 
    spp, 
    dts = year, 
    flow = turtle_bs_flow, 
    temp = turtl,
    syn = syn
  )

bsest <- rbind(chub_syn, vireo_syn, trout_syn, toad_syn, turtle_syn, suc_syn) %>% 
  gather('bsmettyp', 'prd', flow, syn, temp) %>% 
  select(COMID, spp, dts, prd, bsmettyp)

save(bsest, file = here('data', 'bsest.RData'), compress = 'xz')

# get future predictions --------------------------------------------------

# original from JT script L:\Flow ecology and climate change_ES\Jenny\RB4\FlowModel\BiologicalFlowModel.R

load(file= "../../Jenny/RB4/FlowModel/mod_chub_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_trout_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_sucker_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_toad_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_turtle_rf.RData")
load(file= "../../Jenny/RB4/FlowModel/mod_vireo_rf.RData")

clusters<- st_read("../../Jenny/RB4/StreamCat/COMID clustering.shp") %>% 
  data.frame() %>% 
  dplyr::select(COMID, clstrCt, dam) %>% 
  filter(clstrCt == 1 & dam == 0 )

NHD <- st_read("../../Jenny/RB4/WorkingData_3-16-18/NHDFLowline_Clip.shp") %>% 
  filter(FTYPE == "StreamRiver") %>% 
  dplyr::select(COMID) %>% 
  st_zm()

wtrshd_bndry<- st_read("../../Jenny/RB4/WorkingData_3-16-18/RB4watershedBoundaty.shp") %>% 
  dplyr::select(NAME) %>% 
  st_zm()

load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/canesm2flowmetdt1.RData")
canesm2flowmetdt1 <- canesm2flowmetdt1 %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est)
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/canesm2flowmetdt2.RData")
canesm2flowmetdt2 <- canesm2flowmetdt2 %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est)
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/ccsm4flowmetdt1.RData")
ccsm4flowmetdt1 <- ccsm4flowmetdt1 %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est)
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/ccsm4flowmetdt2.RData")
ccsm4flowmetdt2 <- ccsm4flowmetdt2 %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est)
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/miroc5flowmetdt1.RData")
miroc5flowmetdt1 <- miroc5flowmetdt1 %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est)
load("../../Jenny/RB4/WorkingData_3-16-18/flowmetrics/data/miroc5flowmetdt2.RData")
miroc5flowmetdt2 <- miroc5flowmetdt2 %>% 
  select(var, COMID, dtsl, est) %>% 
  spread(var, est)

trout_CanESM2_2040_flow <- (predict(mod_trout_rf, newdata = canesm2flowmetdt1, type = "prob"))[,2]
trout_CanESM2_2100_flow <- (predict(mod_trout_rf, newdata = canesm2flowmetdt2, type = "prob"))[,2]
trout_ccsm4_2040_flow <- (predict(mod_trout_rf, newdata = ccsm4flowmetdt1, type = "prob"))[,2]
trout_ccsm4_2100_flow <- (predict(mod_trout_rf, newdata = ccsm4flowmetdt2, type = "prob"))[,2]
trout_miroc5_2040_flow <- (predict(mod_trout_rf, newdata = miroc5flowmetdt1, type = "prob"))[,2]
trout_miroc5_2100_flow <- (predict(mod_trout_rf, newdata = miroc5flowmetdt2, type = "prob"))[,2]

chub_CanESM2_2040_flow <- (predict(mod_chub_rf, newdata = canesm2flowmetdt1, type = "prob"))[,2]
chub_CanESM2_2100_flow <- (predict(mod_chub_rf, newdata = canesm2flowmetdt2, type = "prob"))[,2]
chub_ccsm4_2040_flow <- (predict(mod_chub_rf, newdata = ccsm4flowmetdt1, type = "prob"))[,2]
chub_ccsm4_2100_flow <- (predict(mod_chub_rf, newdata = ccsm4flowmetdt2, type = "prob"))[,2]
chub_miroc5_2040_flow <- (predict(mod_chub_rf, newdata = miroc5flowmetdt1, type = "prob"))[,2]
chub_miroc5_2100_flow <- (predict(mod_chub_rf, newdata = miroc5flowmetdt2, type = "prob"))[,2]

suc_CanESM2_2040_flow <- (predict(mod_sucker_rf, newdata = canesm2flowmetdt1, type = "prob"))[,2]
suc_CanESM2_2100_flow <- (predict(mod_sucker_rf, newdata = canesm2flowmetdt2, type = "prob"))[,2]
suc_ccsm4_2040_flow <- (predict(mod_sucker_rf, newdata = ccsm4flowmetdt1, type = "prob"))[,2]
suc_ccsm4_2100_flow <- (predict(mod_sucker_rf, newdata = ccsm4flowmetdt2, type = "prob"))[,2]
suc_miroc5_2040_flow <- (predict(mod_sucker_rf, newdata = miroc5flowmetdt1, type = "prob"))[,2]
suc_miroc5_2100_flow <- (predict(mod_sucker_rf, newdata = miroc5flowmetdt2, type = "prob"))[,2]

toad_CanESM2_2040_flow <- (predict(mod_toad_rf, newdata = canesm2flowmetdt1, type = "prob"))[,2]
toad_CanESM2_2100_flow <- (predict(mod_toad_rf, newdata = canesm2flowmetdt2, type = "prob"))[,2]
toad_ccsm4_2040_flow <- (predict(mod_toad_rf, newdata = ccsm4flowmetdt1, type = "prob"))[,2]
toad_ccsm4_2100_flow <- (predict(mod_toad_rf, newdata = ccsm4flowmetdt2, type = "prob"))[,2]
toad_miroc5_2040_flow <- (predict(mod_toad_rf, newdata = miroc5flowmetdt1, type = "prob"))[,2]
toad_miroc5_2100_flow <- (predict(mod_toad_rf, newdata = miroc5flowmetdt2, type = "prob"))[,2]

vireo_CanESM2_2040_flow <- (predict(mod_vireo_rf, newdata = canesm2flowmetdt1, type = "prob"))[,2]
vireo_CanESM2_2100_flow <- (predict(mod_vireo_rf, newdata = canesm2flowmetdt2, type = "prob"))[,2]
vireo_ccsm4_2040_flow <- (predict(mod_vireo_rf, newdata = ccsm4flowmetdt1, type = "prob"))[,2]
vireo_ccsm4_2100_flow <- (predict(mod_vireo_rf, newdata = ccsm4flowmetdt2, type = "prob"))[,2]
vireo_miroc5_2040_flow <- (predict(mod_vireo_rf, newdata = miroc5flowmetdt1, type = "prob"))[,2]
vireo_miroc5_2100_flow <- (predict(mod_vireo_rf, newdata = miroc5flowmetdt2, type = "prob"))[,2]

turtle_CanESM2_2040_flow <- (predict(mod_turtle_rf, newdata = canesm2flowmetdt1, type = "prob"))[,2]
turtle_CanESM2_2100_flow <- (predict(mod_turtle_rf, newdata = canesm2flowmetdt2, type = "prob"))[,2]
turtle_ccsm4_2040_flow <- (predict(mod_turtle_rf, newdata = ccsm4flowmetdt1, type = "prob"))[,2]
turtle_ccsm4_2100_flow <- (predict(mod_turtle_rf, newdata = ccsm4flowmetdt2, type = "prob"))[,2]
turtle_miroc5_2040_flow <- (predict(mod_turtle_rf, newdata = miroc5flowmetdt1, type = "prob"))[,2]
turtle_miroc5_2100_flow <- (predict(mod_turtle_rf, newdata = miroc5flowmetdt2, type = "prob"))[,2]


fut_flow <- cbind(canesm2flowmetdt1[,1], 
                  trout_CanESM2_2040_flow,trout_CanESM2_2100_flow, trout_ccsm4_2040_flow, trout_ccsm4_2100_flow, trout_miroc5_2040_flow, trout_miroc5_2100_flow,
                  chub_CanESM2_2040_flow, chub_CanESM2_2100_flow,chub_ccsm4_2040_flow, chub_ccsm4_2100_flow,chub_miroc5_2040_flow,  chub_miroc5_2100_flow,
                  suc_CanESM2_2040_flow, suc_CanESM2_2100_flow, suc_ccsm4_2040_flow, suc_ccsm4_2100_flow,suc_miroc5_2040_flow,suc_miroc5_2100_flow,
                  toad_CanESM2_2040_flow, toad_CanESM2_2100_flow, toad_ccsm4_2040_flow, toad_ccsm4_2100_flow, toad_miroc5_2040_flow, toad_miroc5_2100_flow,
                  vireo_CanESM2_2040_flow, vireo_CanESM2_2100_flow, vireo_ccsm4_2040_flow, vireo_ccsm4_2100_flow, vireo_miroc5_2040_flow, vireo_miroc5_2100_flow,
                  turtle_CanESM2_2040_flow, turtle_CanESM2_2100_flow, turtle_ccsm4_2040_flow, turtle_ccsm4_2100_flow, turtle_miroc5_2040_flow, turtle_miroc5_2100_flow)

rm(trout_CanESM2_2040_flow,trout_CanESM2_2100_flow, trout_ccsm4_2040_flow, trout_ccsm4_2100_flow, trout_miroc5_2040_flow, trout_miroc5_2100_flow,
   chub_CanESM2_2040_flow, chub_CanESM2_2100_flow,chub_ccsm4_2040_flow, chub_ccsm4_2100_flow,chub_miroc5_2040_flow,  chub_miroc5_2100_flow,
   suc_CanESM2_2040_flow, suc_CanESM2_2100_flow, suc_ccsm4_2040_flow, suc_ccsm4_2100_flow,suc_miroc5_2040_flow,suc_miroc5_2100_flow,
   toad_CanESM2_2040_flow, toad_CanESM2_2100_flow, toad_ccsm4_2040_flow, toad_ccsm4_2100_flow, toad_miroc5_2040_flow, toad_miroc5_2100_flow,
   vireo_CanESM2_2040_flow, vireo_CanESM2_2100_flow, vireo_ccsm4_2040_flow, vireo_ccsm4_2100_flow, vireo_miroc5_2040_flow, vireo_miroc5_2100_flow,
   turtle_CanESM2_2040_flow, turtle_CanESM2_2100_flow, turtle_ccsm4_2040_flow, turtle_ccsm4_2100_flow, turtle_miroc5_2040_flow, turtle_miroc5_2100_flow)
#Future air temperature predictions
#need to get the predictions from the Air_temp_future.R file
# MIROC5_stream_temp<- MIROC5_stream_temp %>%
#   filter(year %in% c(2040, 2100))
# names(MIROC5_stream_temp)[5:10]<- paste(names(MIROC5_stream_temp)[5:10], "miroc5_temp", sep = "_")
# 
# CCSM4_stream_temp<- CCSM4_stream_temp %>%
#   filter(year %in% c(2040, 2100))
# names(CCSM4_stream_temp)[5:10]<- paste(names(CCSM4_stream_temp)[5:10], "ccsm4_temp", sep = "_")
# 
# CanESM2_stream_temp<- CanESM2_stream_temp %>%
#   filter(year %in% c(2040, 2100))
# names(CanESM2_stream_temp)[5:10]<- paste(names(CanESM2_stream_temp)[5:10], "CanESM2_temp", sep = "_")
# 
# fut_temp<- cbind(MIROC5_stream_temp, CCSM4_stream_temp, CanESM2_stream_temp)
# fut_temp<- fut_temp %>%
#   select(-c(11:14, 21:24))
# 
# save(fut_temp, file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/fut_temp.RData')
load('L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/fut_temp.RData')

fut_temp_2040<-fut_temp %>% 
  filter(year %in% 2040) %>% 
  select(-(2:4))
names(fut_temp_2040)[2:19]<- paste(names(fut_temp_2040)[2:19], "2040", sep = "_")

fut_temp_2100<-fut_temp %>% 
  filter(year %in% 2100)%>% 
  select(-(2:4))
names(fut_temp_2100)[2:19]<- paste(names(fut_temp_2100)[2:19], "2100", sep = "_")

fut_temp<-left_join(fut_temp_2040, fut_temp_2100, by = "COMID")

future_predictions<- left_join(fut_temp, fut_flow, by = "COMID")

future_predictions<- left_join(NHD, future_predictions, by = "COMID") 

#need to calculate the syntheized temperature
future_predictions<- future_predictions %>% 
  mutate(trout_CanESM2_2040_syn = pmin(trout_CanESM2_2040_flow, trout_CanESM2_temp_2040),
         trout_CanESM2_2100_syn = pmin(trout_CanESM2_2100_flow, trout_CanESM2_temp_2100),
         trout_ccsm4_2040_syn = pmin(trout_ccsm4_2040_flow, trout_ccsm4_temp_2040),
         trout_ccsm4_2100_syn = pmin(trout_ccsm4_2100_flow, trout_ccsm4_temp_2100),
         trout_miroc5_2040_syn = pmin(trout_miroc5_2040_flow, trout_miroc5_temp_2040),
         trout_miroc5_2100_syn = pmin(trout_miroc5_2100_flow, trout_miroc5_temp_2100),
         chub_CanESM2_2040_syn = pmin(chub_CanESM2_2040_flow, chub_CanESM2_temp_2040),
         chub_CanESM2_2100_syn = pmin(chub_CanESM2_2100_flow, chub_CanESM2_temp_2100),
         chub_ccsm4_2040_syn = pmin(chub_ccsm4_2040_flow, chub_ccsm4_temp_2040),
         chub_ccsm4_2100_syn = pmin(chub_ccsm4_2100_flow, chub_ccsm4_temp_2100),
         chub_miroc5_2040_syn = pmin(chub_miroc5_2040_flow, chub_miroc5_temp_2040),
         chub_miroc5_2100_syn = pmin(chub_miroc5_2100_flow, chub_miroc5_temp_2100),
         suc_CanESM2_2040_syn = pmin(suc_CanESM2_2040_flow, sucker_CanESM2_temp_2040),
         suc_CanESM2_2100_syn = pmin(suc_CanESM2_2100_flow, sucker_CanESM2_temp_2100),
         suc_ccsm4_2040_syn = pmin(suc_ccsm4_2040_flow, sucker_ccsm4_temp_2040),
         suc_ccsm4_2100_syn = pmin(suc_ccsm4_2100_flow, sucker_ccsm4_temp_2100),
         suc_miroc5_2040_syn = pmin(suc_miroc5_2040_flow, sucker_miroc5_temp_2040),
         suc_miroc5_2100_syn = pmin(suc_miroc5_2100_flow, sucker_miroc5_temp_2100),
         vireo_CanESM2_2040_syn = pmin(vireo_CanESM2_2040_flow, vireo_CanESM2_temp_2040),
         vireo_CanESM2_2100_syn = pmin(vireo_CanESM2_2100_flow, vireo_CanESM2_temp_2100),
         vireo_ccsm4_2040_syn = pmin(vireo_ccsm4_2040_flow, vireo_ccsm4_temp_2040),
         vireo_ccsm4_2100_syn = pmin(vireo_ccsm4_2100_flow, vireo_ccsm4_temp_2100),
         vireo_miroc5_2040_syn = pmin(vireo_miroc5_2040_flow, vireo_miroc5_temp_2040),
         vireo_miroc5_2100_syn = pmin(vireo_miroc5_2100_flow, vireo_miroc5_temp_2100),
         toad_CanESM2_2040_syn = pmin(toad_CanESM2_2040_flow, toad_CanESM2_temp_2040),
         toad_CanESM2_2100_syn = pmin(toad_CanESM2_2100_flow, toad_CanESM2_temp_2100),
         toad_ccsm4_2040_syn = pmin(toad_ccsm4_2040_flow, toad_ccsm4_temp_2040),
         toad_ccsm4_2100_syn = pmin(toad_ccsm4_2100_flow, toad_ccsm4_temp_2100),
         toad_miroc5_2040_syn = pmin(toad_miroc5_2040_flow, toad_miroc5_temp_2040),
         toad_miroc5_2100_syn = pmin(toad_miroc5_2100_flow, toad_miroc5_temp_2100),
         turtle_CanESM2_2040_syn = pmin(turtle_CanESM2_2040_flow, turtle_CanESM2_temp_2040),
         turtle_CanESM2_2100_syn = pmin(turtle_CanESM2_2100_flow, turtle_CanESM2_temp_2100),
         turtle_ccsm4_2040_syn = pmin(turtle_ccsm4_2040_flow, turtle_ccsm4_temp_2040),
         turtle_ccsm4_2100_syn = pmin(turtle_ccsm4_2100_flow, turtle_ccsm4_temp_2100),
         turtle_miroc5_2040_syn = pmin(turtle_miroc5_2040_flow, turtle_miroc5_temp_2040),
         turtle_miroc5_2100_syn = pmin(turtle_miroc5_2100_flow, turtle_miroc5_temp_2100)
  )

future_predictions<- future_predictions[complete.cases(future_predictions$trout_miroc5_temp_2040),]

# format for use with app
futest <- future_predictions %>% 
  st_set_geometry(NULL) %>% 
  gather('var', 'prd', -COMID) %>% 
  mutate(
    mettyp = case_when(
      grepl('flow$', var) ~ 'flow',
      grepl('temp', var) ~ 'temp',
      grepl('syn$', var) ~ 'syn'
    ), 
    var = gsub('\\_temp|\\_flow$|\\_syn$', '', var)
  ) %>% 
  separate(var, c('spp', 'mds','dts'), sep = '_') %>% 
  mutate(
    spp = case_when(
      spp %in% c('suc', 'sucker') ~ 'sucker', 
      T ~ spp
    ),
    mds = case_when(
      mds == 'ccsm4' ~ 'CCSM4', 
      mds == 'miroc5' ~ 'MIROC5', 
      T ~ mds
    )
  )

save(futest, file = 'data/futest.RData', compress = 'xz')