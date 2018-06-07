# processing data 

library(tidyverse)
library(sf)

# biological data linked to nhd

biodat <- st_read('Z:/JennyTaylor/Data Visualization/Species_NHD.shp')
save(biodat, file = 'data/biodat.RData', compress = 'xz')
