# README

Website [here](https://sccwrp.shinyapps.io/flowecology/)

## Data

* `allrchdat.RData` all NHD flow reaches for RB4 region

* `biodat.RData` compiled biogy data, species p/a by date and location

* `bioflowmetest.RData` estimated flow metrics from rf models where biology was observed in `biodat.RData`

* `biotmpmet.RData` baseline biology with temperature metrics used fit linear mods

* `bsflowmetest.RData` estimated flow metrics from rf models for selected baseline years 1993 (wet), 2010 (moderate), 2014 (dry), all COMID

* `bstmpmetest.RData` estimated temperature metrics for all COMIDs in all baseline years

* `metmods.RData` list with twelve elements, six rf flow models and six temperature glms

* `rchdat.RData` NHD flow reaches for RB4 region, subset by unaltered locations

* `futest.RData` Future species prob occurrence predictions, all species, models, 2040, 2100, min probability from temperature and flow metric predictions