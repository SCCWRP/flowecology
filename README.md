# README

Website [here](https://sccwrp.shinyapps.io/flowecology/)

## Data

* `allrchdat.RData` all NHD flow reaches for RB4 region

* `biodat.RData` compiled biogy data, species p/a by date and location

* `bioflowmetest.RData` estimated flow metrics from rf models where biology was observed in `biodat.RData`

* `biotmpmet.RData` baseline biology with temperature metrics used to fit linear mods

* `bsest.RData` baseline species prob occurrence predictions, all species, wet/mod/dry years, min probability from temperature and flow metric predictions

* `bsflowmetest.RData` estimated flow metrics from rf models for selected baseline years 1993 (wet), 2010 (moderate), 2014 (dry), all COMID

* `bstmpmetest.RData` estimated temperature metrics for all COMIDs in all baseline years

* `futest.RData` Future species prob occurrence predictions, all species, models, 2040, 2100, min probability from temperature and flow metric predictions

* `metmods.RData` list with twelve elements, six rf flow models and six temperature glms

* `obsbioflo.RData` observed biology presence/absence merged with flow metrics

* `rchdat.RData` NHD flow reaches for RB4 region, subset by unaltered locations
