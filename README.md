## Selecting primary healthcare centres for surveillance of *Plasmodium knowlesi* malaria using geospatial models

Code repo attached to draft thesis chapter / paper

#### TODO
  - store info on duplicated sites somewhere
  - go through *all* the sites and check their coords ffs
  - write this so I'm grabbing health_sites directly from moh dataset, and prelim sites
  - chuck districts shp into repo or grab from MAP pkg
  - check `objective` is definitely Tobin 2024
    - mask out non-Kalimantan/Sumatra pix in lulc_covs? 
    - Grab back Malaysian Borneo pixels for edge effects if they exist somewhere?
    - basically just check all the masks match
  - re-landing step isn't quite right ....
  - check crazy hard-coded threshold in catchment summary
  - check somewhere that I'm only including travel time catchments with at least one pixel in them
  - ~am now thinking that I would prefer xtable to kable? idk? give it a squizz?~
    - ~recolour ecoconstraints column~ :)
    - attach maps of "closest-point" catchments to the closest point supp ... could be a faff