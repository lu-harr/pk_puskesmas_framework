## Selecting primary healthcare centres for surveillance of *Plasmodium knowlesi* malaria using geospatial models

Code repo attached to draft thesis chapter / paper

### TODO
  - mask lulc_covs to sumatra/kalimantan only?
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
  - tables need labels! Organise by district instead of catchment type!
    
#### FIGURES
  - map: superimpose districts masked over predicted risk
  - DONE catchment demos
  - objective surfaces in key districts?
  - supp: forest fringe/oil palm calculation figures
  - supp: dominant land cover?
  - DONE supp: tables
  - DONE supp: maps with labelled ranked sites
  - supp: sensitivity to catchment definition: raw/ranked for travel time/distance
  - supp: sensitivity to site location: for travel time/distance
  - maps of "closest-point" catchments for all districts