## Selecting primary healthcare centres for surveillance of *Plasmodium knowlesi* malaria using geospatial models

Code repo attached to draft thesis chapter / paper

### TODO
  - STOP MESSING AROUND! SORT YOUR GIS OUT! THIS IS GETTING SILLY!
    - Remove danylo_masked from git and put it somewhere else ... or split it up
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
  - get travel time catchments to be contiguous!
  - ~am now thinking that I would prefer xtable to kable? idk? give it a squizz?~
    - ~recolour ecoconstraints column~ :)
  - ~tables need labels! Organise by district instead of catchment type!~
  - Perhaps there should be a shiny app for navigating all the darn outputs?
  - Are Malaysian Borneo pixels included in catchments??
    
#### FIGURES
  - ~DONE map: superimpose districts masked over predicted risk~
    - make orange all the same, lines wider
  - ~DONE catchment demos~
  - objective surfaces in key districts?
  - supp: forest fringe/oil palm calculation figures
  - supp: dominant land cover?
  - ~DONE supp: tables~
  - ~DONE supp: maps with labelled ranked sites~
  - multiple site selection
  - multiple objective selection ? Left panel: geographic space, combinations of sites on front, right panel: objective space, with front, and enumerated objectives for all sampling designs
  - supp: sensitivity to catchment definition: raw/ranked for travel time/distance
  - supp: sensitivity to site location: for travel time only, wrt ranking table in main text
  - supp: maps of "closest-point" catchments for all districts