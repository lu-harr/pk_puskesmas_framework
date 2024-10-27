## Selecting primary healthcare centres for surveillance of *Plasmodium knowlesi* malaria

Welcome to the repo for the fourth chapter of my thesis, *Using geospatial models to 
select primary healthcare centres for surveillance of *Plasmodium knowlesi* malaria*

In the `/code/` repository, you will find:

- `main.R`: reads in packages and sources other scripts, including:
  
    - `plot_functions.R`: a series of functions to simplify plotting
    
    - `sort_gis.R`: a script for reading in and organising data
    
    - `format_site_names.R`: cleans up healthcare site names
    
    - `thin_duplicate_sites.R`: checks for overlaps in the healthcare site dataset, 
    with respect to the raster dataset used in downstream analysis
    
    - `catchment_functions.R`: three functions to calculate three different types of catchments,
    provided with rasters and site locations (and distance/time limits)
    
    - `table_functions.R`: cubby hole for table summarisation code (to get things into xtable, etc)
    
    otherwise, all analysis steps are in the body of `main.R`
    
- `simple_table_out.R`: writes tabular outputs to `.tex` (Malinau/Langkat simple ranking figure code is in here)
  
- `robustness.R` and `catchment_limit_sensitivity.R`: catchment limit 
and site location sensitivity outputs

- `multiple_sites_figures.R`: code for latter figures in chapter

- `misc_figures.R`: other figures (e.g. `catchment_demos.png`) and supps

