# main

# TODO
  # store info on duplicated sites somewhere
  # go through *all* the sites and check their coords ffs
  # write this so I'm grabbing health_sites directly from moh dataset, and prelim sites


setwd("~/Desktop/knowlesi/catchment_paper")
library(raster)
library(viridisLite)
library(scales)
library(dismo)
library(gdistance)
library(sf)
library(RColorBrewer)

code_path = "code/"
source(paste0(code_path,"sort_gis.R"))
DISTRICT_IND = c(2,3,4,5,6,7,9)

# brings in the PHCs
health_sites = read.csv("data/health_sites.csv",
                        header=TRUE)
source(paste0(code_path,"format_site_names.R"))

health_sites$regency = factor(health_sites$regency,
                              levels=pilot_shapes$KABKOT)

source(paste0(code_path, "thin_duplicate_sites.R"))
# some coordinates are corrected in script above, functional duplicate sites are removed

source(paste0(code_path, "catchment_functions.R"))

health_sites$dist_catch = apply(health_sites, 1,
                                getDistanceCatchment, 
                                radius_m=30000, ras=lulc_covs$objective)

health_sites$time_catch = sapply(1:nrow(health_sites), getTimeCatchment,
                                 points=health_points,
                                 transition_surfaces=transition_surfaces,
                                 island_groups=health_sites$island_group,
                                 to_surface=lulc_covs$objective)

health_sites$stretch_catch = wrapNearestSiteCatchment(pilot_shapes,
                                                      health_sites)

# doesn't look to bad?
plot(sum(stack(health_sites$stretch_catch), na.rm=TRUE))
plot(st_geometry(pilot_shapes), add=TRUE)


# need to save progress here please:
# saveRDS(health_sites, file = "health_sites.rds")
health_sites = readRDS(file = "health_sites.rds")

# now for some summarisation

