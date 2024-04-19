# main

# TODO
  # store info on duplicated sites somewhere
  # go through *all* the sites and check their coords ffs
  # write this so I'm grabbing health_sites directly from moh dataset, and prelim sites
  # chuck districts shp into repo or grab from MAP pkg
  # check `objective` is definitely Tobin 2024
    # mask out non-Kalimantan/Sumatra pix in lulc_covs? 
    # Grab back Malaysian Borneo pixels for edge effects if they exist somewhere?
    # basically just check all the masks match
  # re-landing step isn't quite right ....
  # check crazy hard-coded threshold in catchment summary


setwd("~/Desktop/knowlesi/catchment_paper")
library(raster)
library(viridisLite)
library(scales)
library(dismo)
library(gdistance)
library(sf)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(kableExtra)

code_path = "code/"
source(paste0(code_path,"sort_gis.R"))
# Here are the district we're actually interested in:
message(paste0(district_shapes$district_name, collapse = ", "))

# brings in the PHCs - just MoH and sites suggested by ZOOMAL
health_sites = read.csv("data/health_sites.csv", header=TRUE)
# clean up site names over here :)
source(paste0(code_path,"format_site_names.R"))

# it's more useful if it's a factor ...
health_sites$regency = factor(health_sites$regency,
                              levels=district_shapes$district_name)

# remove sites that fall in the same raster pixel (may be true duplicates or just proximal sites)
source(paste0(code_path, "thin_duplicate_sites.R"))
# some coordinates are corrected in script
# should just put corrections into the original dataset :)

source(paste0(code_path, "catchment_functions.R"))

health_sites$dist_catch = apply(health_sites, 1,
                                getDistanceCatchment, 
                                radius_m=30000, ras=lulc_covs$objective)

health_sites$time_catch = sapply(1:nrow(health_sites), getTimeCatchment,
                                 points=health_points,
                                 transition_surfaces=transition_surfaces,
                                 island_groups=health_sites$island_group,
                                 to_surface=lulc_covs$objective)

health_sites$stretch_catch = wrapNearestSiteCatchment(district_shapes,
                                                      health_sites)

# stretch catch map doesn't look too bad?
plot(sum(stack(health_sites$stretch_catch), na.rm=TRUE))
plot(st_geometry(district_shapes), add=TRUE)


# need to save progress here please:
# saveRDS(health_sites, file = "output/health_sites.rds")
health_sites = readRDS(file = "output/health_sites.rds")

# now for some summarisation:

land_use_names = c("human_pop","croplands", "oil_palm", "loss_year")
land_use_crits = quantile(lulc_covs, probs=c(0,0.1,0.5,0.9,1))[land_use_names, 2]
# manually encode crit for oil palm
land_use_crits[3] = 1
# still using 10% quantile for human pop but given the spread of hpop values .. I'm happy with that

dist_catch_summary = lapply(health_sites$dist_catch,
                            catchment_summary_stats, 
                            lulc_covs)
# bit of a spooky warning here ...

time_catch_summary = lapply(health_sites$time_catch,
                            catchment_summary_stats, 
                            lulc_covs)
# aaaand there's an error here ...
# Error in .memtrimlayer(x, padding = padding, values = values, ...) :
#   only NA values found

stretch_catch_summary = lapply(health_sites$stretch_catch,
                               catchment_summary_stats,
                               lulc_covs)
library(data.table)
dist_catch_summary = as.data.frame(data.table::rbindlist(dist_catch_summary))
time_catch_summary = as.data.frame(data.table::rbindlist(time_catch_summary))
stretch_catch_summary = as.data.frame(data.table::rbindlist(stretch_catch_summary))

# now create tables
outpath = "output/tables/"


# ecotype_agg: number of ecotypes represented in each catch
# summaries: the table in a dataframe ready to go :)
head(dist_catch_summary)
ecotype_agg = apply(dist_catch_summary[,5:8], 1, function(x){sum(x > 0)})
tmp_dist_summ = cbind(health_sites$name, dist_catch_summary[, c(1,2,3)], ecotype_agg, dist_catch_summary[, 9:12], dist_catch_summary[,5])
names(tmp_dist_summ) = c("Name", "No' Pixels", "Objective Mean", "Objective Std Dev", "Eco-constraints  Present",  "Human Pop", "Croplands", "Oil Palm","Forest Loss", "Pop Discard")

head(time_catch_summary)
ecotype_agg = apply(time_catch_summary[, 5:8], 1, function(x){sum(x > 0)})
tmp_time_summ = cbind(health_sites$name, time_catch_summary[, c(1,2,3)], ecotype_agg, time_catch_summary[, 9:12], time_catch_summary[,5])
names(tmp_time_summ) = c("Name", "No' Pixels", "Objective Mean", "Objective Std Dev", "Eco-constraints  Present",  "Human Pop", "Croplands", "Oil Palm","Forest Loss", "Pop Discard")


# ranked_map calls - also need to provide
outpath = "~/Desktop/knowlesi/output/tables_030522/"
source(paste0(code_path,"7_rank_maps.R"))

