# main

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

source("code/plot_functions.R")

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

# health_sites$dist_catch = apply(health_sites, 1,
#                                 getDistanceCatchment,
#                                 radius_m=30000, ras=lulc_covs$objective)
# 
# health_sites$time_catch = sapply(1:nrow(health_sites), getTimeCatchment,
#                                  points=health_points,
#                                  transition_surfaces=transition_surfaces,
#                                  island_groups=health_sites$island_group,
#                                  to_surface=lulc_covs$objective,
#                                  time_cap=100)
# 
# health_sites$stretch_catch = wrapNearestSiteCatchment(district_shapes,
#                                                       health_sites)
# 
# # stretch catch map doesn't look too bad?
# plot(sum(stack(health_sites$stretch_catch), na.rm=TRUE))
# plot(st_geometry(district_shapes), add=TRUE)


# need to save progress here please:
# saveRDS(health_sites, file = "output/health_sites.rds")
health_sites = readRDS(file = "output/health_sites.rds")

# now for some summarisation:

land_use_names = c("human_pop","croplands", "oil_palm", "loss_year")
land_use_crits = quantile(lulc_covs, probs=c(0,0.1,0.5,0.9,1))[land_use_names, 2]
# manually encode crit for oil palm
land_use_crits[3] = 1
# still using 10% quantile for human pop but given the spread of hpop values .. I'm happy with that

# might be sensible to check all sites are actually on land?
check_empty_catches <- lapply(health_sites$time_catch, 
                              function(x){
                                return(ifelse(sum(is.na(values(x))) == ncell(x), TRUE, FALSE))
                              })
which(unlist(check_empty_catches) == TRUE)
# don't trust this at all ..
#health_sites <- health_sites[-c(which(unlist(check_empty_catches) == TRUE)),]

source("code/catchment_summary.R")

dist_catch_summary = lapply(health_sites$dist_catch,
                            catchment_summary_stats, 
                            lulc_covs)

time_catch_summary = lapply(health_sites$time_catch,
                            catchment_summary_stats, 
                            lulc_covs)

# All of these should have at least one pixel to themselves if the duplicate removal worked?
# although I think I'm using centroid locations so perhaps not if there is a site in an adjacent pixel
# closer than half the hypotenuse.......
stretch_catch_summary = lapply(health_sites$stretch_catch,
                               catchment_summary_stats,
                               lulc_covs)
# I'm through !

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
tmp_dist_summ = cbind(health_sites$name, 
                      dist_catch_summary[, c(1,2,3)], # npixel, mean and sd objective
                      ecotype_agg, 
                      dist_catch_summary[, 9:12], # max hpop, croplands, oil palm, loss year
                      dist_catch_summary[,5]) # hpop
names(tmp_dist_summ) = c("Name", "No' Pixels", "Objective Mean", "Objective Std Dev", 
                         "Eco-constraints  Present",  "Human Pop", "Croplands", "Oil Palm","Forest Loss", 
                         "Pop Discard")

head(time_catch_summary)
ecotype_agg = apply(time_catch_summary[, 5:8], 1, function(x){sum(x > 0)})
tmp_time_summ = cbind(health_sites$name, 
                      time_catch_summary[, c(1,2,3)], 
                      ecotype_agg, 
                      time_catch_summary[, 9:12], 
                      time_catch_summary[,5])
names(tmp_time_summ) = c("Name", "No' Pixels", "Objective Mean", "Objective Std Dev", 
                         "Eco-constraints  Present",  "Human Pop", "Croplands", 
                         "Oil Palm","Forest Loss", "Pop Discard")

head(stretch_catch_summary)
ecotype_agg = apply(stretch_catch_summary[, 5:8], 1, function(x){sum(x > 0)})
tmp_stretch_summ = cbind(health_sites$name, 
                      stretch_catch_summary[, c(1,2,3)], 
                      ecotype_agg, 
                      stretch_catch_summary[, 9:12], 
                      stretch_catch_summary[,5])
names(tmp_stretch_summ) = c("Name", "No' Pixels", "Objective Mean", "Objective Std Dev", 
                         "Eco-constraints  Present",  "Human Pop", "Croplands", 
                         "Oil Palm","Forest Loss", "Pop Discard")

# now to make up those tables ...
source("code/table_functions.R")

# jump from here into simple_table_out.R for all the sinking/malinau map

##################################################################################

malinau_sites <- health_sites[health_sites$regency == "MALINAU",]

malinau_time <- stack(malinau_sites$time_catch) %>%
  trim()
malinau_time[!is.na(malinau_time)] = 1
malinau_obj <- crop(lulc_covs$objective, malinau_time)

eval_obj_surface_mean <- function(inds, catch_stack, obj_surf){
  # is this quicker than anything else I've written before?
  tmp <- sum(catch_stack[[inds]], na.rm=TRUE)
  tmp[tmp == 0] = NA
  return(c(mean(values(mask(obj_surf, tmp)), na.rm=TRUE), inds))
}

eval_obj_surface_sum <- function(inds, catch_stack, obj_surf){
  # is this quicker than anything else I've written before?
  tmp <- sum(catch_stack[[inds]], na.rm=TRUE)
  tmp[tmp == 0] = NA
  return(c(sum(values(mask(obj_surf, tmp)), na.rm=TRUE), inds))
}

eval_catch_size <- function(inds, catch_stack, obj_surf){
  # is this quicker than anything else I've written before?
  return(c(sum(values(sum(catch_stack[[inds]], na.rm=TRUE))!=0), inds))
}


malinau_obj1 = sapply(1:nrow(malinau_sites), function(x){
  eval_obj_surface_mean(x, malinau_time, malinau_obj)
})

malinau_obj2 = combn(length(malinau_sites), 2, 
                     eval_obj_surface_mean, TRUE, malinau_time, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

malinau_obj2_sum = combn(length(malinau_sites), 2,
                     eval_obj_surface_sum, TRUE, malinau_time, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

malinau_obj2_size = combn(length(malinau_sites), 2,
                         eval_catch_size, TRUE, malinau_time, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

# would be good to have some sort of a table with a graphical representation
# do this when I get to it !

malinau_obj3 = combn(length(malinau_sites), 3, eval_obj_surface, TRUE,
                     malinau_time, malinau_obj)

# jump to a plot ...
pal=rev(viridis(12)[2:11])

{png("figures/malinau_one_obj_two_comb.png",
    height=1200, width=1100, pointsize=30)
  par(xpd=TRUE)
plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
     )
points(malinau_sites[,c("lon","lat")], pch=4)
select = c(1:9)
label_two_combs(malinau_obj2[select,], malinau_sites,
                centre=c(115.1,2.8),
                label_radius=2,
                n_toadstools = 60,
                gap=0.99,
                pal=pal[select],
                labs=select)
select = c(10)
label_two_combs(malinau_obj2[select,], malinau_sites,
                centre=c(115.1,3.2),
                label_radius=2,
                n_toadstools = 60,
                pal=pal[select],
                labs=select)
dev.off()}

pairsdf <- malinau_obj2 %>%
  left_join(malinau_obj2_size, by=c("site1", "site2")) %>%
  left_join(malinau_obj2_sum, by=c("site1", "site2")) %>%
  rename(mean_obj=obj.x, sum_obj=obj, catch_size=obj.y)

{png("figures/malinau_one_obj_two_comb_twopanel.png",
     height=2000, width=2000, pointsize=30)
  par(xpd=TRUE, mfrow=c(2,2), mar=c(2,2,2,2))
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select = c(1:9)
  label_two_combs(malinau_obj2[select,], malinau_sites,
                  centre=c(115.1,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  select = c(10)
  label_two_combs(malinau_obj2[select,], malinau_sites,
                  centre=c(115.1,3.2),
                  label_radius=2,
                  n_toadstools = 60,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(a)")
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(malinau_obj2_sum[select,], malinau_sites,
                  centre=c(115.5,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(b)")
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(malinau_obj2_size[select,], malinau_sites,
                  centre=c(115.5,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,s
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(c)")
  dev.off()}

{png("figures/malinau_one_obj_two_comb_twopanel_pairs.png",
    height=2000, width=2000, pointsize=50)
  pairs(pairsdf[c("mean_obj", "sum_obj", "catch_size")])
  dev.off()}

##################################################################################






