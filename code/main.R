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

malinau_catch_list = lapply(1:nlayers(malinau_time), function(x){
  which(!is.na(values(malinau_time[[x]])))
})


# objective functions:
# THROW THESE IN THE TRASH !
# eval_obj_surface_mean <- function(inds, catch_stack, obj_surf){
#   # is this quicker than anything else I've written before?
#   tmp <- sum(catch_stack[[inds]], na.rm=TRUE)
#   tmp[tmp == 0] = NA
#   return(c(mean(values(mask(obj_surf, tmp)), na.rm=TRUE), inds))
# }
eval_obj_surface_mean <- function(inds, catch_list, obj_surf){
  pix <- unique(unlist(catch_list[inds]))
  return(c(mean(obj_surf[pix]), inds))
}
# 
# eval_obj_surface_sum <- function(inds, catch_stack, obj_surf){
#   # is this quicker than anything else I've written before?
#   tmp <- sum(catch_stack[[inds]], na.rm=TRUE)
#   tmp[tmp == 0] = NA
#   return(c(sum(values(mask(obj_surf, tmp)), na.rm=TRUE), inds))
# }
eval_obj_surface_sum <- function(inds, catch_list, obj_surf){
  pix <- unique(unlist(catch_list[inds]))
  return(c(sum(obj_surf[pix]), inds))
}
# 
# eval_catch_size <- function(inds, catch_stack, obj_surf){
#   # is this quicker than anything else I've written before?
#   return(c(sum(values(sum(catch_stack[[inds]], na.rm=TRUE))!=0), inds))
# }
eval_catch_size <- function(inds, catch_list, obj_surf){
  pix <- unique(unlist(catch_list[inds]))
  return(c(length(pix), inds))
}

eval_network_distance <- function(site_ids, dist_mat){
  if (length(site_ids) == 2){
    return(c(dist_mat[site_ids[1], site_ids[2]], site_ids))
  }
  site_ids = sort(unlist(site_ids)) # does this need to happen?
  dist_mat = dist_mat[site_ids, site_ids]
  tmp_mst = ape::mst(as.dist(dist_mat))
  picks = cbind(expand.grid(rownames(tmp_mst), colnames(tmp_mst)), 
                as.vector(tmp_mst))
  picks[, 1:2] = cbind(as.numeric(picks[, 1]), as.numeric(picks[, 2]))
  picks = picks[picks[, 3] == 1,]
  picks[, 1:2] = t(apply(picks[, 1:2], 1, sort))
  picks = unique(picks)
  c(sum(dist_mat[as.matrix(picks[, 1:2])]), site_ids)
}

{ptm <- proc.time()
  malinau_obj2_sum_alt = combn(nrow(malinau_sites), 2,
                           eval_obj_surface_sum, TRUE, malinau_catch_list, malinau_obj) %>%
     t() %>%
     as.data.frame() %>%
     rename(obj=V1, site1=V2, site2=V3) %>%
     arrange(desc(obj))
  print(proc.time() - ptm)}


# now let's apply them: 
malinau_obj1 = sapply(1:nrow(malinau_sites), function(x){
  eval_obj_surface_mean(x, malinau_catch_list, malinau_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

# reordering malinau_sites by mean objective so that the sites in tables are sorted left to right ..
malinau_sites <- malinau_sites[order(malinau_obj1$obj, decreasing = TRUE),]
malinau_catch_list <- malinau_catch_list[order(malinau_obj1$obj, decreasing = TRUE)]
malinau_distance_matrix = as.matrix(dist(malinau_sites[,c("lon","lat")], upper=TRUE))


malinau_obj1 = sapply(1:nrow(malinau_sites), function(x){
  eval_obj_surface_mean(x, malinau_catch_list, malinau_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

malinau_obj1_sum = sapply(1:nrow(malinau_sites), function(x){
  eval_obj_surface_sum(x, malinau_catch_list, malinau_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

malinau_obj1_size = sapply(1:nrow(malinau_sites), function(x){
  eval_catch_size(x, malinau_catch_list, malinau_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

malinau_obj2 = combn(nrow(malinau_sites), 2, 
                     eval_obj_surface_mean, TRUE, malinau_catch_list, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

malinau_obj2_sum = combn(nrow(malinau_sites), 2,
                     eval_obj_surface_sum, TRUE, malinau_catch_list, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

malinau_obj2_size = combn(nrow(malinau_sites), 2,
                         eval_catch_size, TRUE, malinau_catch_list, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

malinau_obj2_dist = combn(nrow(malinau_sites), 2,
                          eval_network_distance, TRUE, malinau_distance_matrix) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(obj)

# would be good to have some sort of a table with a graphical representation
# do this when I get to it !

malinau_obj3 = combn(nrow(malinau_sites), 3, eval_obj_surface_mean, TRUE,
                     malinau_catch_list, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4) %>%
  arrange(desc(obj))

malinau_obj4 = combn(nrow(malinau_sites), 4, eval_obj_surface_mean, TRUE,
                     malinau_catch_list, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5) %>%
  arrange(desc(obj))

malinau_obj5 = combn(nrow(malinau_sites), 5, eval_obj_surface_mean, TRUE,
                     malinau_catch_list, malinau_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5, site5=V6) %>%
  arrange(desc(obj))

malinau_pairsdf <- malinau_obj2 %>%
  left_join(malinau_obj2_size, by=c("site1", "site2")) %>%
  left_join(malinau_obj2_sum, by=c("site1", "site2")) %>%
  left_join(malinau_obj2_dist, by=c("site1", "site2")) %>%
  #rename(mean_obj=obj.x, sum_obj=obj.x.x, catch_size=obj.y, net_dist=obj.y.y)
  rename(`Mean\n Objective`=obj.x, `Sum\n Objective`=obj.x.x, `Catchment\n Size`=obj.y, 
         `Network\n Distance`=obj.y.y) #%>%
  #dplyr::select(-c("site1", "site2"))

panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y)
}




# maybe I should order sites in malinau_sites by malinau_obj1?
summary_comb(malinau_obj2, malinau_sites, malinau_obj1)
summary_comb(malinau_obj3, malinau_sites, malinau_obj1)
summary_comb(malinau_obj4, malinau_sites, malinau_obj1)
summary_comb(malinau_obj5, malinau_sites, malinau_obj1)


##################################################################################
# Now try for Langkat

langkat_sites <- health_sites[health_sites$regency == "LANGKAT",]

langkat_time <- stack(langkat_sites$time_catch) %>%
  trim()
langkat_obj <- crop(lulc_covs$objective, langkat_time)

langkat_catch_list = lapply(1:nlayers(langkat_time), function(x){
  which(!is.na(values(langkat_time[[x]])))
})

# now let's apply them: 
langkat_obj1 = sapply(1:nrow(langkat_sites), function(x){
  eval_obj_surface_mean(x, langkat_catch_list, langkat_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

# reordering langkat_sites by mean objective so that the sites in tables are sorted left to right ..
langkat_sites <- langkat_sites[order(langkat_obj1$obj, decreasing = TRUE),]
langkat_catch_list <- langkat_catch_list[order(langkat_obj1$obj, decreasing = TRUE)]
langkat_distance_matrix = as.matrix(dist(langkat_sites[,c("lon","lat")], upper=TRUE))

langkat_obj1 = sapply(1:nrow(langkat_sites), function(x){
  eval_obj_surface_mean(x, langkat_catch_list, langkat_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

langkat_obj1_sum = sapply(1:nrow(langkat_sites), function(x){
  eval_obj_surface_sum(x, langkat_catch_list, langkat_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

langkat_obj1_size = sapply(1:nrow(langkat_sites), function(x){
  eval_catch_size(x, langkat_catch_list, langkat_obj)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site=V2)

langkat_obj2 = combn(nrow(langkat_sites), 2, 
                     eval_obj_surface_mean, TRUE, langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

langkat_obj2_sum = combn(nrow(langkat_sites), 2,
                         eval_obj_surface_sum, TRUE, langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

langkat_obj2_size = combn(nrow(langkat_sites), 2,
                          eval_catch_size, TRUE, langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(desc(obj))

langkat_obj2_dist = combn(nrow(langkat_sites), 2,
                          eval_network_distance, TRUE, langkat_distance_matrix) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3) %>%
  arrange(obj)


# would be good to have some sort of a table with a graphical representation
# do this when I get to it !

langkat_obj3 = combn(nrow(langkat_sites), 3, eval_obj_surface_mean, TRUE,
                     langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4) %>%
  arrange(desc(obj))

langkat_obj4 = combn(nrow(langkat_sites), 4, eval_obj_surface_mean, TRUE,
                     langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5) %>%
  arrange(desc(obj))

langkat_obj5 = combn(nrow(langkat_sites), 5, eval_obj_surface_mean, TRUE,
                     langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5, site5=V6) %>%
  arrange(desc(obj))



# sum
langkat_obj3_sum = combn(nrow(langkat_sites), 3,
                         eval_obj_surface_sum, TRUE, langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4) %>%
  arrange(desc(obj))

langkat_obj4_sum = combn(nrow(langkat_sites), 4, eval_obj_surface_sum, TRUE,
                     langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5) %>%
  arrange(desc(obj))

langkat_obj5_sum = combn(nrow(langkat_sites), 5, eval_obj_surface_sum, TRUE,
                         langkat_catch_list, langkat_obj) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5, site5=V6) %>%
  arrange(desc(obj))

# langkat_obj6_sum = combn(nrow(langkat_sites), 6, eval_obj_surface_sum, TRUE,
#                          langkat_catch_list, langkat_obj) %>%
#   t() %>%
#   as.data.frame() %>%
#   rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5, site5=V6, site6=V7) %>%
#   arrange(desc(obj))
# write.csv(langkat_obj6_sum, "output/multiple_sites/langkat_sum_6.csv", row.names=FALSE)


# distance
langkat_obj3_dist = combn(nrow(langkat_sites), 3,
                         eval_network_distance, TRUE, langkat_distance_matrix) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4) %>%
  arrange(desc(obj))

{ptm <- proc.time()
langkat_obj4_dist = combn(nrow(langkat_sites), 4, eval_network_distance, TRUE,
                         langkat_distance_matrix) %>%
  t() %>%
  as.data.frame() %>%
  rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5) %>%
  arrange(desc(obj))
print(proc.time() - ptm)}

# {langkat_obj5_dist = combn(nrow(langkat_sites), 5, eval_network_distance, TRUE,
#                          langkat_distance_matrix) %>%
#   t() %>%
#   as.data.frame() %>%
#   rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5, site5=V6) %>%
#   arrange(desc(obj))
# write.csv(langkat_obj5_dist, "output/multiple_sites/langkat_dist_5.csv", row.names=FALSE)}

# {langkat_obj6_dist = combn(nrow(langkat_sites), 6, eval_network_distance, TRUE,
#                          langkat_distance_matrix) %>%
#   t() %>%
#   as.data.frame() %>%
#   rename(obj=V1, site1=V2, site2=V3, site3=V4, site4=V5, site5=V6, site6=V7) %>%
#   arrange(desc(obj))
# write.csv(langkat_obj6_dist, "output/multiple_sites/langkat_dist_6.csv", row.names=FALSE)}




langkat_pairsdf <- langkat_obj2 %>%
  left_join(langkat_obj2_size, by=c("site1", "site2")) %>%
  left_join(langkat_obj2_sum, by=c("site1", "site2")) %>%
  left_join(langkat_obj2_dist, by=c("site1", "site2")) %>%
  rename(`Mean\n Objective`=obj.x, `Sum\n Objective`=obj.x.x, `Catchment\n Size`=obj.y, 
         `Network\n Distance`=obj.y.y)

# maybe I should order sites in langkat_sites by langkat_obj1?
summary_comb(langkat_obj2, langkat_sites, langkat_obj1)
summary_comb(langkat_obj3, langkat_sites, langkat_obj1)
summary_comb(langkat_obj4, langkat_sites, langkat_obj1)
summary_comb(langkat_obj5, langkat_sites, langkat_obj1)



