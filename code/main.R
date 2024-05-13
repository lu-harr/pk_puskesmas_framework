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
health_sites <- health_sites[-c(which(unlist(check_empty_catches) == TRUE)),]

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

# outpath = "output/tables/"
# for (regency in district_shapes$district_name){
#   summary_table_regency(tab = tmp_dist_summ,
#                         row_index = which(health_sites$regency == regency),
#                         path = paste0(outpath, tolower(gsub(" ", "_", regency)), "_dist_tab.html"),
#                         dataset_vec = health_sites$dataset,
#                         cap = paste0(str_to_title(regency), " puskesmas (distance catchments)"), 
#                         rank_col=2)
# }

# malinau_rank_dist = summary_table_regency(tab = tmp_dist_summ,
#                                           row_index = which(health_sites$regency == toupper("malinau")),
#                                           path = paste0(outpath, "malinau_dist_tab.html"),
#                                           dataset_vec = health_sites$dataset,
#                                           cap = "Malinau sites (distance catchments)", 
#                                           rank_col=3)


# summary_xtable(tab = tmp_time_summ,
#                row_index = which(health_sites$regency == toupper("malinau")),
#                dataset_vec = health_sites$dataset,
#                cap = "Malinau sites (time catchments)",
#                rank_col=3)

#############################################################
# script to do this by catchment type:
# sink("output/tables/dist.tex")
# for (regency in district_shapes$district_name){
#   summary_xtable(tab = tmp_dist_summ,
#                  row_index = which(health_sites$regency == regency),
#                  dataset_vec = health_sites$dataset,
#                  cap = paste0(str_to_title(regency), " sites (distance catchments)"), 
#                  rank_col=3)
# }
# sink()
# 
# sink("output/tables/time.tex")
# for (regency in district_shapes$district_name){
#   summary_xtable(tab = tmp_time_summ,
#                  row_index = which(health_sites$regency == regency),
#                  dataset_vec = health_sites$dataset,
#                  cap = paste0(str_to_title(regency), " sites (travel time catchments)"), 
#                  rank_col=3)
# }
# sink()
# 
# sink("output/tables/stretch.tex")
# for (regency in district_shapes$district_name){
#   summary_xtable(tab = tmp_stretch_summ,
#                  row_index = which(health_sites$regency == regency),
#                  dataset_vec = health_sites$dataset,
#                  cap = paste0(str_to_title(regency), " sites (``closest point'' catchments)"), 
#                  rank_col=3)
# }
# sink()
#############################################################

# script to do the same thing by district and wrap the figures in there too:

{sink("output/tables/all_tables_by_district.tex")
for (regency in district_shapes$district_name){
  cat(paste0("\\subsubsection{", str_to_title(regency), "}\n"))
  
  summary_xtable(tab = tmp_dist_summ,
                 row_index = which(health_sites$regency == regency),
                 dataset_vec = health_sites$dataset,
                 cap = paste0(str_to_title(regency), " sites (distance catchments, x km)"), 
                 lab = paste0("tab:", gsub(" ", "_", str_to_lower(regency)), "_", "dist"),
                 rank_col=3)

  summary_xtable(tab = tmp_time_summ,
                 row_index = which(health_sites$regency == regency),
                 dataset_vec = health_sites$dataset,
                 cap = paste0(str_to_title(regency), " sites (travel time catchments, x minutes)"), 
                 lab = paste0("tab:", gsub(" ", "_", str_to_lower(regency)), "_", "time"),
                 rank_col=3)
  
  summary_xtable(tab = tmp_stretch_summ,
                 row_index = which(health_sites$regency == regency),
                 dataset_vec = health_sites$dataset,
                 cap = paste0(str_to_title(regency), " sites (``closest point'' catchments)"), 
                 lab = paste0("tab:", gsub(" ", "_", str_to_lower(regency)), "_", "stretch"),
                 rank_col=3)
  
  formatted_regency = gsub(" ", "_", str_to_lower(regency))
  cat("\\begin{figure}\n")
  cat("\\centering\n")
  cat(paste0("\\includegraphics[width=\\textwidth]{figs/", formatted_regency,"_all_rank_map.png}\n"))
  cat(paste0("\\caption{Mapped best ranked sites in ", str_to_title(regency), " using catchment definitions of (a) distance-based 
  limits of x km (Table \\ref{tab:", formatted_regency,"_dist}); (b) travel time-based limits of x 
  minutes (Table \\ref{tab:", formatted_regency,"_time}); and (c) tesselated catchments (Table 
  \\ref{tab:", formatted_regency,"_stretch}).}\n"))
  cat(paste0("\\label{fig:maps_", formatted_regency,"}\n"))
  cat("\\end{figure}\n")
  cat("\\clearpage\n")
}
sink()}

# now for the maps
# read in cities locations
idn_cities = read.csv2("~/Desktop/knowlesi/data/raw/idn_cities.csv", header = TRUE, sep=",")
tmp_latlon = as.numeric(unlist(strsplit(as.vector(idn_cities$latlon), ",")))

idn_cities <- idn_cities %>%
  mutate(lat = as.numeric(tmp_latlon[seq(1,length(tmp_latlon),2)]),
         lon = as.numeric(tmp_latlon[seq(2,length(tmp_latlon),2)]))



dist_map_tweaks = data.frame(district = c("TAPANULI SELATAN","TAPANULI TENGAH", "DAIRI",
                                          "LANGKAT", "PAKPAK BHARAT", "MALINAU", "NUNUKAN"),
                       lon = c(99.258049, 98.8, 98.3, 98.25, 98.25, 115.8, 116.7),
                       lat = c(1.52, 2.15, 2.85,3.56, 2.55, 2.5, 4),
                       radius = c(0.68, 0.74, 0.4, 0.6, 0.25, 2.4,1.27),
                       gap = c(0.98, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99),
                       n_toadstools = c(30, 40, 40, 30, 40, 35, 30))

time_map_tweaks = data.frame(district = c("TAPANULI SELATAN","TAPANULI TENGAH", "DAIRI",
                                          "LANGKAT", "PAKPAK BHARAT", "MALINAU", "NUNUKAN"),
                             lon = c(99.258049, 99.1, 98.25, 98.22, 98.25, 115.8, 116.7),
                             lat = c(1.52, 2.2, 2.8,3.72, 2.55, 2.5, 4),
                             radius = c(0.68, 1, 0.4, 0.55, 0.25, 2.4,1.27),
                             gap = c(0.98, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99),
                             n_toadstools = c(30, 50, 40, 20, 40, 35, 30))

stretch_map_tweaks = data.frame(district = c("TAPANULI SELATAN","TAPANULI TENGAH", "DAIRI",
                                             "LANGKAT", "PAKPAK BHARAT", "MALINAU", "NUNUKAN"),
                                lon = c(99.258049, 99.2, 98.25, 98.22, 98.25, 115.7, 116.7),
                                lat = c(1.52, 2.2, 2.8,3.72, 2.55, 2.5, 4),
                                radius = c(0.68, 1.1, 0.4, 0.55, 0.25, 2.4,1.27),
                                gap = c(0.98, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99),
                                n_toadstools = c(30, 50, 40, 20, 40, 35, 30))

# have folded this into 1*3 figures for each district
# probably needs a subplot label?
for (regency in district_shapes$district_name){
  png(paste0("output/district_maps/", tolower(gsub(" ", "_", regency)), "_all_rank_map.png"),
      width = 3200,
      height = 2800,
      pointsize = 40)
  par(mar=c(6.1,7.1,6.1,7.1), oma=c(1.5,1.5,1.5,1.5), mfrow=c(2,2), xpd = NA)
  
  ranks = summary_xtable(tab = tmp_dist_summ,
                         row_index = which(health_sites$regency == regency),
                         dataset_vec = health_sites$dataset,
                         rank_col = 3,
                         rank_only = TRUE)
  ranked_map(all_points = health_sites[which(health_sites$regency == regency),],
             map_tweaks = dist_map_tweaks[which(dist_map_tweaks$district == regency),],
             ranks = ranks,
             regency = regency,
             catch_tag = "dist",
             outpath = "output/district_maps/")
  ranks = summary_xtable(tab = tmp_time_summ,
                         row_index = which(health_sites$regency == regency),
                         dataset_vec = health_sites$dataset,
                         rank_col = 3,
                         rank_only = TRUE)
  ranked_map(all_points = health_sites[which(health_sites$regency == regency),],
             map_tweaks = time_map_tweaks[which(time_map_tweaks$district == regency),],
             ranks = ranks,
             regency = regency,
             catch_tag = "time",
             outpath = "output/district_maps/")
  ranks = summary_xtable(tab = tmp_stretch_summ,
                         row_index = which(health_sites$regency == regency),
                         dataset_vec = health_sites$dataset,
                         rank_col = 3,
                         rank_only = TRUE)
  ranked_map(all_points = health_sites[which(health_sites$regency == regency),],
             map_tweaks = stretch_map_tweaks[which(stretch_map_tweaks$district == regency),],
             ranks = ranks,
             regency = regency,
             catch_tag = "stretch",
             outpath = "output/district_maps/")
  #mtext(str_to_title(regency), outer=TRUE, cex=2)
  dev.off()
}




