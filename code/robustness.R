# Check all outputs against old outputs!

library(terra)
# I'm not sure if I'm coping with ocean pixels right ...

puskesmas_ids <- cellFromXY(lulc_covs$human_pop, health_sites[,c("lon","lat")])

# find neighbouring pixels of all puskesmas
id_ras <- lulc_covs$human_pop
id_ras[] <- 1:ncell(id_ras)
neigh_mat <- focalWeight(lulc_covs$human_pop, 0.05, "rectangle") # queen's case
neigh_mat[!neigh_mat == 0] = 1
catchment_stack <- terra::focal(terra::rast(id_ras), neigh_mat, fun=c)
# a nrow(health_sites) * 9 matrix of neighbours:
catch_membership_mat <- values(catchment_stack, mat=TRUE)[puskesmas_ids,]
# should remove self site from neigh_mat

# chuck all those sites into a df
all_ids <- unique(c(puskesmas_ids, as.vector(catch_membership_mat)))
all_sites <- xyFromCell(lulc_covs$human_pop, all_ids) %>%
  as.data.frame() %>%
  rename(lon=x, lat=y) %>%
  mutate(island_group = ifelse(lon < 108, "sumatera",  "kalimantan"),
         rasid = all_ids)
all_sites <- all_sites[!is.na(raster::extract(lulc_covs$objective,
                                              all_sites[,c("lon","lat")])),]
# leave ocean sites in there and deal with them later ...

all_points <- all_sites[, c("lon","lat")]
coordinates(all_points) <- ~ lon + lat
proj4string(all_points) <- proj4string(idn_shp)

# find catchments for them all
all_sites$dist_catch = apply(all_sites, 1,
                              getDistanceCatchment,
                              radius_m=30000, ras=lulc_covs$objective)
all_sites$time_catch = sapply(1:nrow(all_sites), getTimeCatchment,
                               points=all_points,
                               transition_surfaces=transition_surfaces,
                               island_groups=all_sites$island_group,
                               to_surface=lulc_covs$objective,
                               time_cap=100)

# save our work!
# saveRDS(all_sites, file="output/peripheral_sites.rds")
all_sites <- readRDS("output/peripheral_sites.rds")

# this takes a while :/
# all_dist_summary = lapply(all_sites$dist_catch,
#                           catchment_summary_stats, 
#                           lulc_covs)

# saveRDS(all_dist_summary, file="output/peripheral_distance_summary.rds")
all_dist_summary <- readRDS("output/peripheral_distance_summary.rds")

# all_time_summary = lapply(all_sites$time_catch,
#                           catchment_summary_stats, 
#                           lulc_covs)
# saveRDS(all_time_summary, file="output/peripheral_time_summary.rds")
all_time_summary <- readRDS("output/peripheral_time_summary.rds")


library(data.table)
all_dist_summary = as.data.frame(data.table::rbindlist(all_dist_summary)) %>%
  mutate(rasid = all_sites$rasid)

all_time_summary = as.data.frame(data.table::rbindlist(all_time_summary)) %>%
  mutate(rasid = all_sites$rasid)

# check presence of NAs
dist_catch_mean_obj <- t(sapply(1:nrow(health_sites), function(x){
  row = all_dist_summary[which(all_dist_summary$rasid %in% catch_membership_mat[x,]), 
                         "mean_objective"]
  # then pad with NAs and add central site mean objective, central site catchment size
  row = c(row, rep(NA, ncol(catch_membership_mat) - length(row)),
          all_dist_summary[which(all_dist_summary$rasid == puskesmas_ids[x]), 
                           c("mean_objective", "npixel")])
  row
}))

time_catch_mean_obj <- t(sapply(1:nrow(health_sites), function(x){
  row = all_time_summary[which(all_time_summary$rasid %in% catch_membership_mat[x,]), 
                         "mean_objective"]
  # then pad with NAs and add central site mean objective, central site catchment size
  row = c(row, rep(NA, ncol(catch_membership_mat) - length(row)),
          all_time_summary[which(all_time_summary$rasid == puskesmas_ids[x]), 
                           c("mean_objective", "npixel")])
  row
}))

colnames(dist_catch_mean_obj)[1:9] = paste0("periph_", 1:9)
colnames(time_catch_mean_obj)[1:9] = paste0("periph_", 1:9)

# periph_dist_agg should be df with columns for all of the objective means, 
# column for site objective mean, column for site catchment size
periph_dist_agg <- health_sites %>%
  dplyr::select(c(name, lon, lat, regency)) %>%
  mutate(rasid = puskesmas_ids) %>%
  cbind(as.data.frame(dist_catch_mean_obj))
periph_time_agg <- health_sites %>%
  dplyr::select(c(name, lon, lat, regency)) %>%
  mutate(rasid = puskesmas_ids) %>%
  cbind(as.data.frame(time_catch_mean_obj))



site_range_plot3 = function(health_sel_sites, # relevant rows from health_sites
                            periph_sites_df,
                            main=""){
  
  # order all items by mean objective of central site
  ranked = order(unlist(periph_sites_df[,c("mean_objective")]))
  health_sel_sites = health_sel_sites[ranked,]
  periph_sites_df = periph_sites_df[ranked,]
  periph_sel_sites = periph_sites_df[, grep("periph", names(periph_sites_df))]
  central_catch_size = periph_sites_df[, "npixel"]
  central_sel_sites = periph_sites_df[, "mean_objective"]
  minx = min(unlist(periph_sel_sites), na.rm=TRUE)
  maxx = max(unlist(periph_sel_sites), na.rm=TRUE)
  
  # Main plot at left
  par(mar=c(5.1,10.1,4.1,2.1), fig=c(0,0.85,0,1))
  plot(0,0,
       ylim=c(1, nrow(health_sel_sites)), 
       xlim=c(minx, maxx),
       xlab="Mean objective value", ylab="",
       yaxt="n",
       main=main,
       cex.main=1.5,
       cex.lab=1.5)
  abline(h=1:nrow(health_sel_sites), col="grey80", lwd=2)
  abline(v=0.8433828, col="blue", lwd=3)
  text(x=0.8433828, y=14, 
       col="blue", labels=c("Mean value in \nobjective\n surface"), pos=4,cex=1.2)
  for(i in 1:nrow(health_sel_sites)){
    periph_means = unlist(periph_sel_sites[i,])
    periph_means = periph_means[!is.na(periph_means)]
    points(periph_means, rep(i, length(periph_means)), lwd=3)
    lines(c(min(periph_means, na.rm=TRUE), 
             max(periph_means, na.rm=TRUE)),
           rep(i, 2), lwd=3)
  }
  points(central_sel_sites, 1:nrow(health_sel_sites), col="red", pch=4, lwd=8)
  axis(2, at=1:nrow(health_sel_sites), labels=health_sel_sites$name, las=1, cex=1.5)
  mtext("Site name", 2, line=8.5, cex=1.5)
  
  # Insert barplot
  par(mar=c(5.1,4.1,4.1,2.1), fig=c(0.71,0.97,0,1), new=TRUE, xpd=TRUE)
  mp = barplot(unlist(central_catch_size), horiz=TRUE, col="grey90", 
               ylim=c(0.5, nrow(health_sel_sites)-0.5), space=0,
               xlab="", axes = FALSE)
  text(y = mp, x = central_catch_size, labels=central_catch_size, pos=4)
  mtext("Central site catchment size", 4, line=2.5, cex=1.5)
  return(mp)
}

districts = c("LANGKAT", "MALINAU")
for (district in 1:2){
  png(paste0("figures/adjacent_sites/adjacent_", tolower(districts[district]),"_dist.png"),
      width = 2000,
      height = c(1900,1500)[district],
      pointsize = 40)
  site_range_plot3(health_sel_sites = health_sites[health_sites$regency == districts[district],], 
                   periph_sites_df = periph_dist_agg[health_sites$regency == districts[district],],
                   "")#paste0(str_to_title(districts[district]), " - Distance catchments"))
  dev.off()
}


for (district in 1:2){
  png(paste0("figures/adjacent_sites/adjacent_", tolower(districts[district]),"_time.png"),
      width = 2000,
      height = c(1900,1500)[district],
      pointsize = 40)
  site_range_plot3(health_sel_sites = health_sites[health_sites$regency == districts[district],], 
                   periph_sites_df = periph_time_agg[health_sites$regency == districts[district],],
                   "")#paste0(str_to_title(districts[district]), " - Travel time catchments"))
  dev.off()
}

