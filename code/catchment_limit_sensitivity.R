# catchment limit sensitivity: try out a bunch of catchment limits and see how that changes results
# need to have read in health_sites, etc. in main

library(data.table)

# travel times and distances to look at:
test_dists = seq(10,100,5)
test_times = seq(50,150,10)
library("RColorBrewer")
pal=brewer.pal(8,"Set1")[c(1:5,7,8)]

# calculate mean objectives for a couple of different distances:
# dist_mean_objs = data.frame("x"=rep(1, nrow(health_sites)))
# for (i in 1:length(test_dists)){
#   message(paste0(i, "/", length(test_dists)))
#   catches = apply(health_sites[,c("lon","lat")], 1,
#                   getDistanceCatchment, 
#                   radius_m=1000*test_dists[i], 
#                   ras=lulc_covs$objective)
#   summ = lapply(catches,
#                 function(x){mean(values(x), na.rm = TRUE)})
#   dist_mean_objs[,i] = unlist(summ)
# }
# colnames(dist_mean_objs) = paste0(test_dists, "km")
# dist_mean_objs$name = health_sites$name
# write.csv(dist_mean_objs, "output/distance_limits_mean_obj.csv", row.names=FALSE)
dist_mean_objs = read.csv("output/distance_limits_mean_obj.csv")

time_mean_objs = data.frame("x"=rep(1, nrow(health_sites)))
for (i in 1:length(test_times)){
  message(paste0(i, "/", length(test_times)))
  catches = sapply(1:nrow(health_sites), getTimeCatchment,
                    points=health_points,
                    transition_surfaces=transition_surfaces,
                    island_groups=health_sites$island_group,
                    to_surface=lulc_covs$objective,
                    time_cap=100)
  summ = lapply(catches,
                function(x){mean(values(x), na.rm = TRUE)})
  time_mean_objs[,i] = unlist(summ)
}
colnames(time_mean_objs) = paste0(test_times, "km")
time_mean_objs$name = health_sites$name
write.csv(time_mean_objs, "output/travel_time_limits_mean_obj.csv", row.names=FALSE)
