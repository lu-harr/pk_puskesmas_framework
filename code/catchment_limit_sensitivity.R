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

# time_mean_objs = data.frame("x"=rep(1, nrow(health_sites)))
# for (i in 1:length(test_times)){
#   message(paste0(i, "/", length(test_times)))
#   catches = sapply(1:nrow(health_sites), 
#                    getTimeCatchment,
#                     points=health_points,
#                     transition_surfaces=transition_surfaces,
#                     island_groups=health_sites$island_group,
#                     to_surface=lulc_covs$objective,
#                     time_cap=test_times[i])
#   summ = lapply(catches,
#                 function(x){mean(values(x), na.rm = TRUE)})
#   time_mean_objs[,i] = unlist(summ)
# }
# colnames(time_mean_objs) = paste0(test_times, "mins")
# time_mean_objs$name = health_sites$name
# write.csv(time_mean_objs, "output/travel_time_limits_mean_obj.csv", row.names=FALSE)
time_mean_objs = read.csv("output/travel_time_limits_mean_obj.csv")

################################################################################
# Here come the plots

# raw plot of mean values:
matplot(t(dist_mean_objs), type='l')

# plot of ranks:
dist_ranks = apply(dist_mean_objs, 2, rank)
matplot(x=test_dists, y=t(dist_ranks[,1:19]), type='l',
        xlab="Catchment radius", ylab="Site rank") # need to reverse y axis
# lots of movement in the middle.


# regency-level plot of raw means:

pal=brewer.pal(8,"Set1")[c(1:5,7,8)]
#"#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#A65628" "#F781BF"

# get mean objective value of wider region
regional_means <- district_shapes %>%
  st_centroid() %>%
  st_geometry() %>%
  unlist() %>%
  matrix(ncol=2, byrow=TRUE) %>%
  data.frame() %>%
  rename(lon=X1, lat=X2) %>%
  apply(1, function(x){
    tmp = getDistanceCatchment(x, radius_m=150000, ras=lulc_covs$objective)
    mean(values(tmp), na.rm=TRUE)
    }
  )
  
# pal = viridis(7)[c(1,6,2,5,3,7,4)]
# "#440154FF" "#443A83FF" "#31688EFF" "#21908CFF" "#35B779FF" "#8FD744FF" "#FDE725FF"
# "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5" "#3288BD"
# "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4""#66C2A5" "#3288BD" "#5E4FA2"
# pal = brewer.pal(11, "Spectral")[c(1:4,8:11)]
# pal = turbo(7)
pal=brewer.pal(8,"Set1")[c(1:5,7,8)]
# pal=plasma(7)

{png("figures/catch_limit_sensitivity/catch_limit_dist_raw.png",
     width = 3000,
     height = 2500,
     pointsize = 40)
  par(mfrow=c(3,3))
  for(i in 1:nrow(district_shapes)){
    # could stick with idn-wide scale, or ...
    select_objs <- dist_mean_objs[health_sites$regency == district_shapes$district_name[i],]
    matplot(x=test_dists, 
            y=t(select_objs[, grep("km", names(select_objs))]), 
            type='l',
            xlab="Catchment radius (km)", ylab="Site mean objective value",
            main=str_to_title(district_shapes$district_name[i]),
            col=pal[i],
            lty=1, lwd=2,
            cex.main=1.5, cex.lab=1.4)
    abline(h=regional_means[i], col="grey50", lty=2, lwd=2)
  }
  
  matplot(x=test_dists, y=t(dist_mean_objs[, grep("km", names(select_objs))]), type='l',
          xlab="Catchment radius (km)", ylab="Site mean objective value",
          col=pal[as.numeric(droplevels(health_sites$regency))],
          lty=1, main="All Sites", lwd=2,
          cex.main=1.5, cex.lab=1.4) # need to reverse y axis
  
  plot(0, bty='n', xaxt='n', yaxt='n', type='n', xlab="", ylab="")
  legend("left", legend=str_to_title(district_shapes$district_name), fill=pal, cex=1.4)
  dev.off()}

{png("figures/catch_limit_sensitivity/catch_limit_time_raw.png",
     width = 3000,
     height = 2500,
     pointsize = 40)
  par(mfrow=c(3,3))
  for(i in 1:nrow(district_shapes)){
    # could stick with idn-wide scale, or ...
    select_objs <- time_mean_objs[health_sites$regency == district_shapes$district_name[i],]
    matplot(x=test_times, 
            y=t(select_objs[, grep("min", names(select_objs))]), 
            type='l',
            xlab="Catchment radius (mins)", ylab="Site mean objective value",
            main=str_to_title(district_shapes$district_name[i]),
            col=pal[i],
            lty=1, lwd=2,
            cex.main=1.5, cex.lab=1.4)
    abline(h=regional_means[i], col="grey50", lty=2, lwd=2)
    if (sum(is.na(select_objs)) > 0){
      nastarts <- select_objs[, grep("min", names(select_objs))] %>%
        filter(if_any(everything(), is.na)) %>%
        apply(1, function(x){c(sum(is.na(x)), x[!is.na(x)][1])})
      points(test_times[nastarts[1,] + 1], nastarts[2,], col=pal[i], lwd=2, bg="white",
             cex=1.4, pch=21)
    }
  }
  
  matplot(x=test_times, y=t(time_mean_objs[, grep("min", names(select_objs))]), type='l',
          xlab="Catchment radius (mins)", ylab="Site mean objective value",
          col=pal[as.numeric(droplevels(health_sites$regency))],
          lty=1, main="All Sites", lwd=2,
          cex.main=1.5, cex.lab=1.4) # need to reverse y axis
  
  nastarts <- time_mean_objs[, grep("min", names(select_objs))] %>%
    mutate(pcol = as.numeric(droplevels(health_sites$regency))) %>%
    filter(if_any(-pcol, is.na)) %>%
    apply(1, function(x){c(sum(is.na(x)), x[!is.na(x)][1], x[["pcol"]])})
  points(test_times[nastarts[1,] + 1], nastarts[2,], col=pal[nastarts[3,]], lwd=2, 
         pch=21, bg="white")
  
  plot(0, bty='n', xaxt='n', yaxt='n', type='n', xlab="", ylab="")
  legend("left", legend=str_to_title(district_shapes$district_name), fill=pal, cex=1.4)
  dev.off()}

################################################################################
# let's do a ranked version of the above

dist_ranks = apply(dist_mean_objs[grep("km", names(dist_mean_objs))], 2, function(x){rank(-x)})

{png("figures/catch_limit_sensitivity/catch_limit_dist_rank.png",
     width = 3000,
     height = 2500,
     pointsize = 40)
  par(mfrow=c(3,3))
  for(i in 1:nrow(district_shapes)){
    # could stick with idn-wide scale, or ...
    select_objs <- dist_mean_objs[health_sites$regency == district_shapes$district_name[i],]
    select_ranks = apply(select_objs[, grep("km", names(select_objs))], 2, function(x){rank(-x)})
    plot(0, type="n", xlim=range(test_dists), ylim=rev(range(select_ranks, na.rm=TRUE)),
         xlab="Catchment radius (km)", ylab="Rank by site mean objective value",
         main=str_to_title(district_shapes$district_name[i]))
    sapply(1:nrow(select_ranks), function(j){
        tmp = sum(!is.na(select_ranks[j,]))
        lines(test_dists[(length(test_dists)-tmp+1):length(test_dists)], 
              select_ranks[j,!is.na(select_ranks[j,])],
              col=pal[i],
              lwd=2)
    })
  }
  
  plot(0, type="n", xlim=range(test_dists), ylim=rev(range(dist_ranks, na.rm=TRUE)),
       xlab="Catchment radius (km)", ylab="Rank by site mean objective value",
       main=str_to_title(district_shapes$district_name[i]))
  
  sapply(1:nrow(dist_ranks), function(j){
    tmp = sum(!is.na(dist_ranks[j,]))
    lines(test_dists[(length(test_dists)-tmp+1):length(test_dists)], 
          dist_ranks[j,!is.na(dist_ranks[j,])],
          col=pal[as.numeric(droplevels(health_sites$regency))][j],
          lwd=2)
  })
  
  plot(0, bty='n', xaxt='n', yaxt='n', type='n', xlab="", ylab="")
  legend("left", legend=str_to_title(district_shapes$district_name), fill=pal, cex=1.4)
  dev.off()}


time_ranks = apply(time_mean_objs[grep("mins", names(time_mean_objs))], 2, 
                   function(x){rank(-x, na.last=TRUE)})
time_ranks[which(is.na(time_mean_objs))] = NA

{png("figures/catch_limit_sensitivity/catch_limit_time_rank.png",
     width = 3000,
     height = 2500,
     pointsize = 40)
  par(mfrow=c(3,3))
  for(i in 1:nrow(district_shapes)){
    # could stick with idn-wide scale, or ...
    select_objs <- time_mean_objs[health_sites$regency == district_shapes$district_name[i],]
    select_ranks = apply(select_objs[, grep("mins", names(select_objs))], 2, function(x){rank(-x, na.last=TRUE)})
    select_ranks[which(is.na(select_objs))] = NA
    plot(0, type="n", xlim=range(test_times), ylim=rev(range(select_ranks, na.rm=TRUE)),
         xlab="Catchment radius (mins)", ylab="Rank by site mean objective value",
         main=str_to_title(district_shapes$district_name[i]))
    sapply(1:nrow(select_ranks), function(j){
      tmp = sum(!is.na(select_ranks[j,]))
      lines(test_times[(length(test_times)-tmp+1):length(test_times)], 
            select_ranks[j,!is.na(select_ranks[j,])],
            col=pal[i],
            lwd=2)
    })
    # i've completely forgotten how tf this works
    if (sum(is.na(select_objs)) > 1){
      nastarts <- select_ranks[, grep("min", names(select_objs))] %>%
        as.data.frame() %>%
        mutate(pcol = rep(i, nrow(select_objs))) %>%
        filter(if_any(-pcol, is.na)) %>%
        apply(1, function(x){c(sum(is.na(x)), x[!is.na(x)][1], x[["pcol"]])})
      points(test_times[nastarts[1,] + 1], nastarts[2,], col=pal[nastarts[3,]], lwd=2, 
             pch=21, bg="white")
    }
    
  }
  
  plot(0, type="n", xlim=range(test_times), ylim=rev(range(time_ranks, na.rm=TRUE)),
       xlab="Catchment radius (mins)", ylab="Rank by site mean objective value",
       main=str_to_title(district_shapes$district_name[i]))
  
  sapply(1:nrow(time_ranks), function(j){
    tmp = sum(!is.na(time_ranks[j,]))
    lines(test_times[(length(test_times)-tmp+1):length(test_times)], 
          time_ranks[j,!is.na(time_ranks[j,])],
          col=pal[as.numeric(droplevels(health_sites$regency))][j],
          lwd=2)
  })
  
  nastarts <- time_ranks[, grep("min", names(time_mean_objs))] %>%
    as.data.frame() %>%
    mutate(pcol = as.numeric(droplevels(health_sites$regency))) %>%
    filter(if_any(-pcol, is.na)) %>%
    apply(1, function(x){c(sum(is.na(x)), x[!is.na(x)][1], x[["pcol"]])})
  points(test_times[nastarts[1,] + 1], nastarts[2,], col=pal[nastarts[3,]], lwd=2, 
         pch=21, bg="white")
  
  plot(0, bty='n', xaxt='n', yaxt='n', type='n', xlab="", ylab="")
  legend("left", legend=str_to_title(district_shapes$district_name), fill=pal, cex=1.4)
  dev.off()}

# something weird going on in Nunukan? Tied ranks? catchments must just be too big? Or are we on the island?

################################################################
# IS THIS THE FIGURE ON MY FIRST PK POSTER? YES!

i=7 # MALINAU

select_objs = time_mean_objs[which(health_sites$regency == "MALINAU"),
                             1:length(test_times)]
select_ranks = apply(select_objs, 2, function(x){rank(-x)})
#select_ranks[which(select_objs == 0)] = 0
select_ranks[which(is.na(select_objs))] = NA
# what do these lines do? need to order sites by rank at 100 minutes
tmp_names = time_mean_objs[which(health_sites$regency == pilot_shapes$kabkot_name[i]),
                           "name"]
tmp_names = tmp_names[order(select_ranks[,ncol(select_ranks)])]
select_ranks = select_ranks[order(select_ranks[,ncol(select_ranks)]),]

linelwd <- 4
pal <- viridis(100)[seq(25,93,length.out=nrow(select_objs))]

{png(paste0("figures/catch_limit_sensitivity/malinau_time_limits.png"),
    width = 2800,
    height = 1600,
    pointsize = 40)
par(mar=c(4.1,4.1,4.1,2.1), mfrow=c(1,2))

plot(0,type="n", xlim=range(test_times), ylim=range(select_objs, na.rm=TRUE),
     xlab="Catchment time limit (mins)", ylab="Site objective value",
     #main="Sensitivity to catchment limit\n(Malinau district)",
     col=pal[1], cex.lab=1.5, cex.main=1.5,lty=1, xaxt="n")
axis(1, test_times, test_times)
abline(v=100, lty=5, col="grey60", lwd=linelwd)
tmp = sapply(1:nrow(select_objs), function(j){
  tmp = sum(!is.na(select_objs[j,]))
  lines(test_times[(length(test_times)-tmp+1):length(test_times)], 
        select_objs[j,!is.na(select_objs[j,])],
        col=rev(pal)[j],
        lwd=linelwd)
  return(ifelse(tmp == length(test_times),
                NA,
                select_objs[j, max(which(is.na(select_objs[j,]))) + 1]))
})

plot(0,type="n", xlim=range(test_times), ylim=rev(range(select_ranks, na.rm=TRUE)),
     xlab="Catchment time limit (mins)", ylab="Site ranking",
     #main="Sensitivity to catchment limit\n(Malinau district)",
     col=pal[1], cex.lab=1.5, cex.main=1.5,lty=1, xaxt="n")
axis(1, test_times, test_times)
abline(v=100, lty=5, col="grey60", lwd=linelwd)
abline(h=0:15, col="grey80", lwd=1)
tmp = sapply(1:nrow(select_ranks), function(j){
  tmp = sum(!is.na(select_ranks[j,]))
  lines(test_times[(length(test_times)-tmp+1):length(test_times)], 
        select_ranks[j,!is.na(select_ranks[j,])],
        col=rev(pal)[j],
        lwd=linelwd)
  return(ifelse(tmp == length(test_times),
                NA,
                select_ranks[j, max(which(is.na(select_ranks[j,]))) + 1]))
})
points(c(60,100), c(7,6), col=pal[c(9,11)], cex=1.5, lwd=linelwd, pch=21, bg="white")



dev.off()}

i=7 # MALINAU RAW SENSITIVITY
pal=viridis(100)[seq(50,93,3)]
select_objs = time_mean_objs[which(health_sites$regency == pilot_shapes$kabkot_name[i]),
                             1:length(test_times)]
tmp_names = time_mean_objs[which(health_sites$regency == pilot_shapes$kabkot_name[i]),
                           "name"]
tmp_names = tmp_names[order(select_objs[,11])]
select_objs = select_objs[order(select_objs[,ncol(select_objs)]),]
select_objs[which(select_objs == 0, arr.ind = TRUE)] = NA
message(nrow(select_objs))
png(paste0(plotpath,"raw_time_limits.png"),
    width = 1600,
    height = 1600,
    pointsize = 40)
par(mar=c(4.1,4.1,4.1,2.1))
plot(0,type="n", xlim=range(test_times), 
     ylim=range(as.numeric(unlist(select_objs)), na.rm=TRUE),
     xlab="Catchment time limit (mins)", ylab="Site objective value",
     main="Sensitivity to catchment limit\n(Malinau district)",
     col=pal[1], cex.lab=1.5, cex.main=1.5,lty=1, xaxt="n")
axis(1, test_times, test_times)
abline(v=100, lty=5, col="grey60", lwd=8)
tmp = sapply(1:nrow(select_objs), function(j){
  tmp = sum(!is.na(select_objs[j,]))
  lines(test_times[(length(test_times)-tmp+1):length(test_times)], 
        select_objs[j,!is.na(select_objs[j,])],
        col=ifelse(j == 6, "red", pal[j]), lwd=8)
  return(ifelse(tmp == length(test_times),
                NA,
                select_objs[j, max(which(is.na(select_objs[j,]))) + 1]))
})



