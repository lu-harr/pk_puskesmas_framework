# figures!

library(wesanderson)

#TODO
# fix colour palette - purps is too grey (or change background to not be grey)




purps = c("#F0E6FC","#DAC7EE","#BC95E9","#B582EE","#9C59E5","#A253F6","#831FE8","#6711D1")
purps = brewer.pal(9,"Purples")
bg_col = "grey95"
# The trouble with these purps is there's some grey lookin things that are hard to distinguish from bg ..

getDistCatchUnmasked <- function(point, radius_m, ras){
  # function to give me a raster of pixels in the distance-based catchment
  point <- data.frame(lon=point[["lon"]], lat=point[["lat"]])
  pol <- point %>%
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(ras)) %>%
    st_transform(23830) %>%
    st_buffer(dist = radius_m) %>%
    st_transform(st_crs(ras))
  
  return(raster::mask(distanceFromPoints(trim(ras), point), pol))
  
}

LANGKAT = which(district_shapes$district_name == "LANGKAT")
MALINAU = which(district_shapes$district_name == "MALINAU")

# would also like to look at one of the sites in the north, that are more accessible ...

# catchments example: six panel, with example in Langkat and example in Malinau
langkat_ras <- district_shapes$ras[[LANGKAT]] %>%
  trim(padding=2)
lat_langkat <- mask(init(langkat_ras, "y"), langkat_ras)
lon_langkat <- mask(init(langkat_ras, "x"), langkat_ras)
langkat_sites <- health_sites[which(health_sites$regency == "LANGKAT"),]

# gives me the whole stretch map
stretch_langkat <- calc(stack(lon_langkat, lat_langkat), 
                   fun = function(x){which_min_euclid_distance(x[1], x[2],
                                                               langkat_sites[,c("lon","lat")])})
# cheating a bit here, as per usual
PICK_LANGKAT1 = 5
ind = which(langkat_sites[PICK_LANGKAT1, "name"] == health_sites$name)
langkat_eg1 <- list(getDistCatchUnmasked(langkat_sites[PICK_LANGKAT1,], 20000,
                                        langkat_ras),
                   getTimeCatchment(ind, health_points, transition_surfaces,
                                    health_sites$island_group, lulc_covs$objective,
                                    60, remask=FALSE) %>%
                     crop(langkat_ras),
                   langkat_sites$stretch_catch[[PICK_LANGKAT1]])

PICK_LANGKAT2 = 24
ind = which(langkat_sites[PICK_LANGKAT2, "name"] == health_sites$name)
# 24, 48 a possibility
langkat_eg2 <- list(getDistCatchUnmasked(langkat_sites[PICK_LANGKAT2,], 20000,
                                         langkat_ras),
                   getTimeCatchment(ind, health_points, transition_surfaces,
                                     health_sites$island_group, lulc_covs$objective,
                                     60, remask=FALSE) %>%
                                     crop(langkat_ras),
                   langkat_sites$stretch_catch[[PICK_LANGKAT2]])

# remove Bulungan
malinau_ras <- lulc_covs$human_pop %>%
  mask(district_shapes[MALINAU,]) %>%
  trim(padding=8)
# get rid of Pulau Sapi also ...
lat_malinau <- mask(init(malinau_ras, "y"), malinau_ras)
lon_malinau <- mask(init(malinau_ras, "x"), malinau_ras)
malinau_sites <- health_sites[which(health_sites$regency == "MALINAU"),]

# this ends up being malinau + bulungan ...
# decide later what I want to do about that
stretch_malinau <- calc(stack(lon_malinau, lat_malinau), 
                        fun = function(x){which_min_euclid_distance(x[1], x[2],
                                                                    malinau_sites[,c("lon","lat")])})

PICK_MALINAU1 = 6
ind = which(malinau_sites[PICK_MALINAU1, "name"] == health_sites$name)
malinau_eg1 <- list(getDistCatchUnmasked(malinau_sites[PICK_MALINAU1,], 20000,
                                        malinau_ras),
                   getTimeCatchment(ind, health_points, transition_surfaces,
                                    health_sites$island_group, lulc_covs$objective,
                                    60, remask=FALSE),
                   malinau_sites$stretch_catch[[PICK_MALINAU1]])

# there is something wrong with this ...
PICK_MALINAU2 = 1
ind = which(malinau_sites[PICK_MALINAU2, "name"] == health_sites$name)
malinau_eg2 <- list(getDistCatchUnmasked(malinau_sites[PICK_MALINAU2,], 20000,
                                         malinau_ras),
                    getTimeCatchment(ind, health_points, transition_surfaces,
                                     health_sites$island_group, lulc_covs$objective,
                                     100, remask=FALSE),
                    malinau_sites$stretch_catch[[PICK_MALINAU2]])


point_lwd = 5
border_lwd = 2

subfigure_label <- function(lab, loc){
  ext <- par()$usr
  text(x = ext[1] + loc[1]*(ext[2] - ext[1]), 
       y = ext[3] + loc[2]*(ext[4] - ext[3]),
       labels = lab, cex=1.4)
}

{png("figures/catchment_demos.png",
     height=2000,
     width=3200,
     pointsize=50)

par(mfrow=c(2,3), bty="n", mar=c(0,0,0,0), xpd=NA)
# I've used 20km
plot(langkat_ras, col=bg_col, legend=FALSE, axes=FALSE)
plot(merge(langkat_eg1[[1]], langkat_eg2[[1]]), col=purps, legend=FALSE, axes=FALSE, add=TRUE)
points(langkat_sites[c(PICK_LANGKAT1, PICK_LANGKAT2),c("lon","lat")], col="red", pch=4, lwd=point_lwd)
par(new=TRUE)
plot(st_geometry(district_shapes[LANGKAT,]), add=TRUE, lwd=border_lwd)
subfigure_label("(a)", c(0.1,0.9))

# and 60 min
plot(langkat_ras, col=bg_col, legend=FALSE, axes=FALSE)
par(new=TRUE)
plot(merge(langkat_eg1[[2]], langkat_eg2[[2]]), col=purps, legend=FALSE, axes=FALSE)
points(langkat_sites[PICK_LANGKAT1,c("lon","lat")], col="red", pch=4, lwd=point_lwd)
par(new=TRUE)
points(langkat_sites[PICK_LANGKAT2,c("lon","lat")], col="red", pch=4, lwd=point_lwd)
plot(st_geometry(district_shapes[LANGKAT,]), add=TRUE, lwd=border_lwd)
subfigure_label("(b)", c(0.1,0.9))

plot(langkat_ras, col=bg_col, legend=FALSE, axes=FALSE)
par(new=TRUE)
plot(crop(langkat_eg1[[3]], langkat_ras), col=purps, legend=FALSE, axes=FALSE)
par(new=TRUE)
plot(crop(langkat_eg2[[3]], langkat_ras), col=purps, legend=FALSE, axes=FALSE)
par(new=TRUE)
plot(rasterToPolygons(stretch_langkat, dissolve=TRUE), add=TRUE, 
     border="grey50", lwd=border_lwd)
points(langkat_sites[-c(PICK_LANGKAT1, PICK_LANGKAT2),c("lon","lat")], pch=4, lwd=point_lwd)
points(langkat_sites[c(PICK_LANGKAT1, PICK_LANGKAT2),c("lon","lat")], col="red", pch=4, lwd=point_lwd)
plot(st_geometry(district_shapes[LANGKAT,]), add=TRUE, lwd=border_lwd)
subfigure_label("(c)", c(0.1,0.9))

plot(malinau_ras, col=bg_col, legend=FALSE, axes=FALSE)
plot(merge(malinau_eg1[[1]], malinau_eg2[[1]]), col=purps, add=TRUE, legend=FALSE)
par(new=TRUE)
points(malinau_sites[c(PICK_MALINAU1, PICK_MALINAU2), c("lon","lat")], col="red", pch=4, lwd=point_lwd)
plot(st_geometry(district_shapes[MALINAU,]), add=TRUE, lwd=border_lwd)
subfigure_label("(d)", c(0.1,0.9))

# stuff is getting cut off :/
plot(malinau_ras, col=bg_col, legend=FALSE, axes=FALSE)
par(new=TRUE)
#plot(crop(malinau_eg1[[2]], malinau_ras), col=purps, legend=FALSE, axes=FALSE)
plot(merge(malinau_eg2[[2]], malinau_eg1[[2]]), col=purps, legend=FALSE, axes=FALSE, add=TRUE)
points(malinau_sites[c(PICK_MALINAU1, PICK_MALINAU2),c("lon","lat")], col="red", pch=4, lwd=point_lwd)
plot(st_geometry(district_shapes[MALINAU,]), add=TRUE, lwd=border_lwd)
subfigure_label("(e)", c(0.1,0.9))


plot(malinau_ras, col=bg_col, legend=FALSE, axes=FALSE)
# plot(crop(malinau_eg1[[3]], malinau_ras), col=purps, legend=FALSE, axes=FALSE)
# plot(crop(malinau_eg2[[3]], malinau_ras), col=purps, legend=FALSE, axes=FALSE)
plot(merge(malinau_eg1[[3]], malinau_eg2[[3]]), col=purps, legend=FALSE, add=TRUE)
plot(rasterToPolygons(stretch_malinau, dissolve=TRUE), add=TRUE, 
     border="grey50", lwd=border_lwd)
points(malinau_sites[-c(PICK_MALINAU1, PICK_MALINAU2),c("lon","lat")], pch=4, lwd=point_lwd)
points(malinau_sites[c(PICK_MALINAU1, PICK_MALINAU2),c("lon","lat")], col="red", pch=4, lwd=point_lwd)
plot(st_geometry(district_shapes[MALINAU,]), add=TRUE, lwd=border_lwd)
subfigure_label("(f)", c(0.1,0.9))

dev.off()}



# ... need unmasked version of getdistance catch
# not sure if I'm getting the best picture out of Malinau yet ..



###############################################################################
# MULTI-PANEL FIGURE OF "ECO-TYPE" SURFACES

###############################################################################
# "OBJECTIVE SURFACE" FIGURE

###############################################################################
# (EXAMPLE TABLE)

###############################################################################
# SENSITIVITY


