# figures!

library(wesanderson)

#TODO
# fix colour palette - purps is too grey (or change background to not be grey)

purps = c("#F0E6FC","#DAC7EE","#BC95E9","#B582EE","#9C59E5","#6711D1")
purps = brewer.pal(9,"Purples")
bg_col = "grey95"
# The trouble with these purps is there's some grey lookin things that are hard to distinguish from bg ..

###############################################################################
# MAPS FIGURE
# would be good to get colour schemes the same across the whole figure

# world ras is now in sort_gis script

#"#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#A63603" "#7F2704"
#"#BC95E9"

"#F16913"

oranges <- brewer.pal(9, "Oranges")[3:6]

{png("figures/big_map.png",
     height=2800,
     width=3000,
     pointsize=30)
bg_col = "grey85"  
midfig <- c(0.155,0.845,0.31,0.69)
textcex <- 1.8
axcex <- 1.4
linedf <- data.frame(district = c("LANGKAT", "DAIRI", "TAPANULI TENGAH", "NUNUKAN",
                                  "PAKPAK BHARAT", "TAPANULI SELATAN", "MALINAU"),
                     x = c(0.15,0.4,0.65,0.85, 0.15,0.4,0.63),
                     y = c(rep(0.8, 4), rep(0.2, 3)))
ncolours <- 100
colbreaks <- seq(minValue(lulc_covs$brt_mean), maxValue(lulc_covs$brt_mean), 
                 length.out=ncolours + 1)
colbreaks[length(colbreaks)] <- colbreaks[length(colbreaks)] + 1e-7

par(fig=midfig, oma = c(0,0,0,0), mar=c(0,0,0,0))

plot(world_ras, col = bg_col, box=FALSE,
     #main = "Study area: Selected districts in Indonesia",
     #xlab = "Longitude", ylab = "Latitude",
     cex.main=1.8, cex.lab=1.3, legend=FALSE, legend.mar=0, axes=FALSE)
par(fig=midfig, oma = c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(lulc_covs$brt_mean, col=viridis(ncolours), breaks=colbreaks,
     legend.mar=0, legend=FALSE, box=FALSE, cex.axis=axcex)
par(fig=midfig, oma = c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(st_geometry(district_shapes), col="#FD8D3C", add=TRUE, border="grey20", lwd=4)
mtext("Longitude", 1, cex=textcex, line=2.5)
mtext("Latitude", 2, cex=textcex, line=2.5)

par(fig=c(0.155,0.9,0.31,0.69))
plot(lulc_covs$brt_mean, legend.only=TRUE, col=viridis(ncolours), breaks=colbreaks, 
     legend.shrink=0.75, legend.width=2,
     axis.args = list(at=seq(0,0.8, length.out=5), labels=seq(0,0.8, length.out=5), cex.axis=axcex),
     legend.args = list("Mean predicted relative risk", side=4, line=4, cex=textcex))
#mtext("Study area: Selected districts in Indonesia", 3, cex=textcex*1.1, line=2)

midusr = par()$usr # extremes of the user coordinates of the plotting region

par(oma=c(0,0,0,0), mar=c(1,1,4.1,1), new=TRUE, mfrow=c(4,4), mfg=c(1,1))

for (district in c("LANGKAT", "DAIRI", "TAPANULI TENGAH", "NUNUKAN")){
  plot(st_geometry(district_shapes[district_shapes$district_name == district,]))
  plot(trim(mask(lulc_covs$brt_mean, district_shapes[district_shapes$district_name == district,])),
       col=viridis(ncolours), breaks=colbreaks, add=TRUE, legend=FALSE)
  plot(st_geometry(district_shapes[district_shapes$district_name == district,]),
       add=TRUE, border="grey60", lwd=4)
  mtext(str_to_title(district), 3, cex=textcex, line=1.5)
}

par(oma=c(0,0,0,0), mar=c(4.1,1,1,1), new=TRUE, mfrow=c(4,4), mfg=c(4,1))

for (district in c("PAKPAK BHARAT", "TAPANULI SELATAN", "MALINAU")){
  plot(st_geometry(district_shapes[district_shapes$district_name == district,]))
  plot(trim(mask(lulc_covs$brt_mean, district_shapes[district_shapes$district_name == district,])),
       col=viridis(ncolours), breaks=colbreaks, add=TRUE, legend=FALSE)
  plot(st_geometry(district_shapes[district_shapes$district_name == district,]),
       add=TRUE, border="grey60", lwd=4)
  mtext(str_to_title(district), 1, cex=textcex, line=1.5)
}

par(oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE, mfrow=c(1,1))
plot(0, xlim=c(0,1), ylim=c(0,1), type="n", axes=FALSE)

# lines(c(0.3, 0.3), c(midfig[3], midfig[4]), col="red", lwd=4)
# lines(c(midfig[1], midfig[2]), c(0.3, 0.3), col="red", lwd=4)

# readjust margins because raster::plot() refuses to stay in its lane >:(
midfig <- c(0.13, 0.867, 0.295, 0.704)

# lines(c(0.2, 0.2), c(midfig[3], midfig[4]), col="blue", lwd=4)
# lines(c(midfig[1], midfig[2]), c(0.28, 0.28), col="blue", lwd=4)

for (district in 1:nrow(linedf)){
  cent <- district_shapes %>%
    filter(district_name == linedf$district[district]) %>%
    st_centroid() %>%
    st_geometry() %>%
    unlist()
  lines(c(linedf$x[district], ((cent[1] - midusr[1]) / (midusr[2] - midusr[1])) * (midfig[2] - midfig[1]) + midfig[1]),
        c(linedf$y[district], ((cent[2] - midusr[3]) / (midusr[4] - midusr[3])) * (midfig[4] - midfig[3]) + midfig[3]), 
        col="grey20", lwd=3)
}

dev.off()}



###############################################################################
# CATCHMENT DEMO FIGURE 

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

subfigure_label <- function(lab, loc, cex=2){
  ext <- par()$usr
  text(x = ext[1] + loc[1]*(ext[2] - ext[1]), 
       y = ext[3] + loc[2]*(ext[4] - ext[3]),
       labels = lab, cex=cex)
}

{png("figures/catchment_demos.png",
     #height=2000,
     height=1200,
     width=3200,
     pointsize=50)

# par(mfrow=c(2,3), bty="n", mar=c(0,0,0,0), xpd=NA)
par(mfrow=c(1,3), bty="n", mar=c(0,0,0,0), xpd=NA)
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

# plot(malinau_ras, col=bg_col, legend=FALSE, axes=FALSE)
# plot(merge(malinau_eg1[[1]], malinau_eg2[[1]]), col=purps, add=TRUE, legend=FALSE)
# par(new=TRUE)
# points(malinau_sites[c(PICK_MALINAU1, PICK_MALINAU2), c("lon","lat")], col="red", pch=4, lwd=point_lwd)
# plot(st_geometry(district_shapes[MALINAU,]), add=TRUE, lwd=border_lwd)
# subfigure_label("(d)", c(0.1,0.9))
# 
# # stuff is getting cut off :/
# plot(malinau_ras, col=bg_col, legend=FALSE, axes=FALSE)
# par(new=TRUE)
# #plot(crop(malinau_eg1[[2]], malinau_ras), col=purps, legend=FALSE, axes=FALSE)
# plot(merge(malinau_eg2[[2]], malinau_eg1[[2]]), col=purps, legend=FALSE, axes=FALSE, add=TRUE)
# points(malinau_sites[c(PICK_MALINAU1, PICK_MALINAU2),c("lon","lat")], col="red", pch=4, lwd=point_lwd)
# plot(st_geometry(district_shapes[MALINAU,]), add=TRUE, lwd=border_lwd)
# subfigure_label("(e)", c(0.1,0.9))
# 
# 
# plot(malinau_ras, col=bg_col, legend=FALSE, axes=FALSE)
# # plot(crop(malinau_eg1[[3]], malinau_ras), col=purps, legend=FALSE, axes=FALSE)
# # plot(crop(malinau_eg2[[3]], malinau_ras), col=purps, legend=FALSE, axes=FALSE)
# plot(merge(malinau_eg1[[3]], malinau_eg2[[3]]), col=purps, legend=FALSE, add=TRUE)
# plot(rasterToPolygons(stretch_malinau, dissolve=TRUE), add=TRUE, 
#      border="grey50", lwd=border_lwd)
# points(malinau_sites[-c(PICK_MALINAU1, PICK_MALINAU2),c("lon","lat")], pch=4, lwd=point_lwd)
# points(malinau_sites[c(PICK_MALINAU1, PICK_MALINAU2),c("lon","lat")], col="red", pch=4, lwd=point_lwd)
# plot(st_geometry(district_shapes[MALINAU,]), add=TRUE, lwd=border_lwd)
# subfigure_label("(f)", c(0.1,0.9))

dev.off()}

# ... need unmasked version of getdistance catch
# not sure if I'm getting the best picture out of Malinau yet ..

###############################################################################
# OIL PALM FIGURE !
# need to go back to work out where each of these surfaces were made?
pres_mode = raster("~/Desktop/knowlesi/oil_palm/oilpalm_pres_mode.tif") %>%
  crop(nw_idn_mask) %>%
  mask(nw_idn_mask)
values(pres_mode)[values(pres_mode) == 0] = NA
mode_only = raster("~/Desktop/knowlesi/oil_palm/oilpalm_mode_only.tif") %>%
  crop(nw_idn_mask) %>%
  mask(nw_idn_mask)
values(mode_only)[values(mode_only == 0)] = NA
# who the feck knows where this is ...
# danylo_all = raster('data/clean/raster/danylo5.tif')
# is this it?

# Might need to come back to this: (there's a hole in the raster)
# danylo_all = raster("~/Desktop/knowlesi/oil_palm/oilpalm_noresample.tif") %>%
#   crop(nw_idn_mask) %>%
#   mask(nw_idn_shp)
# writeRaster(danylo_all, "output/danylo_masked.tif")
# danylo_non_zero = danylo_all
# this takes a while ... maybe this is why I didn't do it like this the first time ...
# huh
danylo_all <- raster("output/danylo_masked.tif")
# grabbed this down from Spartan where I did the zeroing in blocks and then merged:
danylo_op <- raster("output/danylo_op_only.tif")

bg_col = "grey70"
pal = viridis(100)
zero_col = pal[1]
op_cols = pal[seq(50,100,length.out=37)] # pick some less dark cols from pal?
  
outer_extent = extent(lulc_covs)
inset_extent = c(98.4, 99.4,0.9,2.1)
xdisp <- 0.1
ydisp <- 0.1

inset_box <- function(ext=inset_extent){
  lines(c(ext[1], ext[2], ext[2], ext[1], ext[1]),
        c(ext[3], ext[3], ext[4], ext[4], ext[3]),
        col="red", lwd=4)
}

world_no_idn <- world_ras
world_no_idn[world_no_idn < 2] = NA

{png("figures/oilpalm_all.png",
    height=2900, width=2400, pointsize=55)

moma = c(6,0.1,0.1,0.1)
mmar = c(0,0,0,0)
mfig = c(0,0.7,2/3,1)
par(oma=moma, mar=mmar, fig=mfig)
plot(crop(world_ras, outer_extent), col=bg_col, legend=FALSE,
     xaxt="n", legend.mar=-2, yaxt="n")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(danylo_all, col=zero_col, legend=FALSE,
     xaxt="n", legend.mar=-2, yaxt="n",
     xlim=outer_extent[1:2], ylim=outer_extent[3:4])
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(danylo_op, col=op_cols, add=TRUE, legend=FALSE)
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(world_no_idn, col=bg_col, legend=FALSE, legend.mar=-2, xaxt="n", yaxt="n", add=TRUE)
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
subfigure_label(par()$usr, xdisp*0.5, ydisp, "(a)")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
inset_box()

mfig = c(0.7,1,2/3,1)
par(mar=mmar, fig=mfig, oma=moma, new=TRUE)
plot(crop(danylo_all, inset_extent), col=zero_col, legend=FALSE,
     legend.mar=-2, yaxt="n", xaxt="n")
par(mar=mmar, fig=mfig, oma=moma, new=TRUE)
subfigure_label(par()$usr, xdisp, ydisp, "(b)")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(crop(danylo_op, inset_extent), col=op_cols, legend=FALSE, legend.mar=-2,
     yaxt="n", xaxt="n")

mfig = c(0,0.7,1/3,2/3)
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(crop(world_ras, outer_extent), col=bg_col, legend=FALSE,
     xaxt="n", legend.mar=-2, yaxt="n")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(lulc_covs$human_pop, col=zero_col, legend=FALSE, 
     xaxt="n", legend.mar=-2, yaxt="n",
     xlim=outer_extent[1:2], ylim=outer_extent[3:4])
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(mode_only, col=op_cols, add=TRUE, legend=FALSE)
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
subfigure_label(par()$usr, xdisp*0.5, ydisp, "(c)")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
inset_box()

mfig = c(0.7,1,1/3,2/3)
par(mar=mmar, fig=mfig, oma=moma, new=TRUE)
plot(crop(lulc_covs$human_pop, inset_extent), col=zero_col, legend=FALSE, 
     legend.mar=-2, yaxt="n", xaxt="n")
par(mar=mmar, fig=mfig, oma=moma, new=TRUE)
subfigure_label(par()$usr, xdisp, ydisp, "(d)")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(crop(mode_only, inset_extent), col=op_cols, legend=FALSE, legend.mar=-2,
     yaxt="n", xaxt="n")

mfig = c(0,0.7,0,1/3)
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(crop(world_ras, outer_extent), col=bg_col, legend=FALSE,
     xaxt="n", legend.mar=-2, yaxt="n")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(lulc_covs$human_pop, col=zero_col, legend=FALSE, 
     xaxt="n", legend.mar=-2, yaxt="n",
     xlim=outer_extent[1:2], ylim=outer_extent[3:4])
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(pres_mode, col=op_cols, add=TRUE, legend=FALSE)
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
subfigure_label(par()$usr, xdisp*0.5, ydisp, "(e)")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
inset_box()

mfig = c(0.7,1,0,1/3)
par(mar=mmar, fig=mfig, oma=moma, new=TRUE)
plot(crop(lulc_covs$human_pop, inset_extent), col=zero_col, legend=FALSE, 
     legend.mar=-2, yaxt="n", xaxt="n")
par(mar=mmar, fig=mfig, oma=moma, new=TRUE)
subfigure_label(par()$usr, xdisp, ydisp, "(f)")
par(oma=moma, mar=mmar, fig=mfig, new=TRUE)
plot(crop(pres_mode, inset_extent), col=op_cols, legend=FALSE, legend.mar=-2,
     yaxt="n", xaxt="n")


par(mar=c(6,0.1,0.1,0.1), oma=c(0,0,0,0), fig=c(0,0.8,0,1))
plot(pres_mode+1980, legend.only=TRUE, horizontal=TRUE, col=op_cols,
     axis.args=list(cex.axis=0.8), legend.width=0.5, legend.shrink=0.7,
     legend.args=list(text="Year", side=3, line=0.5, cex=1.1))
par(mar=c(0.1,0.1,0.1,0.1), oma=c(0,0,0,0), fig=c(0,1,0,1), new=TRUE)
plot(0, axes=FALSE)
legend("bottomright", "No oil\npalm", xpd=NA, fill=viridis(100)[1], bty="n",
       inset=c(0.15,0.05), cex=1.1)

dev.off()}






