# script for catchment functions
# TODO
# - fix contiguity issue with travel time catchments! Once and for all!


###############################################################################
# DISTANCE (CIRCULAR) CATCHMENTS

getDistanceCatchment <- function(point, radius_m, ras){
  # function to give me a raster of pixels in the distance-based catchment
  point <- data.frame(lon=point[["lon"]], lat=point[["lat"]])
  pol <- point %>%
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(ras)) %>%
    st_transform(23830) %>%
    st_buffer(dist = radius_m) %>%
    st_transform(st_crs(ras))

  return(raster::mask(ras, pol))
  
}

# For example:
# tmp = getDistanceCatchment(health_sites[1,c("lon","lat")], 30000, lulc_covs$human_pop)
# tmp = apply(health_sites[1:2,c("lon","lat")], 1,
#       getDistanceCatchment, 
#       radius_m=30000, ras=lulc_covs$objective)

##############################################################################
# TRAVEL TIME CATCHMENTS

library(malariaAtlas)
library(gdistance)

idn_shp = malariaAtlas::getShp(country="Indonesia",
                               admin_level=c("admin1"))
kalimantan_shp = subset(idn_shp, idn_shp$name_1 %in% c("Kalimantan Barat",
                                                       "Kalimantan Selatan",
                                                       "Kalimantan Tengah",
                                                       "Kalimantan Timur",
                                                       "Kalimantan Utara"))
sumatera_shp = subset(idn_shp, idn_shp$name_1 %in% c("Aceh",
                                                     "Bengkulu",
                                                     "Jambi",
                                                     #"Kepulauan Riau",
                                                     "Lampung",
                                                     "Riau",
                                                     "Sumatera Barat",
                                                     "Sumatera Selatan",  
                                                     "Sumatera Utara"))


getRasterWrap = function(shp){
  # function computes transition surface for ith entry in list of shps
  friction = malariaAtlas::getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015", 
    shp = shp)
  Tr <- gdistance::transition(friction, function(x) 1/mean(x), 8)
  T.GC <- gdistance::geoCorrection(Tr)
  
  crs(T.GC) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  T.GC
}

# compute a transition surface for each regency
transition_surfaces <- list(kalimantan = getRasterWrap(kalimantan_shp),
                            sumatera = getRasterWrap(sumatera_shp))


getTimeCatchment = function(index, points, transition_surfaces, island_groups, 
                            to_surface, time_cap=100, remask=TRUE){
  # function finds catchment raster of cells < time_cap away from points[index,]
  message(index, island_groups[index])
  local_transition = transition_surfaces[[island_groups[index]]]
  out = gdistance::accCost(local_transition, points[index,])
  
  # note: cells cut BEFORE projecting back to 5km * 5km grid
  out[out > time_cap] = NA
  crs(out) = crs(to_surface)
  out = projectRaster(out, to_surface)
  if (remask == TRUE){
    out = mask(to_surface, out)
  }
  out
}

# get our health site location data ready
health_points = health_sites[, c("lon","lat")]
coordinates(health_points) <- ~ lon + lat
proj4string(health_points) <- proj4string(idn_shp)
health_sites$island_group = ifelse(health_sites$regency %in% c("MALINAU", "NUNUKAN"), "kalimantan", "sumatera")

# For example:
# tmp = getTimeCatchment(1, health_points, transition_surfaces, health_sites$island_group,
#                        lulc_covs$human_pop)


###############################################################################
# NEAREST-PHC CATCHMENTS

library(geosphere)

which_min_euclid_distance <- function(x, y, point.coords, allow_ties=TRUE){
  # for a given (x,y), which coord in point_coords is closest?
  dists = apply(point.coords, 1, distm, c(x,y), distHaversine)
  out = which(dists == min(dists))
  # tie breaking ..
  if (length(out) == 1){
    return(out)
  } else {
    return(out[1]) # not ideal
  }
}

# for example:
# tmp = which_min_euclid_distance(98.2, 3.8, cand_sites[,c("lon","lat")], allow_ties = FALSE)


kabkotNearestSiteCatchment <- function(ras,
                                     cand_phcs){
  # given a ras of sites to be allocated, and a df of candidate puskesmas,
  # give me a list of rasters with nearest site catchments
  all_sites <- rasterToPoints(ras)
  non_na_idx <- which(!is.na(values(ras)))
  pix <- apply(all_sites, 1, function(x){
    which_min_euclid_distance(x[1], x[2],
                              cand_phcs[,c("lon","lat")])
  })
  
  # now populate rasters
  flat_ras <- ras
  flat_ras[] <- NA
  lapply(1:nrow(cand_phcs), function(x){
    to_fill <- non_na_idx[which(pix == x)]
    flat_ras[to_fill] <- 1
    flat_ras
  })
  
}

# for example:
# tmp = wrapNearestSiteCatchment(pilot_shapes$ras[[which(pilot_shapes$KABKOT == "LANGKAT")]],
#                          cand_sites)


wrapNearestSiteCatchment <- function(regency_razzes,
                                     phc_df){
  outlst = vector(mode = "list", length = nrow(phc_df))
  
  for (kabkot in 1:nrow(regency_razzes)){
    ind <- which(phc_df$regency == regency_razzes$district_name[kabkot])
    outlst[ind] <- kabkotNearestSiteCatchment(regency_razzes$ras[[kabkot]],
                                              phc_df[ind,])
  }
  
  message(sum(sapply(outlst, is.null)))
  outlst
}








