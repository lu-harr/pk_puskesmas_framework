# THIN OUT FUNCTIONAL/GENUINE DUPLICATES FROM PUSKESMAS SET

# identified some coords that needed fixing by running the loop below,
# but I shan't put the fixes into the original dataset ...

# here are some sites where the latlon was just not at all correct
ammendments = read.csv("data/health_sites_ammendments.csv", header=TRUE)
for (i in 1:nrow(ammendments)){
  health_sites[health_sites$name == ammendments$name[i], c("lon", "lat")] = ammendments[i, c("lon", "lat")]
}

# here are some sites that need re-landing
# should I do this for all? check other sites don't need relanding?

#library(seegSDM) # dependencies deprecated
nearestLand <- function (points, raster, max_distance) {
  # get nearest non_na cells (within a maximum distance) to a set of points
  # points can be anything extract accepts as the y argument
  # max_distance is in the map units if raster is projected
  # or metres otherwise
  
  # function to find nearest of a set of neighbours or return NA
  nearest <- function (lis, raster) {
    neighbours <- matrix(lis[[1]], ncol = 2)
    point <- lis[[2]]
    # neighbours is a two column matrix giving cell numbers and values
    land <- !is.na(neighbours[, 2])
    if (!any(land)) {
      # if there is no land, give up and return NA
      return (c(NA, NA))
    } else{
      # otherwise get the land cell coordinates
      coords <- xyFromCell(raster, neighbours[land, 1])
      
      if (nrow(coords) == 1) {
        # if there's only one, return it
        return (coords[1, ])
      }
      
      # otherwise calculate distances
      dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                      (coords[, 2] - point[2]) ^ 2)
      
      # and return the coordinates of the closest
      return (coords[which.min(dists), ])
    }
  }
  
  # extract cell values within max_distance of the points
  neighbour_list <- extract(raster, points,
                            buffer = max_distance,
                            cellnumbers = TRUE)
  
  # add the original point in there too
  neighbour_list <- lapply(1:nrow(points),
                           function(i) {
                             list(neighbours = neighbour_list[[i]],
                                  point = as.numeric(points[i, ]))
                           })
  
  return (t(sapply(neighbour_list, nearest, raster)))
}

# this doesn't match what happens below ...
raster::extract(lulc_covs$objective, health_sites[,c("lon","lat")])

health_sites[health_sites$name == "Pulo Pakkat", c("lon", "lat")] = 
  nearestLand(as.data.frame(health_sites[health_sites$name == "Pulo Pakkat", c("lon", "lat")]),
              district_shapes$ras[[which(district_shapes$district_name == "TAPANULI TENGAH")]], 5000)
  
health_sites[health_sites$name == "Kuta Buluh", c("lon", "lat")] = 
  nearestLand(as.data.frame(health_sites[health_sites$name == "Kuta Buluh", c("lon", "lat")]),
              district_shapes$ras[[which(district_shapes$district_name == "DAIRI")]], 5000)


# par(mar=c(1,1,1,1), bty="n", mfrow=c(3,3))
keep = c()
binary_stack <- stack(district_shapes$ras)
names(binary_stack) <- gsub(" ", "_", tolower(district_shapes$district_name))
# dont_keep1 = c()
# dont_keep2 = c()
for (i in 1:nrow(district_shapes)){
  #ras = trim(district_shapes$ras[[i]])
  values(binary_stack[[i]])[!is.na(values(binary_stack[[i]]))] = 0
  site_ind = which(health_sites$regency == district_shapes$district_name[i])
  sites = health_sites[site_ind,]
  if (nrow(sites) >= 1){
    for (j in 1:nrow(sites)){
      tmp = cellFromXY(binary_stack[[i]], sites[j, c("lon","lat")])
      if (is.na(binary_stack[[i]][tmp])){
        # points that have fallen off the raster
        message(paste0("OFF ", sites[j, "name"], j))
        # dont_keep2 = c(dont_keep2, site_ind[j])
      }
      else if (binary_stack[[i]][tmp] == 1){
        # points where there's already a site at that pixel
        message(paste0("dup ", sites[j, "name"], j))
        # dont_keep1 = c(dont_keep1, site_ind[j])
      } else {
        binary_stack[[i]][tmp] = 1
        keep = c(keep, site_ind[j])
      }
    }
    # plot(ras)
    # points(health_sites[keep, c("lon","lat")], pch=3)
    # points(health_sites[dont_keep1, c("lon","lat")], pch=3, col="red")
    # points(health_sites[dont_keep2, c("lon","lat")], pch=3, col="blue")
    
    # dont_keep1 = c()
    # dont_keep2 = c()
  }
}

health_sites = health_sites[keep,]


