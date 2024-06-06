subfigure_label = function(plot_region, x_displacement, y_displacement, label,
                           cex.label=1){
  # gives me an (a)
  text(plot_region[1] + (plot_region[2] - plot_region[1])*x_displacement, 
       plot_region[3] + (plot_region[4] - plot_region[3])*y_displacement,
       label, cex=cex.label)
}


my_simple_dist = function(pos1, pos2){
  # this was quicker than googling it :)))
  # for plotting only - distHaversine should be used where geographical 
  # distance is important
  return((pos1[1] - pos2[1])**2 + (pos1[2] - pos2[2])**2)
}


plot_sites_radius_pusk = function(ranked_sites, 
                                  labs, 
                                  centre, 
                                  # lablocs,
                                  label_radius,
                                  rank_cutoff=FALSE, 
                                  n_toadstools=30, 
                                  gap=0.95,
                                  testing=FALSE,
                                  line_col="red", lab_col="red", lab_cex=1.2){
  # possibly the most geniusest piece of code the author has ever written
  # and it's just for making my plot lables nice :')
  # would be nice to rig this for longer lables though mate - set incline to angle
  # and include clause to flip if angle in pi/2-3pi/2
  
  # ranked sites is a df of two columns: x, y
  #if (nrow(lablocs) > 0){
  #  outer_circle_coords = lablocs
  #  circle_coords = lablocs
    
  #} else {
  angles = (1:n_toadstools)*2*pi/n_toadstools # you know, like a fairy circle :)
  angles = angles[-c(which(angles >= 12*pi/13 & angles <= 14*pi/13))]
  angles = angles[-c(which(angles <= 1*pi/13 | angles >= 25*pi/13))]
  centre = unlist(centre) # coming from st_centroid()
  
  n_toadstools <- length(angles)
  
  # possible set of label positions
  outer_circle_coords = data.frame(x=(label_radius)*cos(angles)+centre[1], 
                                   y=(label_radius)*sin(angles)+centre[2])
  
  # possible set of end points for the lines we're putting in
  circle_coords = data.frame(x=(label_radius*gap)*cos(angles)+centre[1], 
                             y=(label_radius*gap)*sin(angles)+centre[2])
  
  if (testing == TRUE){
    points(circle_coords, col="blue")
    abline(h=centre[2])
    abline(v=centre[1])}
  
  taken_coords = c() # keep track of occupied positions in the circle
  for (i in 1:nrow(ranked_sites)){
    # find distance from point to all possible label locations
    dists_to_circle = my_simple_dist(circle_coords, c(ranked_sites[i,1],
                                                      ranked_sites[i,2]))
    # find closest label
    up = which(dists_to_circle == min(dists_to_circle))
    down = up
    
    # iterate around the circle if the label is already assigned
    while(up %in% taken_coords & down %in% taken_coords){
      down = ifelse(down <= 1, n_toadstools, down - 1)
      up = ifelse(up >= n_toadstools, 1, up + 1)
    }
    
    # pick the best outta the `up` and `down` options we're left with
    if(!(up %in% taken_coords) & !(down %in% taken_coords)){
      tmp = ifelse(dists_to_circle[up,1] > dists_to_circle[down,1], down, up)
    } else {
      tmp = ifelse(up %in% taken_coords, down, up)
    }
    
    outpoint = circle_coords[tmp,]
    outpoint_label = outer_circle_coords[tmp,]
    pos = ifelse(angles[tmp] > 7*pi/12 & angles[tmp] < 17*pi/12, 2, 
                 ifelse(angles[tmp] >= 5*pi/12 & angles[tmp] <= 7*pi/12, 3,
                        ifelse(angles[tmp] >= 17*pi/12 & angles[tmp] <= 19*pi/12, 1, 4))) # >:)
    taken_coords = c(taken_coords, tmp)
    
    # we're ready to add to the plot!
    text(outpoint_label, labels=labs[i], col=lab_col, cex=lab_cex, pos=pos) # needs a shift :)
    lines(c(ranked_sites[i,1], outpoint[1]), 
          c(ranked_sites[i,2], outpoint[2]), 
          col=line_col, lwd=3)
  }
}

################################################################################
# VERSION OF PLOT_SITES_RADIUS_PUSK() FOR COMBINATIONS OF TWO SITES :)
# CAN'T WAIT TO DO THIS AGAIN FOR MORE THAN THREE SITES :)
# USE A CONVEX HULL IF I DECIDE TO DO COMBS OF FOUR + !

label_two_combs = function(ranked_combs, locdf, 
                           centre, label_radius, n_toadstools=30, gap=0.95,
                           testing=FALSE,
                           line_col="black", lab_col="black", lab_cex=1, 
                           pal=viridis(nrow(ranked_combs)), labs=1:nrow(ranked_combs)){
  
  angles = (1:n_toadstools)*2*pi/n_toadstools # you know, like a fairy circle :)
  centre = unlist(centre) # coming from st_centroid()
  
  # possible set of label positions
  outer_circle_coords = data.frame(x=(label_radius)*cos(angles)+centre[1], 
                                   y=(label_radius)*sin(angles)+centre[2])
  
  # possible set of end points for the lines we're putting in
  circle_coords = data.frame(x=(label_radius*gap)*cos(angles)+centre[1], 
                             y=(label_radius*gap)*sin(angles)+centre[2])
  
  if (testing == TRUE){
    points(circle_coords)
  }
  
  taken_coords = c()
  for (i in nrow(ranked_combs):1){
    comb = ranked_combs[i,]
    midpoint = c((locdf$lon[comb$site1] + locdf$lon[comb$site2])/2,
                 (locdf$lat[comb$site1] + locdf$lat[comb$site2])/2)
    
    dists_to_circle = my_simple_dist(circle_coords, midpoint)
    
    # find closest label
    up = which(dists_to_circle == min(dists_to_circle))
    down = up
    
    # iterate around the circle if the label is already assigned
    while(up %in% taken_coords & down %in% taken_coords){
      down = ifelse(down <= 1, n_toadstools, down - 1)
      up = ifelse(up >= n_toadstools, 1, up + 1)
    }
    
    # pick the best outta the `up` and `down` options we're left with
    if(!(up %in% taken_coords) & !(down %in% taken_coords)){
      tmp = ifelse(dists_to_circle[up,1] > dists_to_circle[down,1], down, up)
    } else {
      tmp = ifelse(up %in% taken_coords, down, up)
    }
    
    outpoint = circle_coords[tmp,]
    outpoint_label = outer_circle_coords[tmp,]
    pos = ifelse(angles[tmp] > 7*pi/12 & angles[tmp] < 17*pi/12, 2, 
                 ifelse(angles[tmp] >= 5*pi/12 & angles[tmp] <= 7*pi/12, 3,
                        ifelse(angles[tmp] >= 17*pi/12 & angles[tmp] <= 19*pi/12, 1, 4))) # >:)
    taken_coords = c(taken_coords, tmp)
    
    # we're ready to add to the plot!
    text(outpoint_label, labels=labs[i], col=lab_col, cex=lab_cex, pos=pos)
    lines(c(locdf$lon[comb$site1], outpoint[1], locdf$lon[comb$site2]), 
          c(locdf$lat[comb$site1], outpoint[2], locdf$lat[comb$site2]), 
          col=pal[i], lwd=2)
    
  }
}


draw_me_an_arrow <- function(plotbox, from, to, 
                             width=0.1, proplen=0.8, headwidth=0.2,
                             col='#B1D668'){
  theta <- atan((from[2] - to[2]) / (from[1] - to[1]))
  len <- sqrt((from[1] - to[1])**2 + (from[2] - to[2])**2)
  
  side1 <- width * c(sin(theta), cos(theta))
  side2 <- proplen * len * c(cos(theta), sin(theta))
  side3 <- (headwidth - width) * c(sin(theta), cos(theta))
  
  xxx <- c(0, side1[1], side1[1] + side2[1],
           side1[1] + side2[1] + side3[1], 0, 
           -1*side1[1] + side2[1] - side3[1],
           -1*side1[1] + side2[1], -1*side1[1], 0)
  yyy <- c(0,-1*side1[2], -1*side1[2] + side2[2],
           -1*side1[2] + side2[2] - side3[2],
           0,side1[2] + side2[2] + side3[2],
           side1[2] + side2[2], side1[2], 0)
  
  
  if (from[1] <= to[1]){
    xxx = xxx + from[1]
    xxx[5] = to[1]
  } else {
    xxx = xxx*-1 + from[1]
    xxx[5] = to[1]
  }
  
  if (from[1] <= to[1]){
    yyy = yyy + from[2]
    yyy[5] = to[2]
  } else {
    yyy = yyy*-1 + from[2]
    yyy[5] = to[2]
  }
  
  xxx <- xxx * (plotbox[2] - plotbox[1]) + plotbox[1]
  yyy <- yyy * (plotbox[4] - plotbox[3]) + plotbox[3]
  
  polygon(xxx, yyy, col=alpha(col, 0.6), border=NA)
}

# to test it out:
# plot(0, xlim=c(0,1), ylim=c(0,1))
# draw_me_an_arrow(par()$usr, c(0.1,0.5), c(0.5, 0.4))
# draw_me_an_arrow(par()$usr, c(0.1,0.1), c(0.5, 0.5))
# draw_me_an_arrow(par()$usr, c(0.9,0.9), c(0.5, 0.5))
# draw_me_an_arrow(par()$usr, c(0.9,0.1), c(0.5,0.5))




