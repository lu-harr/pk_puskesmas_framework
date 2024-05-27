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
  #message(paste0(angles, sep=", "))
  angles = angles[-c(which(angles >= 12*pi/13 & angles <= 14*pi/13))]
  angles = angles[-c(which(angles <= 1*pi/13 | angles >= 25*pi/13))]
  #message(paste0(angles, sep=", "))
  centre = unlist(centre) # coming from st_centroid()
  
  n_toadstools <- length(angles)
  
  # possible set of label positions
  outer_circle_coords = data.frame(x=(label_radius)*cos(angles)+centre[1], 
                                   y=(label_radius)*sin(angles)+centre[2])
  
  # possible set of end points for the lines we're putting in
  circle_coords = data.frame(x=(label_radius*gap)*cos(angles)+centre[1], 
                             y=(label_radius*gap)*sin(angles)+centre[2])
  #}
  
  #message(length(angles), nrow(circle_coords))
  
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
    #message(paste0(pos, ",", tmp, ",", angles[tmp]))
    taken_coords = c(taken_coords, tmp)
    
    # we're ready to add to the plot!
    text(outpoint_label, labels=labs[i], col=lab_col, cex=lab_cex, pos=pos) # needs a shift :)
    lines(c(ranked_sites[i,1], outpoint[1]), 
          c(ranked_sites[i,2], outpoint[2]), 
          col=line_col, lwd=3)
  }
}


# for (tmp in 1:length(angles)){
#   pos = ifelse(angles[tmp] > 4*pi/9 & angles[tmp] < 13*pi/9, 2, 
#                ifelse(angles[tmp] >= 4*pi/9 & angles[tmp] <= 5*pi/9, 3,
#                       ifelse(angles[tmp] >= 13*pi/9 & angles[tmp] <= 14*pi/9, 1, 4))) # >:)
#   message(pos)
#   message(angles[tmp])
# }
