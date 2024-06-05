# FIGURES: SURVEILLANCE DESIGNS

# surveillance designs with varying numbers of sites ..
pointcex_adjust = 0.8
pal=rev(viridis(12)[2:11])

{png("figures/multiple_sites/malinau_one_obj_2_5_comb.png",
  #    height=1800, width=1700, pointsize=40)
  # par(xpd=NA, mfrow=c(2,2), mar=c(1,1,1,1))
  height=2600, width=1700, pointsize=40)
  par(xpd=NA, mfrow=c(3,2), mar=c(1,1,1,1))
  set.seed(2)
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  select = 1:10
  label_two_combs(malinau_obj2[select,], malinau_sites,
                  centre=c(115.1,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  site_freqs <- malinau_obj2[1:10, grep("site", names(malinau_obj2))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(malinau_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(a)", 1.2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),)
  for (i in 1:10){
    comb <- malinau_obj3[11-i,]
    lines(jitter(c(malinau_sites$lon[comb$site1], malinau_sites$lon[comb$site2], 
                   malinau_sites$lon[comb$site3], malinau_sites$lon[comb$site1]), factor=0.3),
          jitter(c(malinau_sites$lat[comb$site1], malinau_sites$lat[comb$site2], 
                   malinau_sites$lat[comb$site3], malinau_sites$lat[comb$site1]), factor=0.5), 
          col=pal[11-i], lwd=2)
    
  }
  points(malinau_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  site_freqs <- malinau_obj3[1:10, grep("site", names(malinau_obj3))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(malinau_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(b)", 1.2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),)
  for (i in 1:10){
    comb <- malinau_obj4[11-i,2:5]
    comb_locs <- malinau_sites[unlist(comb),c("lon","lat")]
    comb_chull <- chull(comb_locs)
    lines(jitter(comb_locs[comb_chull[c(1:4,1)], "lon"], factor=0.5),
          jitter(comb_locs[comb_chull[c(1:4,1)], "lat"], factor=0.5), 
          col=pal[11-i], lwd=2)
    
  }
  points(malinau_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  site_freqs <- malinau_obj4[1:10, grep("site", names(malinau_obj4))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(malinau_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(c)", 1.2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),)
  for (i in 1:10){
    comb <- malinau_obj5[11-i,2:6]
    comb_locs <- malinau_sites[unlist(comb),c("lon","lat")]
    comb_chull <- chull(comb_locs)
    lines(jitter(comb_locs[comb_chull[c(1:5,1)], "lon"], factor=0.5),
          jitter(comb_locs[comb_chull[c(1:5,1)], "lat"], factor=0.5), 
          col=pal[11-i], lwd=2)
    
  }
  points(malinau_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  site_freqs <- malinau_obj5[1:10, grep("site", names(malinau_obj5))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(malinau_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(d)", 1.2)
  
  hbreaks <- seq(min(malinau_obj2$obj), max(malinau_obj2$obj), length.out=40)
  par(oma=c(5.1,2,55,25), mfrow=c(4,1), mar=c(0.1,4.1,0.1,0.1), new=TRUE, mfg=c(1,1))
  h2 <- hist(malinau_obj2$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  h3 <- hist(malinau_obj3$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  h4 <- hist(malinau_obj4$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  h5 <- hist(malinau_obj5$obj, breaks=hbreaks, cex.lab=1.2, cex.axis=0.7,
             xlab="Mean objective value", main="", ylab="")
  mtext("Frequency", 2, outer=TRUE, cex=0.8)
  
  par(mfrow=c(1,1), new=TRUE, oma=c(0,0,0,0), mar=c(3.1,4.1,36.1,10.1))
  plot(0, xlim=c(0,1), ylim=c(0,1), bty="n", axes=FALSE, xlab="", ylab="", type="n")
  text(rep(0.8), seq(0.1,0.9, length.out=4), paste(5:2, "sites"), cex=0.9)
  par(xpd=NA)
  subfigure_label(par()$usr,-0.1,0.9,"(e)", 0.8)
  
  dev.off()}

# surveillance designs of two sites with some different objectives ...

pal=rev(viridis(12)[2:11])

{png("figures/multiple_sites/malinau_one_obj_two_comb.png",
     height=1200, width=1100, pointsize=30)
  par(xpd=TRUE)
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select = 1:10
  label_two_combs(malinau_obj2[select,], malinau_sites,
                  centre=c(115.1,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  dev.off()}

{png("figures/multiple_sites/malinau_one_obj_two_comb.png",
     height=2200, width=1800, pointsize=35)
  
  # get ready for the most cursed use of oma in a bit
  pairs(pairsdf, oma=c(4,5,54,35), new=TRUE, cex.labels=1.2, cex.axis=0.8,
        #lower.panel=panel.hist, 
        cex=0.8)
  
  par(xpd=TRUE, mfrow=c(3,2), mar=c(1,1,1,1), oma=c(0,0,0,0), mfg=c(1,1), new=TRUE)
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select = 1:10
  par(xpd=NA)
  label_two_combs(malinau_obj2[select,], malinau_sites,
                  centre=c(115.1,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(a)",1.3)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(malinau_obj2_sum[select,], malinau_sites,
                  centre=c(115.5,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(b)",1.3)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(malinau_obj2_size[select,], malinau_sites,
                  centre=c(115.5,2.8),
                  label_radius=2,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(c)",1.3)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]),
  )
  points(malinau_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(malinau_obj2_dist[select,], malinau_sites,
                  centre=c(115.5,2.8),
                  label_radius=1.9,
                  n_toadstools = 60,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(d)",1.3)
  
  plot(0, type="n", axes=FALSE)
  subfigure_label(par()$usr,0.1,1,"(e)",1.3)
  
  dev.off()}

{png("figures/malinau_one_obj_pairs.png",
     height=1000, width=1000, pointsize=30)
  pairs(pairsdf)
  dev.off()}


############################################################################################


{png("figures/multiple_sites/langkat_one_obj_2_5_comb.png",
     #    height=1800, width=1700, pointsize=40)
     # par(xpd=NA, mfrow=c(2,2), mar=c(1,1,1,1))
     height=2200, width=1800, pointsize=35)
  pal=rev(viridis(12)[2:11])
  par(xpd=NA, mfrow=c(3,2), mar=c(1,1,1,1))
  set.seed(2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),
  )
  points(langkat_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  select = 1:10
  par(xpd=NA)
  label_two_combs(langkat_obj2[select,], langkat_sites,
                  centre=c(98.22, 3.6),
                  label_radius=0.47,
                  n_toadstools = 35,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  site_freqs <- langkat_obj2[1:10, grep("site", names(langkat_obj2))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(langkat_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(a)", 1.2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),)
  for (i in 1:10){
    comb <- langkat_obj3[11-i,]
    lines(jitter(c(langkat_sites$lon[comb$site1], langkat_sites$lon[comb$site2], 
                   langkat_sites$lon[comb$site3], langkat_sites$lon[comb$site1]), factor=0.3),
          jitter(c(langkat_sites$lat[comb$site1], langkat_sites$lat[comb$site2], 
                   langkat_sites$lat[comb$site3], langkat_sites$lat[comb$site1]), factor=0.5), 
          col=pal[11-i], lwd=2)
    
  }
  points(langkat_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  site_freqs <- langkat_obj3[1:10, grep("site", names(langkat_obj3))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(langkat_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(b)", 1.2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),)
  for (i in 1:10){
    comb <- langkat_obj4[11-i,2:5]
    comb_locs <- langkat_sites[unlist(comb),c("lon","lat")]
    comb_chull <- chull(comb_locs)
    lines(jitter(comb_locs[comb_chull[c(1:4,1)], "lon"], factor=0.5),
          jitter(comb_locs[comb_chull[c(1:4,1)], "lat"], factor=0.5), 
          col=pal[11-i], lwd=2)
    
  }
  points(langkat_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  site_freqs <- langkat_obj4[1:10, grep("site", names(langkat_obj4))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(langkat_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(c)", 1.2)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),)
  for (i in 1:10){
    comb <- langkat_obj5[11-i,2:6]
    comb_locs <- langkat_sites[unlist(comb),c("lon","lat")]
    comb_chull <- chull(comb_locs)
    lines(jitter(comb_locs[comb_chull[c(1:5,1)], "lon"], factor=0.5),
          jitter(comb_locs[comb_chull[c(1:5,1)], "lat"], factor=0.5), 
          col=pal[11-i], lwd=2)
    
  }
  points(langkat_sites[,c("lon","lat")], pch=4, lwd=2, cex=0.8)
  site_freqs <- langkat_obj5[1:10, grep("site", names(langkat_obj5))] %>%
    unlist() %>%
    table() %>%
    as.data.frame
  points(langkat_sites[as.numeric(levels(site_freqs[,1])),c("lon","lat")],
         cex=site_freqs$Freq*pointcex_adjust, lwd=2)
  subfigure_label(par()$usr,0.1,0.9,"(d)", 1.2)
  
  hbreaks <- seq(min(langkat_obj2$obj), max(langkat_obj2$obj), length.out=40)
  par(oma=c(5.1,2,55,25), mfrow=c(4,1), mar=c(0.1,4.1,0.1,0.1), new=TRUE, mfg=c(1,1))
  h2 <- hist(langkat_obj2$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  h3 <- hist(langkat_obj3$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  h4 <- hist(langkat_obj4$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  h5 <- hist(langkat_obj5$obj, breaks=hbreaks, cex.lab=1.2, cex.axis=0.7,
             xlab="Mean objective value", main="", ylab="")
  mtext("Frequency", 2, outer=TRUE, cex=0.8)
  
  par(mfrow=c(1,1), new=TRUE, oma=c(0,0,0,0), mar=c(3.1,4.1,36.1,10.1))
  plot(0, xlim=c(0,1), ylim=c(0,1), bty="n", axes=FALSE, xlab="", ylab="", type="n")
  text(rep(0.8), seq(0.1,0.9, length.out=4), paste(5:2, "sites"), cex=0.9)
  par(xpd=NA)
  subfigure_label(par()$usr,-0.1,0.9,"(e)", 0.8)
  
  dev.off()}

# surveillance designs of two sites with some different objectives ...

{png("figures/multiple_sites/langkat_one_obj_two_comb.png",
     height=2200, width=1800, pointsize=35)
  
  # get ready for the most cursed use of oma in a bit
  pairs(pairsdf, oma=c(4,5,54,35), new=TRUE, cex.labels=1.2, cex.axis=0.8,
        #lower.panel=panel.hist, 
        cex=0.8)
  
  par(xpd=TRUE, mfrow=c(3,2), mar=c(1,1,1,1), oma=c(0,0,0,0), mfg=c(1,1), new=TRUE)
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),
  )
  points(langkat_sites[,c("lon","lat")], pch=4)
  select = 1:10
  par(xpd=NA)
  label_two_combs(langkat_obj2[select,], langkat_sites,
                  centre=c(98.22, 3.6),
                  label_radius=0.47,
                  n_toadstools = 35,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(a)",1.3)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),
  )
  points(langkat_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(langkat_obj2_sum[select,], langkat_sites,
                  centre=c(98.22, 3.8),
                  label_radius=0.47,
                  n_toadstools = 45,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(b)",1.3)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),
  )
  points(langkat_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(langkat_obj2_size[select,], langkat_sites,
                  centre=c(98.22, 3.8),
                  label_radius=0.5,
                  n_toadstools = 45,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(c)",1.3)
  
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]),
  )
  points(langkat_sites[,c("lon","lat")], pch=4)
  select=1:10
  label_two_combs(langkat_obj2_dist[select,], langkat_sites,
                  centre=c(98.22, 3.7),
                  label_radius=0.55,
                  n_toadstools = 45,
                  gap=0.99,
                  pal=pal[select],
                  labs=select)
  subfigure_label(par()$usr,0.1,0.9,"(d)",1.3)
  
  plot(0, type="n", axes=FALSE)
  subfigure_label(par()$usr,0.1,1,"(e)",1.3)
  
  dev.off()}

{png("figures/langkat_one_obj_pairs.png",
     height=1000, width=1000, pointsize=30)
  pairs(pairsdf)
  dev.off()}
