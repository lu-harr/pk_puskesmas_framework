# FIGURES: SURVEILLANCE DESIGNS

# surveillance designs with varying numbers of sites ..
pointcex_adjust = 0.8
pal=rev(viridis(12)[2:11])

{png("figures/multiple_sites/malinau_one_obj_2_5_comb.png",
  height=2400, width=1700, pointsize=40)
  par(xpd=NA, mfrow=c(2,2), mar=c(1,1,1,1), oma=c(17,0,0,0))
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
  par(oma=c(5.1,12,55,12), mfrow=c(4,1), mar=c(0.1,2.1,0.1,0.1), new=TRUE, mfg=c(1,1), xpd=TRUE)
  hist(malinau_obj2$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  abline(v=malinau_obj2$obj[1:10], col=pal, lwd=2)
  hist(malinau_obj3$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  abline(v=malinau_obj3$obj[1:10], col=pal, lwd=2)
  hist(malinau_obj4$obj, breaks=hbreaks, xaxt="n", main="", cex.axis=0.7, xlab="", ylab="")
  abline(v=malinau_obj4$obj[1:10], col=pal, lwd=2)
  hist(malinau_obj5$obj, breaks=hbreaks, cex.lab=1.2, cex.axis=0.7,
        main="", ylab="")
  abline(v=malinau_obj5$obj[1:10], col=pal, lwd=2)
  par(xpd=NA)
  mtext("Frequency", 2, outer=TRUE, cex=0.8)
  mtext("Mean objective value", 1, outer=TRUE, cex=0.8, line=2)
  
  par(mfrow=c(1,1), new=TRUE, oma=c(3.1,1,36,1), mar=c(0.1,2.1,0.1,2.1))
  plot(0, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", type="n", axes=FALSE)
  text(rep(0.9), seq(0.1,0.9, length.out=4), paste(5:2, "sites"), cex=0.9)
  par(xpd=NA)
  subfigure_label(par()$usr,0.18,1.02,"(e)", 1)
  
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
  pairs(dplyr::select(malinau_pairsdf, -c("site1", "site2")), oma=c(4,5,54,35), new=TRUE, cex.labels=1.2, cex.axis=0.8,
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
pal=rev(viridis(12)[2:11])

{png("figures/multiple_sites/langkat_one_obj_two_comb.png",
     height=2200, width=1800, pointsize=35)
  
  # get ready for the most cursed use of oma in a bit
  pairs(dplyr::select(langkat_pairsdf, -c("site1","site2"))[,c(1,3,2,4)], oma=c(4,5,54,35), new=TRUE, cex.labels=1.2, cex.axis=0.8,
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
  
  par(oma=c(3.1,33,55,7), mfrow=c(4,1), mar=c(2.1,4.1,0.1,0.1), new=TRUE, mfg=c(1,1))
  par(xpd=FALSE)
  hist(langkat_obj2$obj, breaks=seq(min(langkat_obj2$obj), max(langkat_obj2$obj), length.out=40), 
        main="", cex.axis=0.7, xlab="", ylab="")
  abline(v=langkat_obj2$obj[1:10], col=pal, lwd=2)
  hist(langkat_obj2_sum$obj,breaks=seq(min(langkat_obj2_sum$obj), max(langkat_obj2_sum$obj), length.out=40), 
       main="", cex.axis=0.7, xlab="", ylab="")
  abline(v=langkat_obj2_sum$obj[1:10], col=pal, lwd=2)
  hist(langkat_obj2_size$obj,breaks=seq(min(langkat_obj2_size$obj), max(langkat_obj2_size$obj), length.out=40), 
             main="", cex.axis=0.7, xlab="", ylab="")
  abline(v=langkat_obj2_size$obj[1:10], col=pal, lwd=2)
  hist(langkat_obj2_dist$obj, breaks=seq(min(langkat_obj2_dist$obj), max(langkat_obj2_dist$obj), length.out=40), 
       cex.axis=0.7, xlab="", main="", ylab="")
  abline(v=langkat_obj2_dist$obj[1:10], col=pal, lwd=2)
  par(xpd=NA)
  mtext("Objective Value", 1, outer=TRUE, cex=0.8, line=0.5)
  mtext("Frequency", 2, outer=TRUE, cex=0.8, line=-1.5)
  
  par(mfrow=c(1,1), new=TRUE, oma=c(0,0,0,0), mar=c(3.1,25.5,34.1,2.1))
  plot(0, xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", type="n")
  text(rep(1), seq(0.1,0.83, length.out=4), names(pairsdf)[c(4,2,3,1)], cex=0.9)
  par(xpd=NA)
  subfigure_label(par()$usr,-0.1,0.9,"(f)", 0.9)
  
  dev.off()}

{png("figures/langkat_one_obj_pairs.png",
     height=1000, width=1000, pointsize=30)
  pairs(pairsdf)
  dev.off()}



# let's do a pareto figure?
# pairs?
library(rPref)
head(langkat_pairsdf)

scatter_panel <- function(x, y){
  plot(x,y,col="grey80")
  message(x)
  pareto <- psel(as.data.frame(x=x,y=y), high("x")*high("y"))
}

# custom pairs ... I ain't got time to work out how the real think works (eyerolls are costly)
# think about colour ...
# I like spectral but the middle yellow is too light ...
# paste(paste0("'", brewer.pal(11,"Spectral"), "'"), collapse=",")
spectral <- c('#9E0142','#D53E4F','#F46D43','#FDAE61',"#F8DA56",#"#FEFC60",#'#FEE08B',#'#FFFFBF',
              #'#E6F598',
              '#B1D668','#66C2A5','#3288BD','#5E4FA2')

pal <- colorRampPalette(spectral)
pal <- colorRampPalette(c("#D4E9F6", iddu(4)[2:4], "#F7D3E2"))
pal <- colorRampPalette(iddu(4)[2:4])

{png("figures/multiple_sites/langkat_multi_obj_two_sites.png",
    height=2300, width=2100, pointsize=35)

objnames <- subset(names(langkat_pairsdf), !grepl("site", names(langkat_pairsdf)))
gridsize <- length(objnames)

par(mfrow=rep(gridsize, 2), mar=rep(1.5,4), xpd=NA)
for (i in 1:gridsize){
  for (j in 1:gridsize){
    if (i == j){
      plot(0, type="n", axes=FALSE, xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="")
      text(0, 0, objnames[i], cex=2)
    } else if (i < j){
      par(mar=rep(1.5,4))
      # scatter plots
      subj <- data.frame(x=langkat_pairsdf[,objnames[i]], 
                         y=langkat_pairsdf[,objnames[j]])
      if (j == 4){
        # minimise network distance .. assuming network distance is fourth column (and doesn't end up in i)
        plot(subj$y, subj$x, xlab="", ylab="", col="grey30", type="n", xlim=rev(range(subj$y)))
        pareto <- psel(subj, high("x")*low("y"))
      } else {
        plot(subj$y, subj$x, xlab="", ylab="", col="grey30", type="n")
        pareto <- psel(subj, high("x")*high("y"))
      }
      draw_me_an_arrow(par()$usr, c(0.1,0.1), c(0.3,0.3), 
                       width=0.05, headwidth=0.12, proplen = 0.7)
      pareto <- pareto[order(pareto$x),]
      points(subj$y, subj$x, col="grey30",)
      lines(pareto$y, pareto$x, col='#B1D668', lwd=2)
      points(pareto$y, pareto$x, pch=21, bg=pal(nrow(pareto)), cex=1.5)
      
    } else if (i > j){
      par(mar=rep(0.2,4), bty="o", xpd=NA)
      # maps
      plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]))
      bb <- par()$usr
      xadj <- 0.03
      yadj <- 0.015
      lines(c(bb[1]+xadj, bb[2]-xadj, bb[2]-xadj, bb[1]+xadj, bb[1]+xadj), 
            c(bb[3]+yadj, bb[3]+yadj, bb[4]-yadj, bb[4]-yadj, bb[3]+yadj))
      if (i == 4){
        # minimise network distance .. assuming network distance is fourth column 
        # (and doesn't end up in i)
        pareto <- psel(data.frame(x=langkat_pairsdf[,objnames[i]],
                                  y=langkat_pairsdf[,objnames[j]],
                                  site1=langkat_pairsdf[,"site1"],
                                  site2=langkat_pairsdf[,"site2"]),
                       low("x")*high("y"))
        pareto <- pareto[order(pareto$x),]
      } else {
        pareto <- psel(data.frame(x=langkat_pairsdf[,objnames[i]],
                                  y=langkat_pairsdf[,objnames[j]],
                                  site1=langkat_pairsdf[,"site1"],
                                  site2=langkat_pairsdf[,"site2"]),
                       high("x")*high("y"))
        pareto <- pareto[order(pareto$y),]
      }
      
      points(langkat_sites[,c("lon","lat")], pch=4, col="grey30")
      tmppal <- pal(nrow(pareto))
      sapply(1:nrow(pareto), function(x){
        lines(langkat_sites[unlist(pareto[x,c("site1", "site2")]), c("lon","lat")],
              col=tmppal[x], lwd=3)
      })
      ptcex <- table(unlist(pareto[,c("site1","site2")]))
      points(langkat_sites[as.numeric(names(ptcex)), c("lon", "lat")],
             cex=ptcex*1.2)
    }
  }
}
dev.off()}

###############################################################################
# MORE SITES IN THE DESIGNS !

scatter_panel <- function(sites, xlab="", ylab="", main="", 
                          lims=c(min(sites$obj.x), max(sites$obj.x), min(sites$obj.y), max(sites$obj.y))){
  # hardcoded for maximising and minimising
  plot(sites[,c("obj.x", "obj.y")], xlab=xlab, ylab=ylab, main=main,
       xlim=rev(lims[1:2]), ylim=lims[3:4], cex.lab=1.3, cex.main=1.3)
  pareto <- psel(sites, high("obj.y")*low("obj.x"))
  draw_me_an_arrow(par()$usr, c(0.4,0.4), c(0.6,0.6),
                   width=0.05, headwidth=0.12, proplen = 0.7)
  pareto <- pareto[order(pareto$obj.x),]
  lines(pareto$obj.x, pareto$obj.y, col='#B1D668', lwd=2)
  points(pareto$obj.x, pareto$obj.y, pch=21, bg=pal(nrow(pareto)), cex=1.5)
}

map_panel <- function(sites1, sites, district){
  pareto <- psel(sites, high("obj.y")*low("obj.x"))
  pareto <- pareto[order(pareto$obj.x),]
  plot(st_geometry(district_shapes[district_shapes$district_name == district,]))
  points(sites1[,c('lon', "lat")], pch=4, col="grey60")
  
  tmppal <- pal(nrow(pareto))
  cols <- grep("site", names(pareto))
  sapply(1:nrow(pareto), function(x){
    lines(sites1[unlist(pareto[x, c(cols, cols[1])]), c("lon","lat")],
          col=tmppal[x], lwd=3)
  })
  
  ptcex <- table(unlist(pareto[,cols]))
  points(langkat_sites[as.numeric(names(ptcex)), c("lon", "lat")],
         cex=ptcex*1.2)
}
map_panel(langkat_sites, sites2, "LANGKAT")

sites2 <- inner_join(langkat_obj2_dist, langkat_obj2_sum, by=c("site1", "site2"))
sites3 <- inner_join(langkat_obj3_dist, langkat_obj3_sum, by=c("site1", "site2", "site3"))
sites4 <- inner_join(langkat_obj4_dist, langkat_obj4_sum, by=c("site1", "site2", "site3", "site4"))
sites5 <- inner_join(langkat_obj5_dist, langkat_obj5_sum, by=c("site1", "site2", "site3", "site4", "site5"))
sites6 <- inner_join(langkat_obj6_dist, langkat_obj6_sum, by=c("site1", "site2", "site3", "site4", "site5", "site6"))

{png("figures/multiple_sites/langkat_sum_dist_2_6_option1.png",
    height=3000,
    width=2000,
    pointsize=40)
par(mfrow = c(4,3), mar=c(4,1.5,2,1.5), oma=c(0,4,0,0), xpd=NA)
scatter_panel(sites2, main="2 sites", xlab="Network distance", ylab="Sum objective")
scatter_panel(sites3, main="3 sites", xlab="Network distance")
scatter_panel(sites4, main="4 sites", xlab="Network distance")
map_panel(langkat_sites, sites2, "LANGKAT")
map_panel(langkat_sites, sites3, "LANGKAT")
map_panel(langkat_sites, sites4, "LANGKAT")
scatter_panel(sites5, main="5 sites", xlab="Network distance", ylab="Sum objective")
scatter_panel(sites6, main="6 sites", xlab="Network distance")
plot(0, type="n", axes=FALSE, xlab="", ylab="")
map_panel(langkat_sites, sites6, "LANGKAT")
map_panel(langkat_sites, sites5, "LANGKAT")
dev.off()}

alllims <- data.frame(summin=c(min(langkat_obj2_sum$obj),
                            min(langkat_obj3_sum$obj),
                            min(langkat_obj4_sum$obj),
                            min(langkat_obj5_sum$obj),
                            min(langkat_obj6_sum$obj)),
                   summax=c(max(langkat_obj2_sum$obj),
                            max(langkat_obj3_sum$obj),
                            max(langkat_obj4_sum$obj),
                            max(langkat_obj5_sum$obj),
                            max(langkat_obj6_sum$obj)),
                   distmin=c(min(langkat_obj2_dist$obj),
                            min(langkat_obj3_dist$obj),
                            min(langkat_obj4_dist$obj),
                            min(langkat_obj5_dist$obj),
                            min(langkat_obj6_dist$obj)),
                   distmax=c(max(langkat_obj2_dist$obj),
                            max(langkat_obj3_dist$obj),
                            max(langkat_obj4_dist$obj),
                            max(langkat_obj5_dist$obj),
                            max(langkat_obj6_dist$obj)))

{png("figures/multiple_sites/langkat_sum_dist_2_6_option2.png",
    height=2500,
    width=2000,
    pointsize=40)
par(mfrow=c(3,3), mar=c(4.1,1.1,3.1,1.1), oma=c(0,3,0,0), xpd=NA)
scatter_panel(sites2, main="2 sites", xlab="Network distance", ylab="Sum objective",
              lims=c(min(alllims$distmin), max(alllims$distmax), 
                     min(alllims$summin), max(alllims$summax)))
scatter_panel(sites3, main="3 sites", xlab="Network distance",
              lims=c(min(alllims$distmin), max(alllims$distmax), 
                     min(alllims$summin), max(alllims$summax)))
scatter_panel(sites4, main="4 sites", xlab="Network distance",
              lims=c(min(alllims$distmin), max(alllims$distmax), 
                     min(alllims$summin), max(alllims$summax)))
par(mfrow=c(3,2), mar=c(4.1,1.1,3.1,1.1), oma=c(0,12,0,9), xpd=NA, mfg=c(2,1))
scatter_panel(sites5, main="4 sites", xlab="Network distance", ylab="Sum objective",
              lims=c(min(alllims$distmin), max(alllims$distmax), 
                     min(alllims$summin), max(alllims$summax)))
scatter_panel(sites6, main="5 sites", xlab="Network distance",
              lims=c(min(alllims$distmin), max(alllims$distmax), 
                     min(alllims$summin), max(alllims$summax)))

pal2 = viridis(7)[2:6]
pal2 <- rev(idem(6))
par(mfrow=c(6,1), xpd=FALSE, mar=c(4.1,1.1,1.1,1.1), oma=c(0,3,0,0),  mfg=c(5,1), new=TRUE)
plot(0, xlim=c(180, max(alllims$summax)+10), ylim=c(0,1.5),
     type="n", yaxt="n", ylab="", xlab="Sum objective (maximising)", cex.lab=1.3)
draw_me_an_arrow(par()$usr, c(0.42,0.4), c(0.57,0.4),
                 width=0.08, headwidth=0.2, proplen = 0.7)
for (i in 1:nrow(alllims)){
  lines(rep(alllims$summax[i], 2), c(-0.1, 0.9), lwd=5, col=pal2[i])
}
text(c(alllims$summax[1:2], alllims$summax[3]-1, alllims$summax[4], alllims$summax[5]+1), 
     c(rep(1.2, 3), 1.45, 1.2),
     paste0(2:6, " sites"), col=pal2, font=2, cex=1.3)

plot(0, xlim=rev(c(0, max(alllims$distmin)+0.05)), ylim=c(0,1.5), 
     type="n", yaxt="n", ylab="", xlab="Network distance objective (minimising)", cex.lab=1.3)
draw_me_an_arrow(par()$usr, c(0.42,0.4), c(0.57,0.4),
                 width=0.08, headwidth=0.2, proplen = 0.7)
for (i in 1:nrow(alllims)){
  lines(rep(alllims$distmin[i], 2), c(-0.1, 0.9), lwd=5, col=pal2[i])
}
text(alllims$distmin, rep(1.2, 5), paste0(2:6, " sites"), col=pal2, font=2, cex=1.3)

par(mfrow=c(1,1), mar=rep(0.1, 4), oma=rep(0,4), new=TRUE)
plot(0, xlab="", ylab="", axes=FALSE, xlim=c(0,1), ylim=c(0,1), bty="l", type="n")
subfigure_label(par()$usr, 0.03,0.99,"(a)")
subfigure_label(par()$usr, 0.03,0.33,"(b)")
subfigure_label(par()$usr, 0.03,0.16,"(c)")

dev.off()}




