#############################################################
# ALL TABLES AND MAPS: INCLUDING SUPPS/FIGS IN MAIN TEXT

# script to do the same thing by district and wrap the figures in there too:

{sink("output/tables/all_tables_by_district.tex")
  for (regency in district_shapes$district_name){
    cat(paste0("\\subsubsection{", str_to_title(regency), "}\n"))
    
    summary_xtable(tab = tmp_dist_summ,
                   row_index = which(health_sites$regency == regency),
                   dataset_vec = health_sites$dataset,
                   cap = paste0(str_to_title(regency), " sites (distance catchments, 30 km)"), 
                   lab = paste0("tab:", gsub(" ", "_", str_to_lower(regency)), "_", "dist"),
                   rank_col=3)
    
    summary_xtable(tab = tmp_time_summ,
                   row_index = which(health_sites$regency == regency),
                   dataset_vec = health_sites$dataset,
                   cap = paste0(str_to_title(regency), " sites (travel time catchments, 100 minutes)"), 
                   lab = paste0("tab:", gsub(" ", "_", str_to_lower(regency)), "_", "time"),
                   rank_col=3)
    
    summary_xtable(tab = tmp_stretch_summ,
                   row_index = which(health_sites$regency == regency),
                   dataset_vec = health_sites$dataset,
                   cap = paste0(str_to_title(regency), " sites (``closest point'' catchments)"), 
                   lab = paste0("tab:", gsub(" ", "_", str_to_lower(regency)), "_", "stretch"),
                   rank_col=3)
    
    formatted_regency = gsub(" ", "_", str_to_lower(regency))
    cat("\\begin{figure}\n")
    cat("\\centering\n")
    cat(paste0("\\includegraphics[width=\\textwidth]{figs/", formatted_regency,"_all_rank_map.png}\n"))
    cat(paste0("\\caption{Mapped best ranked sites in ", str_to_title(regency), " using catchment definitions of (a) distance-based 
  limits of 30 km (Table \\ref{tab:", formatted_regency,"_dist}); (b) travel time-based limits of 100 
  minutes (Table \\ref{tab:", formatted_regency,"_time}); and (c) tesselated catchments (Table 
  \\ref{tab:", formatted_regency,"_stretch}). Site with ranks shaded in grey were suggested by surveillance study stakeholders. 
 Eco-constraints are indicated in green (pass) and purple (fail), summarised as ``Eco-constraints Present''.}\n"))
    cat(paste0("\\label{fig:maps_", formatted_regency,"}\n"))
    cat("\\end{figure}\n")
    cat("\\clearpage\n")
  }
  sink()}

# now for the maps
# read in cities locations
idn_cities = read.csv2("~/Desktop/knowlesi/data/raw/idn_cities.csv", header = TRUE, sep=",")
tmp_latlon = as.numeric(unlist(strsplit(as.vector(idn_cities$latlon), ",")))

idn_cities <- idn_cities %>%
  mutate(lat = as.numeric(tmp_latlon[seq(1,length(tmp_latlon),2)]),
         lon = as.numeric(tmp_latlon[seq(2,length(tmp_latlon),2)]))



dist_map_tweaks = data.frame(district = c("TAPANULI SELATAN","TAPANULI TENGAH", "DAIRI",
                                          "LANGKAT", "PAKPAK BHARAT", "MALINAU", "NUNUKAN"),
                             lon = c(99.258049, 98.8, 98.3, 98.25, 98.25, 115.8, 116.7),
                             lat = c(1.52, 2.15, 2.85,3.56, 2.55, 2.5, 4),
                             radius = c(0.68, 0.74, 0.4, 0.6, 0.25, 2.4,1.27),
                             gap = c(0.98, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99),
                             n_toadstools = c(30, 40, 40, 30, 40, 35, 30))

time_map_tweaks = data.frame(district = c("TAPANULI SELATAN","TAPANULI TENGAH", "DAIRI",
                                          "LANGKAT", "PAKPAK BHARAT", "MALINAU", "NUNUKAN"),
                             lon = c(99.258049, 99.1, 98.25, 98.22, 98.25, 115.8, 116.7),
                             lat = c(1.52, 2.2, 2.8,3.72, 2.55, 2.5, 4),
                             radius = c(0.68, 1, 0.4, 0.55, 0.25, 2.4,1.27),
                             gap = c(0.98, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99),
                             n_toadstools = c(30, 50, 40, 20, 40, 35, 30))

stretch_map_tweaks = data.frame(district = c("TAPANULI SELATAN","TAPANULI TENGAH", "DAIRI",
                                             "LANGKAT", "PAKPAK BHARAT", "MALINAU", "NUNUKAN"),
                                lon = c(99.258049, 99.2, 98.25, 98.22, 98.25, 115.7, 116.7),
                                lat = c(1.52, 2.2, 2.8,3.72, 2.55, 2.5, 4),
                                radius = c(0.68, 1.1, 0.4, 0.55, 0.25, 2.4,1.27),
                                gap = c(0.98, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99),
                                n_toadstools = c(30, 50, 40, 20, 40, 35, 30))

# have folded this into 1*3 figures for each district
# probably needs a subplot label?
for (regency in district_shapes$district_name){
  png(paste0("output/district_maps/", tolower(gsub(" ", "_", regency)), "_all_rank_map.png"),
      width = 3200,
      height = 2800,
      pointsize = 40)
  par(mar=c(6.1,7.1,6.1,7.1), oma=c(1.5,1.5,1.5,1.5), mfrow=c(2,2), xpd = NA)
  
  ranks = summary_xtable(tab = tmp_dist_summ,
                         row_index = which(health_sites$regency == regency),
                         dataset_vec = health_sites$dataset,
                         rank_col = 3,
                         rank_only = TRUE)
  ranked_map(all_points = health_sites[which(health_sites$regency == regency),],
             map_tweaks = dist_map_tweaks[which(dist_map_tweaks$district == regency),],
             ranks = ranks,
             regency = regency,
             catch_tag = "dist",
             outpath = "output/district_maps/")
  ranks = summary_xtable(tab = tmp_time_summ,
                         row_index = which(health_sites$regency == regency),
                         dataset_vec = health_sites$dataset,
                         rank_col = 3,
                         rank_only = TRUE)
  ranked_map(all_points = health_sites[which(health_sites$regency == regency),],
             map_tweaks = time_map_tweaks[which(time_map_tweaks$district == regency),],
             ranks = ranks,
             regency = regency,
             catch_tag = "time",
             outpath = "output/district_maps/")
  ranks = summary_xtable(tab = tmp_stretch_summ,
                         row_index = which(health_sites$regency == regency),
                         dataset_vec = health_sites$dataset,
                         rank_col = 3,
                         rank_only = TRUE)
  ranked_map(all_points = health_sites[which(health_sites$regency == regency),],
             map_tweaks = stretch_map_tweaks[which(stretch_map_tweaks$district == regency),],
             ranks = ranks,
             regency = regency,
             catch_tag = "stretch",
             outpath = "output/district_maps/")
  #mtext(str_to_title(regency), outer=TRUE, cex=2)
  dev.off()
}

#############################################################################
# Now just want map for Malinau/travel time catches .. 
# For the main body of the chapter ..
# will need to rig for zoom?
malinau_ranks = summary_xtable(tab = tmp_time_summ,
                       row_index = which(health_sites$regency == "MALINAU"),
                       dataset_vec = health_sites$dataset,
                       rank_col = 3,
                       rank_only = TRUE)[1:10]

malinau_map_tweaks <- time_map_tweaks[which(time_map_tweaks$district == "MALINAU"),]
malinau_points <- health_sites[which(health_sites$regency == "MALINAU"),]
malinau_unranked <- malinau_points[which(!1:nrow(malinau_points) %in% malinau_ranks),]
malinau_points <- malinau_points[malinau_ranks,]


ranked <- 10
#malinau_ext <- c(116.3,116.9,3,3.7)
ncolours <- 100
colbreaks <- seq(minValue(lulc_covs$brt_mean), maxValue(lulc_covs$brt_mean), 
                 length.out=ncolours + 1)
colbreaks[length(colbreaks)] <- colbreaks[length(colbreaks)] + 1e-7

{png(paste0("figures/", "malinau", "_time_rank_map.png"),
     width = 1500,
     height = 1400,
     pointsize = 30)
  par(mar=c(6.1,7.1,6.1,7.1), 
      xpd = NA, bty="n")
  plot(trim(mask(lulc_covs$brt_mean,
                 district_shapes[district_shapes$district_name == "MALINAU",])),
       bty="n", axes=FALSE,
       col=viridis(ncolours), breaks=colbreaks, legend=FALSE)
  plot(lulc_covs$brt_mean, legend.only=TRUE, 
       legend.args=list(text="Mean relative risk", side=2, line=1, cex=1.2),
       col=viridis(ncolours))
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]), lwd=2, add=TRUE)
  
  par(xpd=NA)
  select = c(1:4,6)
  plot_sites_radius_pusk(ranked_sites = malinau_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", malinau_points[x,"name"])}),
                         label_radius = 2,
                         centre = c(116.2,2.1),
                         #testing = TRUE,
                         gap = malinau_map_tweaks$gap,
                         n_toadstools = 50,
                         line_col = "black",
                         lab_col = "black",
                         lab_cex = 1.1)
  
  select = c(5,8)
  plot_sites_radius_pusk(ranked_sites = malinau_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", malinau_points[x,"name"])}),
                         label_radius = 1.3,
                         centre = c(116.2,4),
                         # testing = TRUE,
                         gap = malinau_map_tweaks$gap,
                         n_toadstools = malinau_map_tweaks$n_toadstools,
                         line_col = "black",
                         lab_col = "black",
                         lab_cex = 1.1)
  
  select = c(7,9,10)
  plot_sites_radius_pusk(ranked_sites = malinau_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", malinau_points[x,"name"])}),
                         label_radius = 1.1,
                         centre = c(116.4,3),
                         #testing = TRUE,
                         gap = malinau_map_tweaks$gap,
                         n_toadstools = malinau_map_tweaks$n_toadstools,
                         line_col = "black",
                         lab_col = "black",
                         lab_cex = 1.1)
  
  points(malinau_unranked[, c("lon", "lat")], pch=4, 
         col=ifelse(malinau_unranked$dataset == "prelim", "red", "grey70"), lwd=4)
  points(malinau_points[1:ranked, c("lon", "lat")], pch=4, 
         col=ifelse(malinau_points$dataset[1:ranked] == "prelim", "red", "black"), lwd=4)
  
  # add cities in
  tmp = idn_cities[idn_cities$relevant.regency == "MALINAU",]
  if (nrow(tmp) > 0){
    points(tmp$lon, tmp$lat, col="blue", lwd=4)
    text(tmp$lon, tmp$lat, labels=tmp$Name, col="blue", pos=4,
         font=2,cex=1.1)
  }
  
  mtext("Simple ranking in Malinau:\n 100 minute travel time catchments", font=2, cex=1.4, line=2)
  
  dev.off()}


langkat_ranks = summary_xtable(tab = tmp_time_summ,
                               row_index = which(health_sites$regency == "LANGKAT"),
                               dataset_vec = health_sites$dataset,
                               rank_col = 3,
                               rank_only = TRUE)[1:10]

langkat_map_tweaks <- time_map_tweaks[which(time_map_tweaks$district == "LANGKAT"),]
langkat_points <- health_sites[which(health_sites$regency == "LANGKAT"),]
langkat_unranked <- langkat_points[which(!1:nrow(langkat_points) %in% langkat_ranks),]
langkat_points <- langkat_points[langkat_ranks,]

# watch out! I'm bring in the idem pal >:) it's in misc_figures

# doing it again but panelled with langkat ...
{png(paste0("figures/", "langkat_malinau", "_time_rank_map.png"),
     width = 3000,
     height = 1400,
     pointsize = 35)
  par(mfrow=c(1,2),
      mar=c(5.5,5.5,5.5,5.5),
      oma=c(0,0,0,4),
      xpd = FALSE, bty="n")
  
  # HERE'S LANGKAT
  plot(trim(mask(lulc_covs$brt_mean,
                 district_shapes[district_shapes$district_name == "LANGKAT",])),
       bty="n", axes=FALSE,
       col=purps(ncolours), breaks=colbreaks, legend=FALSE)
  plot(st_geometry(district_shapes[district_shapes$district_name == "LANGKAT",]), lwd=2, add=TRUE)
  
  par(xpd=NA)
  select = c(1,4,5,8,9)
  plot_sites_radius_pusk(ranked_sites = langkat_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", langkat_points[x,"name"])}),
                         label_radius = 0.45,
                         centre = c(98.22, 3.55), gap = langkat_map_tweaks$gap,
                         n_toadstools = 35, line_col="grey80", lab_col = "black", lab_cex = 1.1)
  select = c(2,3,6,7,10)
  plot_sites_radius_pusk(ranked_sites = langkat_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", langkat_points[x,"name"])}),
                         label_radius = langkat_map_tweaks$radius, 
                         centre = c(98.3, 3.65), gap = langkat_map_tweaks$gap,
                         n_toadstools = 35, line_col="grey80", lab_col = "black", lab_cex = 1.1)
  points(langkat_unranked[, c("lon", "lat")], pch=4, 
         #col=ifelse(langkat_unranked$dataset == "prelim", "orange", "grey70"), 
         col="grey50",
         lwd=4)
  points(langkat_points[1:ranked, c("lon", "lat")], pch=4, 
         #col=ifelse(langkat_points$dataset[1:ranked] == "prelim", "orange", "black"), 
         col="orange",
         lwd=4)
  
  # add cities in ... Stabat is in the wrong place
  tmp = idn_cities[idn_cities$relevant.regency == "LANGKAT",]
  tmp = idn_cities[idn_cities$Name == "Binjai",]
  if (nrow(tmp) > 0){
    points(tmp$lon, tmp$lat, col="grey70", lwd=4)
    text(tmp$lon, tmp$lat, labels=tmp$Name, col="grey70", pos=4,
         font=2,cex=1.1)
  }
  
  subfigure_label(par()$usr, 0, 1, "(a)", 1.3)
  
  # HERE'S MALINAU ......
  plot(trim(mask(lulc_covs$brt_mean,
                 district_shapes[district_shapes$district_name == "MALINAU",])),
       bty="n", axes=FALSE,
       col=purps(ncolours), breaks=colbreaks, legend=FALSE)
  plot(st_geometry(district_shapes[district_shapes$district_name == "MALINAU",]), lwd=2, add=TRUE)
  
  par(xpd=NA)
  select = c(1:4,6)
  plot_sites_radius_pusk(ranked_sites = malinau_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", malinau_points[x,"name"])}),
                         label_radius = 1.85, centre = c(116.2,2.1), gap = malinau_map_tweaks$gap,
                         n_toadstools = 50,line_col="grey80", lab_col = "black", lab_cex = 1.1)
  select = c(5,8)
  plot_sites_radius_pusk(ranked_sites = malinau_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", malinau_points[x,"name"])}),
                         label_radius = 1.3, centre = c(116.2,4), gap = malinau_map_tweaks$gap,
                         n_toadstools = malinau_map_tweaks$n_toadstools, line_col="grey80", lab_col = "black",
                         lab_cex = 1.1)
  select = c(7,9,10)
  plot_sites_radius_pusk(ranked_sites = malinau_points[select, c("lon", "lat")],
                         labs = sapply(select, function(x){paste0(x, ". ", malinau_points[x,"name"])}),
                         label_radius = 1.3, centre = c(116.4,3),
                         gap = malinau_map_tweaks$gap, n_toadstools = 30,
                         line_col="grey80", lab_col = "black", lab_cex = 1.1)
  points(malinau_unranked[, c("lon", "lat")], pch=4, 
         #col=ifelse(malinau_unranked$dataset == "prelim", "orange", "grey70"),
         col="grey80",
         lwd=4)
  points(malinau_points[1:ranked, c("lon", "lat")], pch=4, 
         #col=ifelse(malinau_points$dataset[1:ranked] == "prelim", "orange", "black"),
         col="orange",
         lwd=4)
  
  # add cities in
  tmp = idn_cities[idn_cities$relevant.regency == "MALINAU",]
  if (nrow(tmp) > 0){
    points(tmp$lon, tmp$lat, col="grey70", lwd=4)
    text(tmp$lon, tmp$lat, labels=tmp$Name, col="grey70", pos=4,
         font=2,cex=1.1)
  }
  
  subfigure_label(par()$usr, 0, 1, "(b)", 1.3)
  
  par(mfrow=c(1,1), new=TRUE, oma=c(0,0,0,0), mar=c(4.1,4.1,4.1,4.1))
  plot(0, type="n", axes=FALSE, xlab="", ylab="")
  plot(lulc_covs$brt_mean, legend.only=TRUE, legend.mar=5,
       legend.args=list(text="Mean predicted relative risk", side=2, line=0.8, cex=1.2),
       col=purps(ncolours), legend.width=1)
  #mtext("Simple ranking in Malinau:\n 100 minute travel time catchments", font=2, cex=1.4, line=2)
  
  dev.off()}



