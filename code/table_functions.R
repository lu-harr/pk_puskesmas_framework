# FUNCTIONS TO MAKE TABLES ! AND MAPS TO GO WITH !
library(kableExtra)
library(xtable)

########################################################################
# FUNCTION FOR TABLE COLOURING GIVEN PALETTE
spec_color_diy = function(x, palette="BuPu", begin=1, end=0, ncol=9, bool_palette=FALSE, crit=FALSE, rev=FALSE){
  # What if I don't want to use viridis ?!?!?!?
  # written to take a color brewer name
  # could extend to take vector of colours for colorRamp
  if (length(palette) == 1){
    pal = brewer.pal(ncol, palette) # sequential palettes ?
  }
  
  else{
    pal = colorRampPalette(palette)(ncol)
  }
  # trim palette for visibility of font:
  pal = pal[begin: (length(pal) - end)]
  if (bool_palette == TRUE){
    out = ifelse(x > crit, yes=pal[length(pal)], no=pal[1])
    out[is.na(out)] = "white"
    return(out)
  }
  
  if(rev == TRUE){
    pal = rev(pal)
  }
  
  if (length(unique(x)) == 1){
    return(rep(pal[length(pal)], length(x))) # all pixels contain ecotype
  }
  out = pal[as.numeric(cut(x, breaks=length(pal)-1))]
  out[is.na(out)] = "white"
  return(out)
}

########################################################################
# FUNCTION FOR FONT COLOUR
manual_font_colour = function(bg_vec, rev=FALSE){
  if (rev == FALSE){
    ifelse(bg_vec %in% c("#FED976", "#FEB24C"), "black", "white")
  } else {
    ifelse(bg_vec %in% c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A"), "white", "black")
  }
}

########################################################################
# FUNCTION THAT CREATES + SAVES TABLE
summary_table_regency = function(tab, 
                                 row_index,
                                 path, 
                                 dataset_vec, 
                                 cap = " ", 
                                 rank_col=0,
                                 npoints_to_rank=min(10, nrow(tab)),
                                 lu_crits=land_use_crits,
                                 pass_colour=pass_colour, 
                                 fail_colour=fail_colour){
  neworder = NA
  rownames(tab) = NULL
  pop_passes = tab[row_index, 10]
  tab = tab[row_index, 1:9] # subset columns to remove
  dataset_vec = dataset_vec[row_index]
  
  if (rank_col > 0){
    # create the column of ranks at far left
    neworder = order(tab$`Objective Mean`, decreasing=TRUE)
    tab = tab[neworder,]
    pop_passes = pop_passes[neworder]
    dataset_vec = dataset_vec[neworder]
    tab = cbind(c(1:npoints_to_rank, rep("-", nrow(tab) - npoints_to_rank)), tab)
    colnames(tab) = c("Rank", colnames(tab)[2:ncol(tab)])
    rownames(tab) = NULL
    print(neworder)
  }
  
  message(length(dataset_vec))
  message(nrow(tab))
  
  # tab$`Human Pop` = format(tab$`Human Pop`, scientific=TRUE, digits=3)
  tab$`Human Pop` = sapply(ceiling(log10(tab$`Human Pop`)), 
                           function(x){ifelse(x > 0, 
                                              paste(rep("*", x), collapse=""),
                                              "Low")})
  message(names(tab))
  message(length(names(tab)))
  
  tmp = kbl(tab, escape = F, align = "c", caption = cap, digits = 2, 
      table.attr = "style='width:70%;  height=30%'") %>%
    kable_classic() %>%
    add_header_above(c(" "=4+rank_col,"Total"=1, "Maximum Single Value"=3)) %>%
    # rank col
    column_spec(1, background = ifelse(dataset_vec == "prelim", "red", "white")) %>%
    column_spec(2) %>%
    # objective col
    column_spec(2+rank_col, 
                color = manual_font_colour(spec_color_diy(tab[[2+rank_col]], 
                                                          "YlOrRd", begin=3)), 
                background = spec_color_diy(tab[[2+rank_col]], "YlOrRd", begin=3),
                bold=TRUE) %>%
    # objective SD
    column_spec(3+rank_col, 
                color = manual_font_colour(spec_color_diy(tab[[3+rank_col]], 
                                                          "YlOrRd", begin=3),
                                           rev=TRUE), 
                background = spec_color_diy(tab[[3+rank_col]], "YlOrRd", begin=3,
                                            rev=TRUE),
                bold=TRUE) %>%
    # ecotype agg col
    column_spec(4+rank_col, 
                color = manual_font_colour(spec_color_diy(tab[[4+rank_col]], 
                                                          "YlOrRd", begin=3)), 
                background = spec_color_diy(tab[[4+rank_col]], "YlOrRd", begin=3),
                bold=TRUE) %>%
   column_spec(5+ rank_col, background = spec_color_diy(pop_passes, c("mediumpurple","lightgreen"), 
                                                        bool_palette = TRUE, crit=0.1)) %>%
    column_spec(column = 6+rank_col, 
                background = spec_color_diy(tab[[6+rank_col]],
                                            c("mediumpurple","lightgreen"),
                                            bool_palette=TRUE, 
                                            crit=lu_crits[2])) %>%
    column_spec(7+rank_col, 
                background = spec_color_diy(tab[[7+rank_col]],
                                            c("mediumpurple","lightgreen"),
                                            bool_palette=TRUE, 
                                            crit=lu_crits[3])) %>%
    column_spec(8+rank_col, 
                background = spec_color_diy(tab[[8+rank_col]], c("mediumpurple","lightgreen"),
                                            bool_palette=TRUE,
                                            crit=lu_crits[4])) %>%
    save_kable(path, zoom=3)
  
  return(tmp)
}


###############################################################################
# writing a separate version of this in xtable .... let's see how we go

set_bg_col <- function(vec, pal){
  out = pal[as.numeric(cut(vec, breaks=length(pal)-1))]
  out[is.na(out)] = "FFFFFF" # set NA backgrounds to 0
  out[is.na(vec)] = "FFFFFF"
  return(out)
}

set_font_col <- function(x){
  ifelse(x %in% c("BD0026", "800026", "E31A1C"), "FFFFFF", "000000")
}

#"#FED976" "#FEB24C" "#FD8D3C" "#FC4E2A" "#E31A1C" "#BD0026" "#800026"

summary_xtable = function(tab, 
                          row_index,
                          dataset_vec, 
                          cap = " ", 
                          lab = " ",
                          rank_col = 0,
                          npoints_to_rank = min(10, nrow(tab)),
                          lu_crits = land_use_crits,
                          pass_colour = pass_colour, 
                          fail_colour = fail_colour,
                          rank_only = FALSE){
  neworder = NA
  rownames(tab) = NULL
  pop_passes = tab[row_index, 10]
  tab = tab[row_index, 1:9]  # subset columns to remove
  dataset_vec = dataset_vec[row_index]
  
  if (rank_col > 0){
    # create the column of ranks at far left
    neworder = order(tab[,rank_col], decreasing=TRUE)
    tab = tab[neworder,]
    pop_passes = pop_passes[neworder]
    dataset_vec = dataset_vec[neworder]
    tab = cbind(c(1:npoints_to_rank, rep("-", nrow(tab) - npoints_to_rank)), tab)
    colnames(tab) = c("Rank", colnames(tab)[2:ncol(tab)])
    rownames(tab) = NULL
    # print(neworder)
  }
  
  if (rank_only == TRUE){
    return(neworder)
  }
  
  ylorrd_pal <- gsub("#", "", brewer.pal(9, "YlOrRd")[3:9])
  col_fail <- "9370D8"
  col_pass <- "90EE90"
  
  
  tab$`Human Pop` = sapply(ceiling(log10(tab$`Human Pop`)),
                           function(x){ifelse(x > 0,
                                              paste(rep("*", x), collapse=""),
                                              "Low")})
  
  coltab <- tab %>%
    mutate(`Objective Mean` = set_bg_col(tab$`Objective Mean`, ylorrd_pal),
           `Objective Std Dev` = set_bg_col(tab$`Objective Std Dev`, ylorrd_pal),
           `Eco-constraints  Present` = set_bg_col(tab$`Eco-constraints  Present`, ylorrd_pal),
           Croplands = ifelse(Croplands > lu_crits["croplands"], col_pass, col_fail),
           `Oil Palm` = ifelse(`Oil Palm` > lu_crits["oil_palm"], col_pass, col_fail),
           `Forest Loss` = ifelse(`Forest Loss` > lu_crits["loss_year"], col_pass, col_fail)) %>%
    mutate(obj_mean_font = set_font_col(`Objective Mean`),
           obj_sd_font = set_font_col(`Objective Std Dev`),
           ecotypes_font = set_font_col(`Eco-constraints  Present`))

  tab <- tab %>%
    mutate(across(`Objective Mean`:`Objective Std Dev`, 
                  function(x){
                    format(round(x, digits = 2), big.mark = ",", scientific = FALSE)
                  })) %>%
    mutate(across(`Croplands`:`Forest Loss`, 
                  function(x){
                    format(round(x, digits = 2), big.mark = ",", scientific = FALSE)
                  })) %>%
    mutate(Rank = paste0(ifelse(dataset_vec == "prelim", "\\cellcolor{red}", ""),
                         "{", Rank, "}"),
           `Objective Mean` = paste0("\\cellcolor[HTML]{", coltab$`Objective Mean`,
                                     "}\\textcolor[HTML]{", coltab$obj_mean_font, 
                                     "}{", `Objective Mean`, "}"),
           `Objective Std Dev` = paste0("\\cellcolor[HTML]{", coltab$`Objective Std Dev`,
                                        "}\\textcolor[HTML]{", coltab$obj_sd_font, 
                                        "}{",`Objective Std Dev`, "}"),
           `Eco-constraints  Present` = paste0("\\cellcolor[HTML]{", coltab$`Eco-constraints  Present`,
                                               "}\\textcolor[HTML]{", coltab$ecotypes_font, 
                                               "}{",`Eco-constraints  Present`, "}"),
           `Human Pop` = paste0("\\cellcolor[HTML]{",
                                ifelse(pop_passes > 0, col_pass, col_fail),
                                "}{",`Human Pop`, "}"),
           Croplands = paste0("\\cellcolor[HTML]{",
                              coltab$Croplands,
                              "}{",`Croplands`, "}"),
           `Oil Palm` = paste0("\\cellcolor[HTML]{",
                               coltab$`Oil Palm`,
                               "}{",`Oil Palm`, "}"),
           `Forest Loss` = paste0("\\cellcolor[HTML]{",
                                 coltab$`Forest Loss`,
                                 "}{",`Forest Loss`, "}"))
    
  xtab <- xtable::xtable(tab,
                         caption = cap,
                         label = lab)
  
  align(xtab) <- c("c", "C{0.05\\textwidth}", "C{0.15\\textwidth}", 
                   rep("C{0.08\\textwidth}", 8))
  
  print(xtab,
        #add.to.row = addtorow,
        size = "\\fontsize{9pt}{10pt}\\selectfont",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        hline.after = NULL)
  
  #return(neworder)
}

library(tidyverse)

summary_comb <- function(ranked_combs, sitedf, indivobj,
                         cap="", lab=""){
  # sitedf needs to be in order and have individual site mean objective/npixel
  # ranked combs needs to have the site indices and the design objective
  nsites <- sum(grepl("site", names(ranked_combs)))
  
  ylorrd_pal <- gsub("#", "", brewer.pal(9, "YlOrRd")[3:9])
  
  tab <- ranked_combs %>%
    mutate(across(starts_with("site"),
                  function(x){indivobj$obj[x]},
                  .names = "obj_{.col}")) %>%
    mutate(across(starts_with("site"), 
                  function(x){sitedf$name[x]}))
  
  message(nrow(tab))
  
  coltab <- tab %>%
    mutate(across(starts_with("obj"),
                  function(x){set_bg_col(x, ylorrd_pal)})) %>%
    mutate(across(starts_with("obj"),
                  function(x){set_font_col(x)},
                  .names = "font_{.col}"))
  
  tab <- tab %>%
    arrange(desc(obj)) %>%
    mutate(across(starts_with("obj"), 
                  function(x){
                    format(round(x, digits = 2), big.mark = ",", scientific = FALSE)
                  })) %>%
    mutate(across(starts_with("obj"),
                  function(x){
                    paste0("\\cellcolor[HTML]{", coltab[,cur_column()],
                           "}\\textcolor[HTML]{", coltab[,paste0("font_",cur_column())], 
                           "}{", x, "}")
                  })) %>%
    head(n = 10L) %>%
    dplyr::select(c(1, unlist(lapply(1:nsites, function(x){c(x, x+nsites)})) + 1)) #%>%
    # set_names(~ str_to_lower(.) %>%
    #             str_replace_all("_", " ") %>%
    #             str_to_title(.) %>%
    #             str_replace_all("Obj", "Objective") %>%
    #             str_replace_all("ite", "ite "))
  
  #names(tab) <- c("Net Obj", rep("", ncol(tab)-1))
  
  message(nrow(tab))
  
  xtab <- xtable::xtable(tab,
                         caption = cap,
                         label = lab)
  align(xtab) <- c("c\\|",rep("c", ncol(tab)))
  atr <- list(pos = list(-1, nrow(xtab)),
              command = c(paste("\\toprule",
                                "\\textbf{Net Obj}",
                                paste0(sapply(1:nsites, 
                                              function(i){paste0("& \\multicolumn{2}{c}{\\textbf{Site ", i, "}} ")}), 
                                       collapse=""),
                                "\\\\ \\midrule"),
                          "\\bottomrule"))
  
  print(xtab,
        size = "\\fontsize{9pt}{10pt}\\selectfont",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        hline.after = NULL,
        add.to.row = atr)
}
  

################################################################################
# todo here ...

ranked_map = function(all_points, map_tweaks, ranks, regency, catch_tag, outpath,
                      ranked=min(nrow(all_points), 10), malinau_zoom=FALSE, out=TRUE){
  # order points
  all_points = all_points[ranks,]
  kabkot_name = str_to_title(regency)
  kabkot_tag = gsub(" ", "", tolower(regency))
  
  if (out == TRUE){
    # start first plot
    # png(paste0(outpath, kabkot_tag, "_", catch_tag, "_rank_map.png"),
    #     width = 2000,
    #     height = 2000,
    #     pointsize = 40)
    # par(mar=c(6.1,8.1,8.1,8.1), xpd = TRUE)
    plot(st_geometry(district_shapes[district_shapes$district_name == regency,]), 
         #main=paste0(kabkot_name, ": ", catch_tag, " catchments"), 
         lwd=2, cex.main=2)
  }
  
  # ext = extent(trim(district_shapes$ras[[which(district_shapes$district_name == regency)]]))
  ext = par()$usr
  
    label_radius = min(ext[2] - ext[1], ext[4] - ext[3]) * 0.6
    centre = unlist(st_centroid(st_geometry(district_shapes[district_shapes$district_name == regency,])))
 
  
  centre = c(map_tweaks$lon, map_tweaks$lat)
  label_radius = map_tweaks$radius
  gap = map_tweaks$gap
  n_toadstools = map_tweaks$n_toadstools
  
  plot_sites_radius_pusk(ranked_sites = all_points[(1: ranked), c("lon", "lat")],
                         labs = sapply(1: ranked, function(x){paste0(x, ". ", all_points[x,"name"])}),
                         label_radius = label_radius,
                         centre = centre,
                         # lablocs = lablocs,
                         # testing = TRUE,
                         gap = gap,
                         n_toadstools = n_toadstools,
                         line_col = "black",
                         lab_col = "black",
                         lab_cex = 1.4)
   
  points(all_points[ranked+1:nrow(all_points), c("lon", "lat")], pch=4, 
         col=ifelse(all_points$dataset[ranked+1:nrow(all_points)] == "prelim", "red", "grey70"), lwd=4)
  points(all_points[1:ranked, c("lon", "lat")], pch=4, 
         col=ifelse(all_points$dataset[1:ranked] == "prelim", "red", "black"), lwd=4)
  
  # text(all_points[(1:ranked), c("lon", "lat")], 
  #      labels=sapply((1:10), function(x){paste0(x, " ", all_points[x,"name"])}),
  #      cex=1.1, font=2, pos=4, 
  #      col=ifelse(all_points$dataset[(1:ranked)] == "prelim", "red", "black"))
  
  # add cities in
  tmp = idn_cities[idn_cities$relevant.regency == regency,]
  if (nrow(tmp) > 0){
    points(tmp$lon, tmp$lat, col="blue", lwd=4)
    text(tmp$lon, tmp$lat, labels=tmp$Name, col="blue", pos=4,
         font=2,cex=1.1)
  }
  
  
  if (malinau_zoom == TRUE){
    # If this is the zoomed in plot, edit filename, plot title, add box of 
    # zoomed plot to larger plot
    kabkot_save = paste0(kabkot_save, "_zoom")
    kabkot_name = paste0(kabkot_name, " - Northeast")
    bound = c(116.3, 116.9, 3, 3.7)
    lines(c(bound[1], bound[1], bound[2], bound[2], bound[1]),
          c(bound[3], bound[4], bound[4], bound[3], bound[3]),
          col="red", lty=2, lwd=3)
  } else {
    # yeet out of function
    # dev.off()
    return(TRUE)
  }
  # or keep going to second plot
  # dev.off()
  # 
  # label_locs = all_points[1:ranked, c("lon", "lat")]
  # 
  # # start second plot - zoomed in for Malinau
  # png(paste0(outpath, kabkot_save, "_", catch_tag, "_rank_map.png"),
  #     width = 2000,
  #     height = 2000,
  #     pointsize = 50)
  # par(mar=c(5.1,4.1,4.1,0))
  # plot(district_shapes$ras[district_shapes$district_name == kabkot_name], 
  #      main=kabkot_name, cex.main=2,
  #      xlim = c(116.3,116.9), ylim=c(3,3.7), col="white", legend=FALSE,
  #      xlab="Longitude", ylab="Latitude")
  # plot(st_geometry(district_shapes[district_shapes$district_name == regency,]), 
  #      lwd=2, add = TRUE)
  # 
  # points(all_points[ranked+1:nrow(all_points), c("lon", "lat")], pch=4, 
  #        col=ifelse(all_points$dataset[ranked+1:nrow(all_points)] == "prelim",
  #                   "red", "grey70"), 
  #        lwd=4)
  # points(all_points[1:ranked, c("lon", "lat")], pch=4, 
  #        col=ifelse(all_points$dataset[1:ranked] == "prelim", "red", "black"),
  #        lwd=4)
  # text(label_locs, 
  #      labels=sapply(1:10, function(x){paste0(x, " ", all_points[x,"name"])}),
  #      cex=1.4, font=2,
  #      pos=4, col=ifelse(all_points$dataset[1:ranked] == "prelim", "red", "black"))
  # 
  # # add cities in
  # points(idn_cities$lon, idn_cities$lat, col="blue", lwd=4)
  # text(idn_cities$lon, idn_cities$lat, labels=idn_cities$Name, col="blue", pos=4,
  #      font=2)
  # 
  # if (out == TRUE){
  #   dev.off()
  # }
}










