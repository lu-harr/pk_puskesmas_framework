# FUNCTIONS TO MAKE TABLES !

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
summary_table_regency = function(tab, path, dataset_vec, cap = " ", rank_col=0,
                                 npoints_to_rank=min(10, nrow(tab)),
                                 lu_crits=land_use_crits, pop_passes=FALSE,
                                 pass_colour=pass_colour, fail_colour=fail_colour){
  neworder = NA
  
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
  
  kbl(tab, escape = F, align = "c", caption = cap, digits = 2, 
      table.attr = "style='width:70%;  height=30%'") %>%
    kable_classic() %>%
    #kable_styling(full_width = FALSE) %>%
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
    # max ecotype cols
    column_spec(5+ rank_col, background = spec_color_diy(pop_passes, c(fail_colour,pass_colour), bool_palette = TRUE, crit=0.1)) %>%
    column_spec(6+rank_col, 
                background = spec_color_diy(tab[[6+rank_col]],
                                            c(fail_colour,pass_colour),
                                            bool_palette=TRUE, 
                                            crit=lu_crits[2])) %>%
    column_spec(7+rank_col, 
                background = spec_color_diy(tab[[7+rank_col]],
                                            c(fail_colour,pass_colour),
                                            bool_palette=TRUE, 
                                            crit=lu_crits[3])) %>%
    column_spec(8+rank_col, 
                background = spec_color_diy(tab[[8+rank_col]], c(fail_colour,pass_colour),
                                            bool_palette=TRUE,
                                            crit=lu_crits[4])) %>%
    #background = c("#90EE90","white","white","white","white","white","white","white")) %>%
    # column_spec(8+rank_col, 
    #             background = spec_color_diy(tab[[8+rank_col]], c(fail_colour,pass_colour),
    #                                         bool_palette=TRUE, crit=land_use_crits[5])) #%>%
    save_kable(path, zoom=3)
  
  return(neworder)
}
