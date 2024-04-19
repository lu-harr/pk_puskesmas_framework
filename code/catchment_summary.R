# CATCHMENT SUMMARISATION
# I'm disturbed by what I find in here ....
# there's a hard-coded threshold for human population in there

catchment_summary_stats = function(catchment, 
                                   covt_stack,
                                   lu_names=land_use_names,
                                   lu_crits=land_use_crits,
                                   objective_name="objective", 
                                   indiv_out=FALSE,
                                   sum_pop=TRUE){
  # function to summarise ecotypes for a given catchment
  # trim NAs off outside of catchment area to speed things up
  ecotype_counts = trim(catchment)
  # mask covariate stack to catchment area - this is more for indiv catchment output
  #local_covts = projectRaster(covt_stack, ecotype_counts) # don't think this is required? is it an extent thing?
  local_covts = trim(mask(covt_stack, catchment))
  
  # set count of each land pixel to zero
  values(ecotype_counts)[which(!is.na(values(ecotype_counts)))] = 0
  qualifying_pixels = list()
  max_vals = list()
  
  # a n_pixel * n_ecotypes matrix
  indiv_vals = matrix(0, nrow=length(which(!is.na(values(ecotype_counts)))),
                      ncol=length(lu_names))
  indiv_bools = matrix(0, nrow=length(which(!is.na(values(ecotype_counts)))),
                       ncol=length(lu_names))
  
  for (i in 1:length(lu_names)){
    # vector of values of ecotype at all land pixels in catchment
    tmp_indiv_vals = values(local_covts[[lu_names[i]]])[which(!is.na(values(ecotype_counts)))]
    
    # check this hack is necessary? Might be mismatch in ecotype raster land mask
    if (length(tmp_indiv_vals) != length(indiv_vals[,i])){
      # hack for weird NA pixels :/ pads 0s onto the end of non-NA set
      tmp_indiv_vals = c(tmp_indiv_vals, 
                         rep(0, length(indiv_vals[,i]) - length(tmp_indiv_vals)))
    }
    
    indiv_vals[,i] = tmp_indiv_vals
    max_vals[[i]] = max(tmp_indiv_vals)
    
    # want to sum population in catchment rather than find the maximum representative pixel
    if (lu_names[[i]] == "human_pop" && sum_pop == TRUE){
      max_vals[[i]] = sum(tmp_indiv_vals)
    }
    
    # bool based on criterion for land use covt: gives raster of 1s and 0s
    values(local_covts[[lu_names[i]]])[which(!is.na(values(ecotype_counts)))] = ifelse(tmp_indiv_vals > lu_crits[i], 1, 0)
    indiv_bools[,i] = ifelse(tmp_indiv_vals > lu_crits[i], 1, 0)
    
    # how many pixels are representative?
    qualifying_pixels[[i]] = sum(values(local_covts[[lu_names[i]]]), na.rm=TRUE)
    
    # increment overall counts
    values(ecotype_counts) = values(ecotype_counts) + values(local_covts[[lu_names[i]]])
    
    if (lu_names[[i]] == "human_pop"){
      # want boolean for total population in catchment !!!
      # Setting threshold to 2000 manually
      qualifying_pixels[[i]] = ifelse(max_vals[[i]] > 10000, 1,0)
    }
  }
  
  names(qualifying_pixels) = lu_names
  names(max_vals) = paste0("max_", lu_names)
  
  if (indiv_out == TRUE){
    # for uses where we want info on indiv pixels
    return(list(indiv_vals=indiv_vals, 
                indiv_bools=indiv_bools, 
                obj=values(local_covts[[objective_name]])[which(!is.na(values(ecotype_counts)))],
                ecotype_counts=values(ecotype_counts)[which(!is.na(values(ecotype_counts)))],
                lu_names=lu_names))
  }
  
  return(append(list(npixel = length(which(!is.na(values(catchment)))),
                     mean_objective = mean(values(local_covts[[objective_name]]), na.rm=TRUE),
                     sd_objective = sd(values(local_covts[[objective_name]]), na.rm=TRUE),
                     max_objective = max(values(local_covts[[objective_name]]),
                                         na.rm=TRUE)),
                append(qualifying_pixels, max_vals)))
}
