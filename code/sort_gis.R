lulc_covs <- stack("data/lulc_covs.grd")

# drop these shps into the catchment_paper dir eventually ... or use MalariaAtlas pkg
idn_districts <- st_read('~/Desktop/knowlesi/pilot_sites/data/clean/idn_districts/idn_districts.shp')

# not sure which districts I want precisely
pilot_kabkot_names <- c("MALINAU", 
                        "TAPANULI TENGAH",
                        "TAPANULI SELATAN",
                        "LANGKAT",
                        "MANDAILING NATAL",
                        "PAKPAK BHARAT",
                        "BULUNGAN",
                        "NUNUKAN",
                        "DAIRI")

pilot_indices <- which(idn_districts$KABKOT %in% pilot_kabkot_names)
pilot_shapes <- idn_districts[pilot_indices,]

pilot_shapes$ras <- lapply(1:nrow(pilot_shapes), function(x){
  raster::mask(lulc_covs$human_pop, pilot_shapes[x,])
})

# add Bulungan to Malinau raster
pilot_shapes$ras[[7]] = merge(pilot_shapes$ras[[7]], pilot_shapes$ras[[8]])





