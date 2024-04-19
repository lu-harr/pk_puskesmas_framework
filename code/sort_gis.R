lulc_covs <- stack("data/lulc_covs.grd")

# drop these shps into the catchment_paper dir eventually ... or use MalariaAtlas pkg
idn_districts <- st_read('~/Desktop/knowlesi/pilot_sites/data/clean/idn_districts/idn_districts.shp')

# not sure which districts I want precisely
district_names <- c("MALINAU", 
                    "TAPANULI TENGAH",
                    "TAPANULI SELATAN",
                    "LANGKAT",
                    "MANDAILING NATAL",
                    "PAKPAK BHARAT",
                    "BULUNGAN",
                    "NUNUKAN",
                    "DAIRI")



district_indices <- which(idn_districts$KABKOT %in% district_names)
district_shapes <- idn_districts[district_indices,]

district_shapes$ras <- lapply(1:nrow(district_shapes), function(x){
  raster::mask(lulc_covs$human_pop, district_shapes[x,])
})

# add Bulungan to Malinau raster
district_shapes$ras[[7]] = merge(district_shapes$ras[[7]], district_shapes$ras[[8]])

# Here are the district we're actually interested in?
DISTRICT_IND = c(2,3,4,5,6,7,9)

district_shapes <- district_shapes %>%
  select(c("PROVINSI","KABKOT","geometry", "ras")) %>%
  rename(province_name=PROVINSI,
         district_name=KABKOT) %>%
  slice(DISTRICT_IND)




