# work out where the hell I'm getting this from ...
# pretty sure it was the original model covariates
lulc_covs <- stack("data/lulc_covs.grd")

#####################################################
world_ras = st_read("~/Desktop/knowlesi/data/raw/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>%
  filter(NAME %in% c("Singapore","Thailand", "Indonesia",
                     "Philippines","Cambodia",
                     "Vietnam", "Brunei", "Malaysia")) %>%
  rasterize(lulc_covs$human_pop)

#####################################################
# SORT OUT MASK
# relocate these shps to catchment_paper dir
nw_idn_shp <- st_read("~/Desktop/knowlesi/data/raw/admin/admin2013_1.shp") %>%
  subset(COUNTRY_ID == "IDN") %>%
  subset(NAME %in% c("SUMATERA UTARA",
                     "SUMATERA BARAT",
                     "SUMATERA SELATAN",
                     "ACEH",
                     "RIAU",
                     "JAMBI",
                     "BENGKULU",
                     "LAMPUNG",
                     "NANGGROE ACEH DARUSALAM",
                     "KEPULAUAN RIAU",
                     "BANGKA BELITUNG",
                     "KALIMANTAN TENGAH",
                     "KALIMANTAN SELATAN",
                     "KALIMANTAN TIMUR",
                     "KALIMANTAN BARAT",
                     "KALIMANTAN UTARA",
                     "KEPULAUAN BANGKA BELITUNG"))
nw_idn_mask <- rasterize(nw_idn_shp, lulc_covs$human_pop)
# there is a little hole in southern sumatra .. need to fill in eventually

# this is the lulc_covs I want ... just need to reconfigure so the above isn't so circular
lulc_covs <- lulc_covs %>%
  mask(nw_idn_mask) %>%
  trim()

world_ras <- world_ras %>%
  crop(lulc_covs)

#####################################################

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
  dplyr::select(c("PROVINSI","KABKOT","geometry", "ras")) %>%
  rename(province_name=PROVINSI,
         district_name=KABKOT) %>%
  slice(DISTRICT_IND)




