# inundation boundaries

ib <- read_csv("survey_points_inundation.csv")
# write_csv(ib, "survey_points_inundation.csv")

# convert to spatial
coordinates(ib) <- ib[,c(2,3)]

ib_sf <- st_as_sf(ib) %>% 
  dplyr::filter(Survey_ID !=175) # erroneous point

ib_sf %<>% st_set_crs(2248) # NAD83 MD state plane

# duplicate first survey point of each wetland to close polygons
ib_sf2 <- ib_sf %>% 
  group_by(Wetland) %>%
  mutate(pt_no = min_rank(Survey_ID)) %>%
  filter(pt_no == 1) %>%
  dplyr::select(-pt_no) %>%
  rbind(ib_sf)

# points into polygons
ib_polygons <- ib_sf2 %>% 
  arrange(Survey_ID) %>%
  summarise(by = list(ib_sf2$Wetland), do_union = FALSE) %>%
  st_cast(to = "LINESTRING") %>%
  st_cast(to = "POLYGON")

# ib_polygons %>%
#   st_transform(4326) %>%
#   leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery) %>%
#   addPolygons()

# ib_polygons %>% st_transform(4326) %>%
#   st_write("../maps/survey_inundation_boundaries.geojson")

