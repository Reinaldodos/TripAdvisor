pacman::p_load(tidyverse, data.table, rio)
adresses = "Scandinavie/adresses" %>% read_rds()

# Filtrer geolocs ---------------------------------------------------------
Good =
  adresses %>%
  count(location) %>% filter(n == 1) %>%
  semi_join(x = adresses)

Bad = anti_join(x=adresses, y =Good, by="location")

Values = Good %>% distinct(osm_key, osm_value) 

Good =
  inner_join(Bad, Values) %>%
  count(location) %>% filter(n == 1) %>%
  semi_join(x = adresses) %>%
  semi_join(Values) %>% 
  bind_rows(Good)

Bad = anti_join(x=adresses, y =Good, by="location")

test = 
  Bad %>%
  group_by(location) %>%
  summarise(lon = mean(lon, na.rm = T),
            lat = mean(lat, na.rm = T)) %>% 
  bind_rows(Good)

pacman::p_loadR(SQLite)
BASE = src_sqlite(path = "Scandinavie/BASE", create = F)
adresses = 
  tbl(src = BASE, "Attractions") %>% 
  inner_join(y = test, by = c("Adresse" = "location"), copy = T) %>% 
  as.data.table() 


# Sélection des labels ----------------------------------------------------

# tbl(src = BASE, "Themes_des_attractions") %>% 
#   pull(Themes) %>% unique() %>% sort %>% 
#   write(file = "Scandinavie/Labels.txt")

Labels = 'Scandinavie/Labels.txt' %>% read_file() %>% str_split(pattern = "\n") %>% 
  flatten_chr()

adresses =
  tbl(src = BASE, "Themes_des_attractions") %>%
  filter(Themes %in% Labels) %>%
  semi_join(x = adresses, copy = T, by = "Titre")

# Carte  ------------------------------------------------------------------
pacman::p_load(cartography, sf, osrm, rosm, ggmap)
GEOM_adresses = st_as_sf(x = adresses, coords = c("lon", "lat"), crs = 4326)

MAP = rnaturalearth::ne_countries(country = c("Sweden", "Finland"), returnclass = "sf")
Terrain =
  make_bbox(data = adresses, lon = lon, lat = lat) %>% 
  get_map(force = T, maptype = "roadmap")

GEOM_adresses %>% 
  st_transform(x = ., crs = 3857) %>%.$geometry %>% 
  plot(., bgMap = Terrain)

# Clustering --------------------------------------------------------------
Selection =
  GEOM_adresses %>% 
  top_n(n = 100, wt = Reviews)  

Selection %>% 
st_transform(x = ., crs = 3857) %>%.$geometry %>% 
  plot(., bgMap = Terrain)

TABLE = 
    Selection %>% 
    osrm::osrmTable(gepaf = F)

pacman::p_load(dbscan)
new_adresses = 
  dbscan(x = TABLE$durations, eps = 90, minPts = 2) %>% 
  .$cluster %>% cbind.data.frame(Selection, Groupe = .) %>% 
  filter(Groupe>0) %>% 
  mutate(Groupe = as.factor(Groupe)) %>% 
  dplyr::select(Titre, Groupe) %>% 
  inner_join(x = adresses, by = "Titre")

new_GEOM =
  st_as_sf(x = new_adresses, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = MAP) +
  geom_sf(data = new_GEOM, 
          mapping = aes(colour = Groupe))+
  geom_sf_text(data = new_GEOM, mapping = aes(label=Groupe))


# Selection des clusters --------------------------------------------------
GEOM_Selection =
  new_GEOM %>% 
  filter(Groupe %in% c(1, 4, 8, 9)) %>%
  # filter(Groupe %in% c(1, 4, 5, 8, 9, 10, 11)) %>%
  st_bbox() %>% 
  st_crop(x = GEOM_adresses, y = .) 

plot_STAMEN = function(Table, Map){
  Table %>% 
    st_transform(x = ., crs = 3857) %>%.$geometry %>% 
    plot(., bgMap = Map)
}

plot_STAMEN(Table = GEOM_Selection, Map = Terrain)

# On découpe en deux voyages ----------------------------------------------
Clusters =
  adresses %>%
  semi_join(y = GEOM_Selection, by = "Titre") %>%
  select(lon, lat) %>% 
  kmeans(centers = 2)

Pays =
  adresses %>%
  semi_join(y = GEOM_Selection, by = "Titre") %>%
  cbind.data.frame(Cluster = Clusters$cluster) %>% 
  split(f = .$Cluster) %>% 
  map(.f = select, Titre, lon, lat)


TOTO =
  Pays %>%
  map(.f = osrm::osrmTrip, returnclass = "sf")

Trips = 
  TOTO %>% 
  purrr::transpose() %>% .[[1]] %>% 
  purrr::transpose() %>% .$trip

TRIP_MAP <- function(trip) {
  ggplot(data = trip) +
    geom_sf(data = st_crop(x = MAP, y =trip)) +
    geom_sf() +
    geom_sf_text(mapping = aes(label = round(duration)))+
    geom_sf(data = st_crop(x=GEOM_Selection,y = trip))
}

Trips %>% map(TRIP_MAP)

trip_Stamen <- function(trip) {
  trip_MAP = 
    trip %>% st_coordinates %>% data.table() %>% 
    make_bbox(lon = X, lat = Y) %>% 
    get_map(force = F, maptype = "roadmap")
  
  st_union(trip, st_crop(x = GEOM_Selection, y = trip)) %>%
    st_transform(x = ., crs = 3857) %>% .$geometry %>%
    plot(., bgMap = trip_MAP)
}

Trips %>% map(trip_Stamen)
