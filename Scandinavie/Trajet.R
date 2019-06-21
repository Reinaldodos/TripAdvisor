pacman::p_load(tidyverse, data.table, rio)
adresses = "Scandinavie/adresses" %>% read_rds()

test = 
  adresses %>%
  group_by(location) %>%
  summarise(lon = mean(lon, na.rm = T),
            lat = mean(lat, na.rm = T)) 

pacman::p_load(RSQLite)
BASE = src_sqlite(path = "Scandinavie/BASE", create = F)
adresses = 
  tbl(src = BASE, "Attractions") %>% 
  inner_join(y = test, by = c("Adresse" = "location"), copy = T) %>% 
  as.data.table() %>% 
  top_n(n = 100, wt = Reviews)

pacman::p_load(cartography, ggmap, sf, rnaturalearth, osrm)

MAP = rnaturalearth::ne_countries(country = c("Sweden", "Finland"), returnclass = "sf")

ggplot(data = MAP) +
  geom_sf() +
  geom_point(data = adresses, aes(x = lon, y = lat),
             fill = "darkred")

GEOM_adresses = st_as_sf(x = adresses, coords = c("lon", "lat"), crs = 4326)
TABLE = osrm::osrmTable(loc = GEOM_adresses,
                        gepaf = F,
                        measure = "distance")

pacman::p_load(dbscan)
new_adresses = 
  dbscan(x = TABLE$durations, eps = 90, minPts = 2) %>% .$cluster %>% 
  cbind.data.frame(adresses, Groupe = .) %>% 
  filter(Groupe>0)

ggplot(data = MAP) +
  geom_sf() +
  geom_point(data = new_adresses, aes(x = lon, y = lat, colour = Groupe %>% as.character()))

TOTO =
  new_adresses %>%
  semi_join(x = GEOM_adresses) %>%
  osrm::osrmTrip(returnclass = "sf")

TOTO[[1]]$trip %>% st_geometry() %>% plot()
TOTO[[1]]$summary
