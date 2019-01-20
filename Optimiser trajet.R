library(rvest)
library(data.table)
setwd("~/Dropbox/Carto & Stats/R/TripAdvisor")

# Géocodage ---------------------------------------------------------------
fichier = fread("Quebec.csv")
adresses = paste(fichier$Nom, fichier$Adresse) %>% unique
Noms = fichier$Nom %>% unique()

require(ggmap)
gehaut = geocode(adresses) %>% cbind(Noms) %>% cbind(adresses)%>%
  data.table()
torecode = gehaut[is.na(gehaut$lon),] %>% data.table()

torecode$adresses = c( 
  "Cetatea Poienari",
  "Biserica Fortificată"
)

torecode2 = geocode(as.character(torecode$adresses)) %>% cbind(torecode[, .(Noms, adresses)]) %>% data.table()
colnames(torecode2) = colnames(gehaut[, .(lon, lat, Noms, adresses)])
gehaut = rbind(torecode2, gehaut[!is.na(gehaut$lon), .(lon, lat, Noms, adresses)])
saveRDS(
  object = gehaut[,.(lon, lat, Noms)],
  file = "geocode",
  ascii = FALSE,
  compress = "gzip"
)

gehaut = readRDS(file = "geocode")
gehaut = gehaut[lon>0]
# Passage en mode carte ---------------------------------------------------

PROJETER <- function(gehaut)
{
  require(sp)
  gehaut = as.data.frame(gehaut)
  coordinates(gehaut) = c("lon", "lat")
  proj4string(gehaut) = CRS("+proj=longlat +datum=WGS84")
  
  require(rgdal)
  res = spTransform(gehaut, CRS("+proj=utm +zone=34 ellps=WGS84"))
  return(res)
}

require(osrm)
raude = osrmTable(loc = PROJETER(gehaut))
adj = raude$durations %>% data.table()
adj$From = row.names(adj)
# Retirer doublons d'emplacement ------------------------------------------
require(tidyr)
adj = gather(
  data = adj,
  key = "Dest",
  value = "Dist",
  -From,
  na.rm = TRUE,
  convert = TRUE
) %>%
  data.table()

outlets = adj[Dist == 0 & From < Dest]$Dest %>% unique()
condition = rownames(as.data.frame(gehaut)) %in% outlets
gehaut = as.data.frame(gehaut[!condition,])


# Optimisation de trajets -------------------------------------------------
TRIPP <- function(gehaut)
{
  coordinates(gehaut) = c("lon", "lat")
  proj4string(gehaut) = CRS("+proj=longlat +datum=WGS84")
  require(rgdal)
  res = spTransform(gehaut, CRS("+proj=utm +zone=18 ellps=WGS84"))
  
  trips = osrmTrip(loc = res, overview = "full")
  
  if (require("cartography")) {
    osm <-
      getTiles(spdf = trips[[1]]$trip,
               crop = TRUE,
               type = "osmgrayscale")
    tilesLayer(osm)
    plot(
      src,
      pch = 20,
      col = "red",
      cex = 2,
      add = TRUE
    )
    plot(trips[[1]]$trip,
         col = 1:5,
         add = TRUE,
         lwd = 2)
  }
  
  trip = trips[[1]]$trip %>% as.data.frame() %>% data.table()
  return(list(trips))
  
}

test = gehaut %>% as.data.frame() %>% data.table()
trip1 = test %>%
  as.data.frame() %>% TRIPP()

# Analyse du réseau de destinations ---------------------------------------
raude = osrmTable(loc = PROJETER(gehaut))
adj = raude$durations %>% data.table()
adj$From = row.names(adj)

require(igraph)
seuil = 50

proche = sapply(raude$durations, function(x)
  ifelse(
    test = x < seuil,
    yes = 1,
    no = 0
  )) %>%
  matrix(ncol = dim(raude$durations)[1])

graphe = graph.adjacency(adjmatrix = proche,
                         mode = "upper",
                         diag = F)
plot(graphe)

groupes = cluster_optimal(graph = graphe, weights = NULL)
plot(groupes,
     graphe,
     col = membership(groupes),
     mark.groups = communities(groupes))

new_gehaut = cbind.data.frame(as.data.frame(gehaut), as.vector(membership(groupes))) %>%
  data.table()
colnames(new_gehaut) = c("lon", "lat", "adresses", "Groupe")


# Carte colorée par Groupe ------------------------------------------------
test = new_gehaut %>% as.data.frame() %>% data.table()
test = test[grepl(pattern = "Roumanie", x = test$adresses)]

taille = table(test$Groupe) %>% data.table()

test = test[test$Groupe %in% unique(taille[N > nrow(test) / 12]$V1)]
taille = table(test$Groupe) %>% data.table()
taille$Jours = (taille$N / (sum(taille$N) / 12)) %>% round(digits = 0)

require(sqldf)
teste = sqldf(
  '
  select sum(lon)/count(*) as lon, sum(lat)/count(*) as lat, Groupe
  from test
  group by Groupe
  '
) %>%
  data.table() %>%
  merge(y = taille[, .(as.double(V1), Jours)], by.x = "Groupe", by.y = "V1", all = F)
  
  
  trip = teste %>% as.data.frame() %>% TRIPP()

require(cartography)
osm = getTiles(
  spdf = PROJETER(as.data.frame(test)),
  type = "stamenbw",
  zoom = NULL,
  crop = F
)
tilesLayer(x = osm, add = F)
plot(SpatialPoints(PROJETER(as.data.frame(teste))), col = 2, add = T)

teste$QUERY = paste("http://maps.google.com/?q=", teste$lat, ",", teste$lon, sep = "")

