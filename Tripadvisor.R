require(pacman)
p_load(rvest,data.table,tidyverse, rio)
# lister les régions
regions = 
  "Liste Dstinations Japon" %>% read_rds() %>% as.list()
#   list(
#   "https://www.tripadvisor.fr/Attractions-g189413-Activities-Crete.html#ATTRACTION_SORT_WRAPPER"
# )

FETCHEZ <- function(test)
{
  Next_URL = try(read_html(test) %>%
                   html_node(".current+ .taLnk") %>%
                   html_attr("href"),
                 silent = TRUE)
  if (class(Next_URL) != "try-error")
  {
    Next_URL = paste("https://tripadvisor.fr", Next_URL, sep = "")
    return(Next_URL)
  }
}

activites = regions

while (length(regions) >= 1)
{
  test = regions[[1]]
  regions[[1]] = NULL
  show(test)
  output = FETCHEZ(test)
  regions[[length(regions) + 1]] = output
  activites[[length(activites) + 1]] = output
}

activites=activites %>% unlist()
activites %>% saveRDS(file = "Activites Japon")

XMLliste <- function(test)
{
  print(test)
  BRUT =
    test %>%
    read_html() %>%
    html_nodes(css = ".listing_info")
  return(BRUT)
}

Liste_XML = 
  grep(activites, pattern = "html", value = TRUE) %>% 
  # sample(10) %>% 
  map(.f = XMLliste)

Fetch_Score <- function(BRUT)
{
  Score =
    BRUT %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%
    html_attr(name = "alt")
  
  Score = Score[!is.na(Score)]
  
  Trash =
    BRUT %>%
    html_text() %>%
    strsplit(split = "\n") %>% unlist() %>% unique()
  
  Activite = Trash[2]
  Ville = Trash[3]
  Avis = grep(pattern = "avis",
              x = Trash,
              value = TRUE)
  Labels = Trash[6:length(Trash)]
  
  return(list(
    Score = Score,
    Activite = Activite,
    Ville = Ville,
    Avis = Avis,
    Labels = Labels
  ))
}

output =
  Liste_XML %>% 
  # sample(size = 2) %>%
  lapply(function(x)
    lapply(x, Fetch_Score)) %>%
  unlist(recursive = FALSE, use.names = TRUE)

output =
  output[sapply(output, function(x)
    length(x$Score) > 0)]

Labels =
  sapply(output, function(x)
    x$Labels) %>%
  unlist() %>%
  unique() %>% sort()

Labels %>%
  write.csv2(file = "Labels.csv",
             row.names = FALSE,
             quote = FALSE)

To_Shoot =
  "Labels.csv" %>% read.table(header = FALSE,
                              quote = "",
                              sep = ";")

selection =
  lapply(output, cbind.data.frame) %>%
  rbindlist() %>% 
  filter(!is.na(Labels))

SELECTION =
  selection %>%
  filter(Labels %in% To_Shoot$V1) %>%
  anti_join(x = selection, by = "Activite") %>%
  mutate(
    Avis =
      as.numeric(gsub(
        replacement = "",
        pattern = "\\D+",
        x = Avis
      )),
    Ville = gsub(
      pattern = "[^[:alnum:] ]",
      replacement = "",
      x = Ville
    )
  ) 

SELECTION =
  SELECTION %>%
  mutate(
    Score =
      Score %>%
      str_replace(pattern = "[^[:alnum:] ,]", replacement = "") %>%
      str_replace(pattern = ",", replacement = ".") %>%
      str_remove(pattern = "sur 5bulles") %>% trimws() %>% as.numeric()
  ) %>% 
  filter(Score>4,
         !str_detect(string = Ville, pattern = "km de"))

SELECTION =
  SELECTION %>% 
  arrange(desc(Avis)) 

SELECTION %>% 
  distinct(Activite, Ville, Labels) %>% 
  write.table(file = "Crete.csv", quote = TRUE, row.names = FALSE, sep = ",")

require(ggmap)
foo = 
  paste(SELECTION$Activite, SELECTION$Ville, sep = ", ") %>% unique() %>% 
  geocode(output = "more") 

foo =
  paste(SELECTION$Activite, SELECTION$Ville, sep = ", ") %>% unique() %>% 
  cbind.data.frame(Activite=., foo)

foo2=
  foo %>% filter(is.na(lon)) %>% distinct(Activite) %>% 
  unlist(use.names = F) %>% 
  str_remove_all(pattern = "[^[:alnum:] ]") %>% 
  geocode(output = "more") 

foo =
  foo %>% filter(is.na(lon)) %>% distinct(Activite) %>% cbind.data.frame(foo2) %>% 
  bind_rows(foo) %>% filter(!is.na(lon))

foo=foo %>% filter(country=="Greece") 
saveRDS(object = foo, file = "Geocodage Crete")

foo = 'Geocodage Crete' %>% read_rds()
SELECTION="Crete.csv" %>% import()

PROJETER <- function(gehaut)
{
  require(sp)
  gehaut = as.data.frame(gehaut)
  coordinates(gehaut) = c("lon", "lat")
  proj4string(gehaut) = CRS("+proj=longlat +datum=WGS84")
  
  require(rgdal)
  res = spTransform(gehaut, CRS("+proj=utm +zone=18 ellps=WGS84"))
  return(res)
}
gehaut = 
SELECTION %>% distinct(Activite, Ville) %>% 
  mutate(Activite=paste(Activite, Ville, sep = ", ")) %>% 
  inner_join(foo) %>% 
  filter(lat<36) %>% 
  slice(1:100) %>%
  data.table()


p_load(ggmap,osrm)
raude = osrmTable(loc = gehaut %>% select(Activite, lon, lat))

adj = raude$durations %>% data.table()
row.names(adj) = gehaut$Activite
names(adj) = as.character(gehaut$Activite)

p_load(dbscan)
test=dbscan(x = adj %>% as.matrix(), minPts = 10, eps = 90)
Groupes=
  test$cluster %>% cbind.data.frame(Activite=names(adj), Groupe=.) %>% 
  filter(Groupe>0) 

FINAL=
  inner_join(Groupes, gehaut) %>%
  inner_join(SELECTION %>% mutate(Activite = paste(Activite, Ville, sep =
                                                     ", "))) 
FINAL%>%
  distinct(Activite, address, N, Labels) %>% 
  write.table(file = "Crete.csv", sep = ",", row.names = F)

CARTE = get_map(location = "Crète, Grèce", zoom = 8)
ggmap(ggmap = CARTE) +
  geom_point(data = FINAL,
             mapping = aes(
               x = lon,
               y = lat,
               colour = Groupe %>% as.character()
             ))+
  scale_colour_hue()
