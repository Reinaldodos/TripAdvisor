pacman::p_load(rvest, data.table, tidyverse, rio, igraph)
source("Fonctions.R")

setwd(dir = "Cornouailles/")
url ="https://www.tripadvisor.fr/Attractions-g186234-Activities-Cornwall_England.html"


# Récupérer la dernière page ----------------------------------------------
url=
  url %>% 
  read_nodes(CSS = 
".attractions-attraction-overview-main-Pagination__link--2m5mV+ .attractions-attraction-overview-main-Pagination__link--2m5mV a"
) %>% href() %>% HTMLisation()

url=url[length(url)]

# Fetch destinations ------------------------------------------------------

Liste_Destinations = list()
To_Do = url
while (length(To_Do) > 0) {
  url = To_Do[1]
  Liste_Destinations = 
    append(as.list(Liste_Destinations), url) %>% 
    flatten_chr()
  To_Do =
    FETCH_pages(url) %>%
    append(as.list(To_Do), .) %>% flatten_chr() %>%
    setdiff(Liste_Destinations)
}



# Fetch activites ---------------------------------------------------------

Activites = Liste_Destinations %>% sort %>% map(Fetch_activites)

activites = 
  Activites %>% unlist(use.names = F) %>% 
  grep(pattern = "Attraction_Review", value = T) %>% 
  sort

saveRDS(object = activites, file = "Liste activites.rds")


# Fetch reviews -----------------------------------------------------------
output =
  activites %>%sort %>% str_c("http://www.",.) %>%  
  map(Review_attraction)

output = output %>% unique()
output %>% saveRDS(file = "Table activites.rds")

# Mettre en forme les données ---------------------------------------------
TABLE =
  "Table activites.rds" %>% read_rds() %>% 
  map(.f = ~ lapply(., function(x)
    if (identical(x, character(0)))
      NA_character_
    else
      x)) %>%
  map(cbind.data.frame) %>%
  bind_rows() %>%
  mutate(Reviews = str_remove_all(string = Reviews, pattern = "[^[0-9]]") %>%
           as.numeric())


Attractions = TABLE %>% distinct(Titre, Reviews, Adresse)

Attractions_Themes =
  TABLE %>% distinct(Titre, Themes) %>%
  filter(!is.na(Themes),
         Themes != "More")

Themes = Attractions_Themes$Themes %>% unique()

# Filtrer les themes ------------------------------------------------------
Themes %>% write.table(file = "Themes.csv",
                       quote = F,
                       row.names = F)

Themes_a_bannir = "Themes.csv" %>% import(fill = T,
                                                sep = "|",
                                                header = F) %>% .$V1
Attractions=
  Attractions_Themes %>%
  filter(Themes %in% Themes_a_bannir) %>%
  distinct(Titre) %>%
  anti_join(x = Attractions)

Attractions_Themes =
  Attractions %>% distinct(Titre) %>%
  inner_join(Attractions_Themes)

Themes = Attractions_Themes$Themes %>% unique()


# create SQLite -----------------------------------------------------------
pacman::p_load(RSQLite)
BASE = src_sqlite(path = "BASE", create = TRUE)

copy_to(
  dest = BASE,
  df = Attractions,
  name = "Attractions",
  overwrite = TRUE,
  temporary = FALSE,
  analyze = TRUE
)
copy_to(
  dest = BASE,
  df = Attractions_Themes,
  name = "Themes_des_attractions",
  overwrite = TRUE,
  temporary = FALSE,
  analyze = TRUE
)
