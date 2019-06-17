
pacman::p_load(rvest, data.table, tidyverse, rio, igraph)

url =
  c(
    "https://www.tripadvisor.fr/Attractions-g189806-Activities-oa20-Sweden.html",
    "https://www.tripadvisor.fr/Attractions-g189896-Activities-oa20-Finland.html"
  )

read_nodes = function(url, CSS)
{
  url %>% read_html() %>% html_nodes(css = CSS) %>% 
    return()
}

href = function(x)
{
  html_attr(x = x, name = "href") %>% 
    return()
}

LIST_Destinations <- function(url) {
  url %>% read_nodes(CSS = "#LOCATION_LIST a") %>% 
    href() %>% 
    return()
}

Fetch_activites <- function(Page_activite) {
  print(Page_activite)
  Page_activite %>%
    read_html() %>% 
    html_nodes(css = ".attractions-attraction-overview-pois-PoiInfo__name--SJ0a4") %>% 
    html_attr(name = "href") %>% 
    str_c("tripadvisor.com", .) %>%
    return()
}

# Fetch destinations ------------------------------------------------------

FETCH_attractions <- function(url) {
  Liste_Destinations = list()
  while(grepl(pattern = "Attract", x = url))
  {
    Liste_Destinations=append(Liste_Destinations, url)
    url =
      url %>% read_nodes(CSS = ".sprite-pagePrev") %>% 
      html_attr(name = "href") %>% unique() %>% 
      str_c("https://www.tripadvisor.fr", .)
    print(url)
  } 
  
  Liste_Destinations = Liste_Destinations %>% unlist() %>%
    grep(pattern = "Attract", value = T)
  
  Liste_Attractions =
    Liste_Destinations %>% 
    map(.f = LIST_Destinations) 
  
  Liste_Attractions2 =
    Liste_Destinations %>% 
    map(.f = ~ read_nodes(url = ., CSS = ".ap_navigator .taLnk") %>% href())
  
  output =
    append(Liste_Attractions, Liste_Attractions2) %>%
    unlist() %>% unique() %>%
    sort() %>%
    setdiff(Liste_Destinations) %>%
    str_c("https://www.tripadvisor.fr", .)
  
  output %>% return()
}

output = url %>% map(.f = FETCH_attractions)
output %>% flatten_chr() %>% unique() %>% saveRDS(file = "Scandinavie/Destinations")
# Fetch activites ---------------------------------------------------------

pacman::p_load(rvest, data.table, tidyverse, rio)
Destinations = "Scandinavie/Destinations" %>% read_rds() %>% 
  grep(pattern = "Activities", value = TRUE)

Activites = Destinations %>% map(Fetch_activites)

activites = 
  Activites %>% unlist(use.names = F) %>% 
  grep(pattern = "Attraction_Review", value = T) 

saveRDS(object = activites, file = "Scandinavie/Liste activites")

# Fetch reviews -----------------------------------------------------------

activites =
  "Scandinavie/Liste activites" %>% read_rds() %>%
  str_c("https://www.", .)

Review_attraction <- function(attraction) {
  print(attraction)
  attraction_html =  attraction %>% read_html()
  Badge = html_nodes(x = attraction_html, css = ".badgeText") %>% html_text()
  print(Badge)
  if (length(Badge) > 0)
  {
    Adresse =
      html_nodes(x = attraction_html,
                 css = ".ui_link .detail") %>%
      html_text()
    
    attraction_html = attraction_html %>% html_nodes(css = ".attractionsHeader")
    list(
      Badge = Badge,
      Titre =
        html_nodes(x = attraction_html, css = "#HEADING") %>%
        html_text(),
      Themes =
        html_nodes(x = attraction_html,
                   css = ".detail a") %>%
        html_text(),
      Reviews =
        html_nodes(x = attraction_html,
                   css = ".reviewCount") %>%
        html_text(),
      Adresse = Adresse
    ) %>%
      return()
  }
}

output =
  activites %>% 
  map(Review_attraction)

output = output %>% unique()
output %>% saveRDS(file = "Scandinavie/Table activites")

# Mettre en forme les données ---------------------------------------------
TABLE =
  "Scandinavie/Table activites" %>% read_rds() %>% 
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
Themes %>% write.table(file = "Scandinavie/Themes.csv",
                       quote = F,
                       row.names = F)

Themes_a_bannir = "Themes Japon.csv" %>% import(fill = T,
                                                sep = "|",
                                                header = F) %>% .$V1
Attractions =
  Attractions_Themes %>%
  filter(Themes %in% Themes_a_bannir) %>%
  distinct(Titre) %>%
  anti_join(x = Attractions)

Attractions_Themes =
  Attractions %>% distinct(Titre) %>%
  inner_join(Attractions_Themes)

Themes = Attractions_Themes$Themes %>% unique()


# create SQLite -----------------------------------------------------------
pacman::p_load(dbplyr, RSQLite)

BASE = src_sqlite(path = "Scandinavie/BASE", create = TRUE)

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
