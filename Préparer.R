pacman::p_load(rvest, data.table, tidyverse, rio, igraph)

url ="https://www.tripadvisor.fr/Attractions-g186234-Activities-Cornwall_England.html"
read_nodes = function(url, CSS){
  
  url %>% read_html() %>% html_nodes(css = CSS) %>% 
    return()
}

href = function(x){
  
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
    read_nodes(css = ".listing_title a") %>%
    href %>%
    str_c("tripadvisor.com", .) %>%
    return()
}

# Fetch destinations ------------------------------------------------------

Liste_Destinations = list()
while(grepl(pattern = "Attract", x = url))
{
  Liste_Destinations=append(Liste_Destinations, url)
  url =
    url %>% 
    read_nodes(CSS = ".attractions-attraction-overview-main-Pagination__button--1up7M , .attractions-attraction-overview-main-Pagination__button--1up7M a") %>% 
    html_attr(name = "href") %>% unique() %>% purrr::compact() %>%  
    str_c("https://www.tripadvisor.fr", .) %>% 
    str_subset(pattern = ".html")
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

output=
  append(Liste_Attractions, Liste_Attractions2) %>% 
  unlist() %>% unique() %>% 
  sort() %>% 
  setdiff(Liste_Destinations) %>% 
  str_c("https://www.tripadvisor.fr", .)

output %>% saveRDS(file = "Liste Dstinations Japon")



# Fetch activites ---------------------------------------------------------

pacman::p_load(rvest, data.table, tidyverse, rio)
Destinations = "Liste Dstinations Japon" %>% read_rds() %>% 
  grep(pattern = "Activities", value = TRUE)

test = Destinations %>% sample(1)



Activites = Destinations %>% sort %>% map(Fetch_activites)

activites = 
  Activites %>% unlist(use.names = F) %>% 
  grep(pattern = "Attraction_Review", value = T) %>% 
  sort

saveRDS(object = activites, file = "Liste activites Japon")


# Fetch reviews -----------------------------------------------------------

activites =
  "Liste activites Japon" %>% read_rds() %>%
  str_c("https://www.", .)

Review_attraction <- function(attraction) {
  # print(attraction)
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
output %>% saveRDS(file = "Table activites Japon")

# Mettre en forme les données ---------------------------------------------
TABLE =
  "Table activites Japon" %>% read_rds() %>% 
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
Themes %>% write.table(file = "Themes Japon.csv",
                       quote = F,
                       row.names = F)

Themes_a_bannir = "Themes Japon.csv" %>% import(fill = T,
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
BASE = src_sqlite(path = "JAPON", create = TRUE)
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
