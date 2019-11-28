read_nodes = function(url, CSS){
  
  url %>% read_html() %>% html_nodes(css = CSS) %>% 
    return()
}

href = function(x){
  
  html_attr(x = x, name = "href") %>% 
    return()
}

HTMLisation = function(href){
  href %>% 
    unique() %>% purrr::compact() %>%
    str_c("https://www.tripadvisor.fr", .) %>%
    str_subset(pattern = ".html") %>% 
    return()
}

FETCH_pages <- function(url) {
  print(url)
  url %>%
    read_nodes(CSS = ".pageNum") %>%
    href() %>% 
    HTMLisation()%>%
    return()
}


Fetch_activites <- function(Page_activite) {
  
  print(Page_activite)
  Page_activite %>%
    read_nodes(CSS = ".listing_title a") %>%
    href %>%
    str_c("tripadvisor.com", .) %>%
    return()
}

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
