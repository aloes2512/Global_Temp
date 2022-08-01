#' Florians Lösungen'
ibrary(rvest)
library(magrittr)

url <- "https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/"

page <- read_html(url)

# Lösung: Suche Tabelle wähle Spalte "Name" und 
page %>% html_table() %>% {.[[1]]} %>%
  {.[["Name"]]} %>%
  stringr::str_subset("\\.nc$") -> urls_from_table
urls_from_table
# Lösung such alle Links und dann mit einer Regex nur die, die mit nc enden.
page %>% html_nodes("a") %>% html_attr('href') %>%
  stringr::str_subset("\\.nc$") -> urls_from_links


urls_from_table
urls_from_links



