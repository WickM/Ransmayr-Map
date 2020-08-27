#'//////////////////////////////////////////////////////////////////////////////
#' FILE: 
#' AUTHOR: Manuel Wick-Eckl
#' CREATED: 11 August 2020
#' MODIFIED: 11 August 2020
#' PURPOSE: clean the Ransmyr Datafile
#' Status: 
#' Comments:
#'//////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:
options(digits=9)

#'Libraries:
library(readxl)
library(here)
library(tidyverse)
library(janitor)
library(leaflet)
library(glue)

###readingData----
dat_ransmayr <- readxl::read_excel("data/Mapping the Atlas.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::select(- dplyr::starts_with("x")) %>% 
  dplyr::mutate(langengrad = stringr::str_replace_all(langengrad, "(?<=[[:digit:]])[[:punct:]](?=[[:digit:]])", "\\."), 
                breitengrad = stringr::str_replace_all(breitengrad, "(?<=[[:digit:]])[[:punct:]](?=[[:digit:]])", "\\.")) %>% 
  dplyr::mutate(langengrad = stringr::str_remove(langengrad, ","), 
                breitengrad = stringr::str_remove(breitengrad, ",")) %>% 
  dplyr::mutate(langengrad = stringr::str_trim(langengrad), 
                breitengrad = stringr::str_trim(breitengrad)) %>% 
  dplyr::mutate(lng = as.numeric(langengrad), 
                lat = as.numeric(breitengrad))

###Leaflet----

val_lonlat <- leaflet::validateCoords(lng = dat_ransmayr$lng , lat = dat_ransmayr$lat) %>% 
  filter (is.na(lng) | is.na(lat))

# sextant_icon <- makeIcon(
#   iconUrl = "https://upload.wikimedia.org/wikipedia/commons/b/b0/Sextant.png",
#   iconWidth = 20, iconHeight = 20
# )


m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
  addTiles()
  
  for (ii in unique(dat_ransmayr$text)) {
    temp <- dat_ransmayr %>% 
      filter(text == ii)
    
    m <- m %>% addCircleMarkers(lng = temp$lng , lat = temp$lat, popup = glue( 
      "Text: <b>{temp$text}</b> <br> 
    {temp$kategorie_der_anwesenheit}  <br>
    Ort: {temp$ort_laut_atlas}
    "),
      color = "red", radius = 6 ,stroke = FALSE, fillOpacity = 0.5, group = ii, 
      clusterOptions = markerClusterOptions(freezeAtZoom = 10))
  }
  

m <- m %>% addLayersControl(overlayGroups = unique(dat_ransmayr$text))
m


