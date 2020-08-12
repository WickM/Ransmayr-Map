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
  dplyr::mutate(langengrad = stringr::str_remove(langengrad, ","), 
                breitengrad = stringr::str_remove(breitengrad, ",")) %>% 
  dplyr::mutate(langengrad = stringr::str_trim(langengrad), 
                breitengrad = stringr::str_trim(breitengrad)) %>% 
  dplyr::mutate(lng = as.numeric(langengrad), 
                lat = as.numeric(breitengrad))

###Leaflet----

val_lonlat <- leaflet::validateCoords(lng = dat_ransmayr$lng , lat = dat_ransmayr$lat) %>% 
  filter (is.na(lng) | is.na(lat))

sextant_icon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/b/b0/Sextant.png",
  iconWidth = 20, iconHeight = 20
)


m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dat_ransmayr$lng , lat = dat_ransmayr$lat, popup = glue( 
    "text: {dat_ransmayr$text} <br> 
    {dat_ransmayr$kategorie_der_anwesenheit}  <br>
    "), clusterOptions = markerClusterOptions(), icon= sextant_icon
  )
m

