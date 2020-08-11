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


m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dat_ransmayr$lng , lat = dat_ransmayr$lat, popup = glue( 
    "langengrad: {dat_ransmayr$lng} <br> 
    breitengrad: {dat_ransmayr$lat}  <br>
    text: {dat_ransmayr$text}")
    )
m
