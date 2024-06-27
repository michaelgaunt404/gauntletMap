#' Print basic leaflet map template
#'
#' @description
#' This function prints a basic leaflet map template that can be used as a starting point for building maps.
#'
#' @return None (prints to console).
#'
#' @examples
#' leaflet_code_remind_me()
#'
#' @export
leaflet_code_remind_me = function(){
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Printing basic leaflet map template\nUse the template to build your own leaflet maps{gauntlet::strg_make_space_2(last = F)}"))

  cat('
library(mapview)
library(leaflet)
library(gauntletMap)
library(grDevices)
library(tidyverse)

breweries = breweries %>%
  mutate(number.of.types = runif(nrow(breweries)
                                  ,min(breweries$number.of.types)
                                  ,max(breweries$number.of.types)) %>% round(0)
         ,label = str_glue("{brewery}\n{number.of.types}"))

pal_network = leaflet::colorNumeric(
  palette = grDevices::colorRamp(c("#96B856", "#56B8A9", "#7856B8",  "#B85665", "#96B856"), interpolate = "spline")
  # palette = "Blues"
  ,breweries$number.of.types
  ,reverse = T)

tmp_map = leaflet::leaflet() %>%
  leaflet::addTiles(group = "OSM (default)") %>%
  gauntletMap::leaflet_default_tiles() %>%
  leaflet::addCircleMarkers(
    data = breweries
    ,layerId = ~brewery
    ,color = ~pal_network(breweries$number.of.types)
    ,opacity = 1
    ,weight = 5
    ,group = "Network Links"
    ,label = breweries$label %>%
      purrr::map(htmltools::HTML)
  ) %>%
  leaflet::addLayersControl(
    baseGroups = gauntletMap::leaflet_default_tiles_index()
    ,overlayGroups = c("Network Links")
    ,options = leaflet::layersControlOptions(collapsed = F, sortLayers = F)) %>%
  leafem::addMouseCoordinates() %>%
  leaflet::addLegend(
    position = "bottomleft"
    ,title = "Link Bearing"
    ,group = "Network Links"
    ,pal = pal_network
    ,values = breweries$number.of.types)'
    )
}


