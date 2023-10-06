#' Default tiles for a leaflet map.
#'
#' This function adds default map tiles to a leaflet object. It includes OpenStreetMap (OSM),
#' Esri, and CartoDB tiles as options.
#'
#' @param object A blank leaflet object to which you want to add default tiles.
#'
#' @return A leaflet object with default map tiles added.
#'
#' @examples
#' \dontrun{
#' library(mapview)
#' mapview::breweries %>%
#'   leaflet::leaflet() %>%
#'   leaflet::addCircleMarkers(color = "black", weight = 2, fillColor = "blue") %>%
#'   leaflet_default_tiles() %>%
#'   leaflet::addLayersControl(baseGroups = leaflet_default_tiles_index(),
#'                             options = leaflet::layersControlOptions(collapsed = FALSE))
#' }
#'
#' @importFrom leaflet addTiles addProviderTiles
#' @export
leaflet_default_tiles = function(object){
  object %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles(leaflet:::providers$Esri, group = "Esri") %>%
    leaflet::addProviderTiles(leaflet:::providers$CartoDB, group = "CartoDB")
}
