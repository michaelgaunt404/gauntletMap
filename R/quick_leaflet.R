#' Make a quick leaflet plot.
#'
#' This function creates a leaflet map with various options for displaying spatial data.
#'
#' @param data A spatial object.
#' @param markers A boolean value indicating whether to display marker symbols on the map. The default value is false.
#' @param lines A boolean value indicating whether to display lines on the map. The default value is false.
#' @param polys A boolean value indicating whether to display polygons on the map. The default value is false.
#'
#' @return A leaflet map.
#' @export
#'
#' @examples
#' mapview::breweries %>%
#' quick_leaflet(markers = TRUE)
#'
#' @importFrom leaflet addTiles addCircleMarkers addPolylines addPolygons
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom maps::map
quick_leaflet = function(data, markers = F, lines = F, polys = F){
  data %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    { if (markers) (.) %>% leaflet::addCircleMarkers(color = "black", weight = 2, fillColor = "blue") else .} %>%
    { if (lines) (.) %>% leaflet::addPolylines() else .} %>%
    { if (polys) (.) %>% leaflet::addPolygons(color = "black", weight = 2, fillColor = "blue") else .}
}




