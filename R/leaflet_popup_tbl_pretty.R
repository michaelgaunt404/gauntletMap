#' Create a popup table with formatted content.
#'
#' This function takes a spatial object and creates a popup table with formatted content.
#'
#' @param data A spatial object.
#'
#' @return A leaflet popup table.
#'
#' @examples
#' \dontrun{
#' library(mapview)
#' mapview::breweries %>%
#'   leaflet::leaflet() %>%
#'   leaflet::addCircleMarkers(color = "black", weight = 2, fillColor = "blue",
#'                             popup = leaflet_popup_tbl_pretty(mapview::breweries))
#' }
#'
#' @importFrom leafpop popupTable
#' @importFrom janitor clean_names
#' @importFrom sf st_set_geometry
#' @importFrom dplyr %>%
#'
#' @export
leaflet_popup_tbl_pretty = function(data){
  data %>%
    janitor::clean_names() %>%
    sf::st_set_geometry(NULL) %>%
    leafpop::popupTable()
}
