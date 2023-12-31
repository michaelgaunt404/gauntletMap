#' Commonly used Leaflet tiles.
#'
#' This function returns a character vector containing the names of commonly used Leaflet
#' tiles, including "OSM (default)," "Esri," and "CartoDB."
#'
#' @return A character vector of commonly used Leaflet tile names.
#'
#' @examples
#' leaflet_default_tiles_index()
#'
#' @export
leaflet_default_tiles_index = function(){
  c("OSM (default)", "Esri", "CartoDB")
}
