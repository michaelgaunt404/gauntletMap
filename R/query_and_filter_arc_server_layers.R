#' Query an ArcGIS Server layer and optionally spatially filter it
#'
#' Downloads a spatial layer from an ArcGIS Server/REST endpoint, ensures valid
#' geometry, and transforms the result to EPSG:4326 (WGS84). If a filter geometry
#' is provided, features are spatially filtered to those intersecting the filter
#' geometry.
#'
#' @param arc_url Character. ArcGIS layer URL passed to
#'   \code{arcpullr::get_spatial_layer()}.
#' @param fltr_object Optional. A spatial object used to filter results (e.g., an
#'   \code{sf} object), or a file path that can be read by \code{sf::read_sf()}.
#'   If \code{NULL}, no spatial filtering is applied.
#'
#' @return An \code{sf} object in EPSG:4326 containing the queried features,
#'   optionally filtered by \code{fltr_object}.
#'
#'
#' @examples
#' #none
#
#' @importFrom arcpullr get_spatial_layer
#' @importFrom sf st_make_valid st_transform read_sf st_filter
#'
#' @export
query_and_filter_arc_server_layers = function(arc_url, fltr_object = NULL){
  temp_query_data = arcpullr::get_spatial_layer(
    url = arc_url) %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)
  if (!is.null(fltr_object)){
    temp_geometry = sf::read_sf(geo_file) %>%
      sf::st_transform(4326)
    temp_query_data = temp_query_data %>%
      sf::st_filter(temp_geometry)
  }
  return(temp_query_data)
}


