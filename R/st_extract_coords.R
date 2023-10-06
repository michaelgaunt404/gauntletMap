#' Extract the longitude and latitude coordinates of a spatial object
#'
#' This function extracts the longitude and latitude coordinates of a spatial object
#' representing points.
#'
#' @param spatial_object A spatial object representing points
#' @return A data frame with two columns, "lon" for longitude and "lat" for latitude
#' @importFrom sf st_coordinates
#' @export
#' @examples
#'mapview::breweries %>%
#'  st_extract_coords()
#'
st_extract_coords = function(spatial_object){
  spatial_object %>%
    mutate(lon = st_coordinates(geometry)[,1]
           ,lat = st_coordinates(geometry)[,2])
}
