#' Make a bounding box of an sf object
#'
#' @param base_geo an sf object - can be a singular ploygon or multiple polygons.
#'
#' @return a dataframe of coordinates describing extent of supplied sf object
#' @export
#'
#' @examples
#' #none
st_make_bounding_box = function(base_geo){
  stopifnot("CRS must be 4326, please convert..." = (st_crs(base_geo)$input == "EPSG:4326"))

  temp = base_geo %>% st_bbox()

  return(temp)
}
