#' Quickly create a buffer around spatial objects
#'
#' This function creates a buffer around a spatial object with a specified radius.
#' By default, the buffer is created using a projected coordinate reference system (CRS)
#' and then transformed back to a geographic CRS.
#'
#' @param geo_data a spatial object
#' @param to the CRS to transform to, default is WGS84 (EPSG code 4326)
#' @param with the CRS to transform from, default is UTM zone 55S (EPSG code 2781)
#' @param radius the radius of the buffer in the unit of the input CRS, default is NA
#' @param endCapStyle the style of the end caps, defaults to "ROUND"
#' @param joinStyle the style of the line joints, defaults to "ROUND"
#'
#' @return a spatial object representing the buffer
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(sf)
#' curved_line = st_sfc(
#'   st_linestring(
#'     rbind(
#'       c(-122.68, 45.52)
#'       ,c(-122.63, 45.55)
#'       ,c(-122.58, 45.57)
#'       ,c(-122.55, 45.54)
#'     ))
#'   ,crs = 4326) %>%
#'   st_as_sf()
#'
#' mapview::mapview(curved_line) +
#'   (quick_buffer(curved_line, radius = 1000) %>%
#'      mapview::mapview())
#'
#' }
st_quick_buffer = function(geo_data, to = 4326, with = 2781, radius = NA
                           ,endCapStyle = "ROUND"
                           ,joinStyle = "ROUND"){
  geo_data %>%
    st_transform(crs = with) %>%
    st_buffer(dist = radius
              ,endCapStyle = endCapStyle
              ,joinStyle = joinStyle) %>%
    st_transform(crs = to)
}
