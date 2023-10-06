#' Make a doughnut polygon around a spatial object.
#'
#' Use an object and make a buffer around itself. Works well if you want to isolate an object but can only do it spatially.
#'
#' @param object A spatial object of class 'sf', 'sfc' or 'sfg'.
#' @param rad_bg Numeric. The radius for the background buffer.
#' @param rad_sm Numeric. The radius for the smaller buffer.
#' @param nm A string indicating the name of the buffer. Defaults to NULL.
#' @return A list with two elements: a background buffer and a difference buffer.
#' @examples
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
#' curved_line %>%
#'   st_self_buffer(rad_bg = 1000, rad_sm = 50, nm = "arbitary_name") %>%
#'   .[[2]] %>%
#'   mapview::mapview()
#' @importFrom sf st_as_sf st_difference st_union
#' @export
#'
#' @keywords spatial
#' @seealso \code{\link{st_buffer}}, \code{\link{st_difference}}, \code{\link{st_as_sf}}, \code{\link{st_union}}
st_self_buffer = function(spatial_object, rad_bg = NA, rad_sm = NA, nm = NULL){

  on_bg = spatial_object %>%
    quick_buffer(radius = rad_bg) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(buffer = nm) %>%
    suppressWarnings() %>%
    suppressMessages()

  on_sm = spatial_object %>%
    quick_buffer(radius = rad_sm) %>%
    st_union() %>%
    st_as_sf() %>%
    suppressWarnings() %>%
    suppressMessages

  ob_diff = st_difference(on_bg, on_sm) %>%
    mutate(rm_flag = 1) %>%
    suppressWarnings() %>%
    suppressMessages

  list(on_bg, ob_diff)
}
