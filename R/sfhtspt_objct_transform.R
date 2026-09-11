#' Rebuild an object as a plain `sf` object
#'
#' Strips any extra classes/attributes an object may carry (e.g. output
#' from packages like `sfhotspot` that return objects layered on top of
#' `sf`) by reconstructing it from scratch: the non-spatial columns via
#' [sf::st_drop_geometry()] and the geometry column via
#' [sf::st_geometry()], recombined with [sf::st_as_sf()]. The result is a
#' standard `sf` object with the same data and geometry, but none of the
#' original object's other classes.
#'
#' @param obj An object that behaves like an `sf` object -- i.e. supports
#'   [sf::st_drop_geometry()] and [sf::st_geometry()] -- but may carry
#'   additional classes on top of `"sf"`.
#'
#' @return A plain `sf` object containing `obj`'s attribute columns and
#'   geometry.
#'
#' @export
#'
#' @importFrom sf st_as_sf st_drop_geometry st_geometry
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = 1:3,
#'   value = c(10, 25, 40),
#'   lng = c(-118.2437, -122.3321, -73.9857),
#'   lat = c(34.0522, 47.6062, 40.7484)
#' )
#' pts <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
#'
#' # pretend `pts` came back from a function that added extra classes on
#' # top of "sf" (e.g. sfhotspot output) -- this strips them back down
#' plain_sf <- sfhtspt_objct_transform(pts)
#' class(plain_sf)
#' }
sfhtspt_objct_transform <- function(obj) {
  sf::st_as_sf(
    sf::st_drop_geometry(obj),
    geometry = sf::st_geometry(obj)
  )
}
