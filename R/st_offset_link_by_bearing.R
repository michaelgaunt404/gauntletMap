#' Offset line geometries perpendicular to a bearing
#'
#' For each feature in `sf_obj` (typically a `LINESTRING` network link),
#' shifts every vertex of its geometry by `offset` units in the direction
#' perpendicular to `bearing`, producing a new, parallel `LINESTRING`.
#' Useful for visually separating links that would otherwise sit exactly
#' on top of each other on a map -- e.g. offsetting the two directions of
#' travel along the same corridor so both are visible.
#'
#' The offset direction is `bearing + 90` degrees, so a positive
#' `offset` value shifts the geometry to the right of the bearing's
#' direction of travel, and a negative value shifts it left.
#'
#' @param sf_obj An `sf` object. Each feature's geometry should be a
#'   `LINESTRING` with at least two vertices -- a `POINT` geometry will
#'   error, since a single coordinate can't be turned into a line.
#' @param bearing_col Character, name of a numeric column in `sf_obj`
#'   giving each feature's bearing/azimuth in degrees (0 = north,
#'   increasing clockwise), used to compute the perpendicular offset
#'   direction.
#' @param offset Either a single number (the default) applying the same
#'   offset distance to every feature, or a character string naming a
#'   numeric column in `sf_obj` to use a different offset per feature.
#'   Units are the linear units of `crs` (e.g. meters for a UTM zone).
#'   Positive values shift right of the bearing direction, negative
#'   values shift left. Defaults to `5` (i.e. every line shifted
#'   5 meters right, when `crs` is a meter-based CRS).
#' @param crs CRS to reproject into before computing the offset, since
#'   the offset distance needs to be in real linear units rather than
#'   degrees. Anything accepted by [sf::st_crs()]. Defaults to `32605`
#'   (UTM zone 5N) -- pick a CRS whose linear unit and zone actually
#'   suit your data's location.
#'
#' @return An `sf` object with the same attribute columns as `sf_obj`,
#'   reprojected to `crs`, with its geometry column replaced by the
#'   offset `LINESTRING` geometries. The result's CRS is `crs`; it is
#'   not reprojected back to `sf_obj`'s original CRS.
#'
#' @export
#'
#' @importFrom sf st_transform st_geometry st_coordinates st_linestring
#'   st_sfc st_set_geometry
#' @importFrom purrr map2
#'
#' @examples
#' \dontrun{
#' library(sfnetworks)
#' roxel
#' links_with_bearing <- st_calculate_heading_dataframe(roxel)
#'
#' links_offset = st_offset_link_by_bearing(
#'   links_with_bearing, "bearing", offset = 5
#' )
#'
#' mapview::mapview(links_with_bearing, color = "red", layer.name = "Regular") +
#'   mapview::mapview(links_offset, color = "blue", layer.name = "Offset")
#' }
st_offset_link_by_bearing <- function(sf_obj, bearing_col, offset = 5, crs = 32605) {
  sf_proj <- sf::st_transform(sf_obj, crs = crs)

  # grab values
  bearings <- sf_proj[[bearing_col]]

  if (is.character(offset) && length(offset) == 1) {
    if (!offset %in% names(sf_proj)) {
      stop("`offset` was given as a string but '", offset, "' is not a column in `sf_obj`.", call. = FALSE)
    }
    offsets <- sf_proj[[offset]]
  } else if (is.numeric(offset)) {
    offsets <- rep_len(offset, length(bearings))
  } else {
    stop("`offset` must be either a single number or the name of a numeric column in `sf_obj`.", call. = FALSE)
  }

  geoms <- sf::st_geometry(sf_proj)

  # Compute new geometries
  new_geoms <- purrr::map2(geoms, seq_along(bearings), function(g, i) {
    coords <- sf::st_coordinates(g)[, 1:2]

    b_rad <- bearings[i] * pi / 180
    p_rad <- b_rad + pi / 2

    delta_x <- offsets[i] * cos(p_rad)
    delta_y <- offsets[i] * sin(p_rad)

    offset_coords <- sweep(coords, 2, c(delta_x, delta_y), "+")
    sf::st_linestring(offset_coords)
  }) |>
    sf::st_sfc(crs = crs)

  sf::st_set_geometry(sf_proj, new_geoms)
}
