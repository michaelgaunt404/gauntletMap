#' Calculate compass bearing for line features or lat/lon pairs
#'
#' Computes the compass bearing (0-360 degrees, measured clockwise from
#' north) from a start point to an end point. Works two ways, depending
#' on which arguments are supplied:
#'
#' * **Column mode** -- `df` is a plain data frame (or sf object) with
#'   separate start/end latitude and longitude columns; pass their names
#'   via `startLatCol`/`startLonCol`/`endLatCol`/`endLonCol`.
#' * **Geometry mode** -- leave all four of those `NULL` (the default)
#'   and pass `df` as an `sf` object instead. The bearing is computed
#'   from the *first* and *last* vertex of each feature's geometry.
#'   A line with more than two vertices (e.g. a 3-point polyline with a
#'   bend in the middle) still just uses its endpoints -- the bend is
#'   ignored for the bearing calculation.
#'
#' In geometry mode, `df` is reprojected to EPSG:4326 first if it isn't
#' already, since the bearing formula assumes WGS84 longitude/latitude
#' in degrees. An `sf` object with no CRS set will raise an error rather
#' than silently assuming one.
#'
#' @param df Either a plain data frame with the four lat/lon columns
#'   named below (column mode), or an `sf` object (geometry mode, used
#'   when those four arguments are left `NULL`).
#' @param startLatCol,startLonCol,endLatCol,endLonCol Character, column
#'   names of the start/end latitude/longitude in `df`. Leave all four
#'   `NULL` (the default) to compute the bearing from each feature's
#'   geometry instead.
#' @param floor_divide Numeric, passed to [gauntlet::floor_divide()] to
#'   bucket the computed bearings -- e.g. `floor_divide = 5` rounds each
#'   bearing down to the nearest 5 degrees. Defaults to `1` (no
#'   bucketing).
#'
#' @return `df` with an added (or overwritten) `bearing` column,
#'   0-360 degrees clockwise from north. In geometry mode, the returned
#'   object is `df` reprojected to EPSG:4326 with `bearing` added; the
#'   geometry column itself is left otherwise untouched.
#'
#' @export
#'
#' @importFrom sf st_crs st_transform st_geometry st_coordinates
#' @importFrom purrr map_dfr
#' @importFrom gauntlet floor_divide
#'
#' @examples
#' \dontrun{
#' # --- geometry mode: bearing from each LINESTRING's first/last vertex ---
#' library(sfnetworks)
#' roxel
#' links_with_bearing <- st_calculate_heading_dataframe(roxel)
#' }
st_calculate_heading_dataframe <- function(df,
                                           startLatCol = NULL,
                                           startLonCol = NULL,
                                           endLatCol = NULL,
                                           endLonCol = NULL,
                                           floor_divide = 1) {

  uses_geom <- is.null(startLatCol) && is.null(startLonCol) &&
    is.null(endLatCol) && is.null(endLonCol)

  if (uses_geom) {

    if (!inherits(df, "sf")) {
      stop(
        "`df` must be an sf object when startLatCol/startLonCol/endLatCol/endLonCol are not supplied.",
        call. = FALSE
      )
    }
    if (is.na(sf::st_crs(df))) {
      stop("`df` has no CRS set. Assign one first, e.g. sf::st_set_crs(df, <epsg>).", call. = FALSE)
    }
    if (sf::st_crs(df) != sf::st_crs(4326)) {
      df <- sf::st_transform(df, crs = 4326)
    }

    endpoints <- purrr::map_dfr(sf::st_geometry(df), function(g) {
      coords <- sf::st_coordinates(g)[, c("X", "Y"), drop = FALSE]
      if (nrow(coords) < 2) {
        stop("Each geometry needs at least two vertices to compute a bearing.", call. = FALSE)
      }
      data.frame(
        startLon = coords[1, "X"],            startLat = coords[1, "Y"],
        endLon   = coords[nrow(coords), "X"], endLat   = coords[nrow(coords), "Y"]
      )
    })

    lon1 <- endpoints$startLon; lat1 <- endpoints$startLat
    lon2 <- endpoints$endLon;   lat2 <- endpoints$endLat

  } else {

    if (is.null(startLatCol) || is.null(startLonCol) || is.null(endLatCol) || is.null(endLonCol)) {
      stop(
        "Supply all four of startLatCol/startLonCol/endLatCol/endLonCol, or leave all four ",
        "NULL and pass `df` as an sf object instead.",
        call. = FALSE
      )
    }
    lat1 <- df[[startLatCol]]; lon1 <- df[[startLonCol]]
    lat2 <- df[[endLatCol]];   lon2 <- df[[endLonCol]]
  }

  # Convert degrees to radians
  lat1_rad <- lat1 * pi / 180
  lon1_rad <- lon1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  lon2_rad <- lon2 * pi / 180
  dlon     <- lon2_rad - lon1_rad

  # Bearing (initial course) from point 1 to point 2
  bearing <- atan2(
    sin(dlon) * cos(lat2_rad),
    cos(lat1_rad) * sin(lat2_rad) - sin(lat1_rad) * cos(lat2_rad) * cos(dlon)
  ) * (180 / pi)

  # Adjust to 0-360 degrees, then bucket per floor_divide
  bearing <- (bearing + 360) %% 360
  bearing <- gauntlet::floor_divide(bearing, floor_divide)

  df$bearing <- bearing
  df
}
