#TODO PUT INTO GAUNTLET MAP

#' Reproject longitude/latitude columns in a data frame
#'
#' Reprojects a pair of coordinate columns in `df` from one CRS to another
#' using [sf::sf_project()]. By default the reprojected coordinates
#' overwrite `lng`/`lat` in place; pass `suffix` to instead write them to
#' new columns, leaving the originals untouched.
#'
#' @param df A data frame or tibble containing the coordinate columns to
#'   reproject.
#' @param lng <[`data-masked`][rlang::args_data_masking]> Unquoted name of
#'   the longitude column in `df`.
#' @param lat <[`data-masked`][rlang::args_data_masking]> Unquoted name of
#'   the latitude column in `df`.
#' @param to Target CRS to reproject into. Anything accepted by
#'   [sf::st_crs()] (e.g. an EPSG code, proj4string, WKT, or an existing
#'   `crs` object).
#' @param from CRS the input coordinates are currently in. Anything
#'   accepted by [sf::st_crs()]. Defaults to `4326` (WGS84 lon/lat).
#' @param suffix Optional character string. If `NULL` (the default), the
#'   `lng`/`lat` columns are overwritten in place with the reprojected
#'   values. Otherwise, new columns named `paste0(lng_col, suffix)` and
#'   `paste0(lat_col, suffix)` are added and the originals are left
#'   unchanged.
#'
#' @return `df` with reprojected coordinates -- either overwriting `lng`
#'   and `lat`, or added as new columns named `<lng><suffix>` and
#'   `<lat><suffix>` when `suffix` is supplied.
#'
#' @export
#'
#' @importFrom rlang ensym sym as_string
#' @importFrom sf sf_project st_crs
#' @importFrom dplyr pull mutate
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   site = c("A", "B", "C"),
#'   lng = c(-118.2437, -122.3321, -73.9857),
#'   lat = c(34.0522, 47.6062, 40.7484)
#' )
#'
#' # overwrite in place: WGS84 lon/lat -> NAD83 / UTM zone 15N
#' df2 <- st_reproject_coords(df, lng, lat, to = 26915)
#'
#' # keep the originals, write reprojected coords to new suffixed columns
#' df3 <- st_reproject_coords(df, lng, lat, to = 26915, suffix = "_utm")
#' }
st_reproject_coords <- function(df, lng, lat, to, from = 4326, suffix = NULL) {
  lng <- rlang::ensym(lng)
  lat <- rlang::ensym(lat)

  m <- sf::sf_project(
    from = sf::st_crs(from),
    to = sf::st_crs(to),
    pts = cbind(dplyr::pull(df, !!lng), dplyr::pull(df, !!lat))
  )

  out_lng <- if (is.null(suffix)) lng else rlang::sym(paste0(rlang::as_string(lng), suffix))
  out_lat <- if (is.null(suffix)) lat else rlang::sym(paste0(rlang::as_string(lat), suffix))

  df |>
    dplyr::mutate(
      !!out_lng := m[, 1],
      !!out_lat := m[, 2]
    )
}
