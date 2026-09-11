#' Add a clickable Google Maps search link column
#'
#' Builds an HTML `<a>` link to a Google Maps search for each row's
#' coordinates and adds it as a new `gmap_link` column, marked as HTML
#' (the same `c("html", "character")` class [htmltools::HTML()] would
#' set) so it renders as a link (e.g. in a `DT`/`reactable` table or an R
#' Markdown/Quarto HTML output) rather than as literal text.
#'
#' Note: the class is attached directly with `class<-()` rather than by
#' calling `htmltools::HTML()` on the whole vector. `HTML()` concatenates
#' all of its input into a *single* string -- fine for one link, but
#' calling it on an n-row vector collapses all n links into one string
#' and recycles that same value into every row. Attaching the class
#' after building the vector with `paste0()` keeps each row's link
#' separate.
#'
#' Works two ways, depending on `data`:
#' * A plain data frame/tibble with separate latitude and longitude
#'   columns -- pass their names via `coords`.
#' * An `sf` object with `POINT` geometry -- leave `coords = NULL` and the
#'   coordinates are read directly from the geometry column via
#'   [sf::st_coordinates()].
#'
#' @param data A data frame, tibble, or `sf` object. If `sf`, its
#'   geometry column must be `POINT` (not `MULTIPOINT`, lines, or
#'   polygons).
#' @param coords Character vector of length 2, giving the column names in
#'   `data` to use for the link, in the order Google Maps expects them:
#'   `coords[1]` should be the **latitude** column and `coords[2]` the
#'   **longitude** column (Google's query format is `lat,lng`). Swapping
#'   the order will silently produce a link pointing at the wrong spot.
#'   Ignored (and can be left `NULL`, the default) when `data` is an
#'   `sf` object -- coordinates are taken from the geometry column
#'   instead.
#'
#' @return `data` with an added `gmap_link` column of class `"html"`
#'   (from [htmltools::HTML()]), one Google Maps search link per row. If
#'   `data` was an `sf` object, the result stays an `sf` object with its
#'   geometry column intact.
#'
#' @export
#'
#' @importFrom dplyr mutate pull
#' @importFrom rlang .data
#' @importFrom sf st_geometry_type st_coordinates
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   site = c("A", "B", "C"),
#'   lat = c(34.0522, 47.6062, 40.7484),
#'   lng = c(-118.2437, -122.3321, -73.9857)
#' )
#'
#' # plain data frame: supply the lat/lng column names
#' df_with_links <- st_make_google_map_link(df, coords = c("lat", "lng"))
#'
#' # sf POINT object: coordinates come from the geometry column
#' pts <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
#' pts_with_links <- st_make_google_map_link(pts)
#' }
st_make_google_map_link <- function(data, coords = NULL) {

  if (inherits(data, "sf")) {

    geom_types <- unique(as.character(sf::st_geometry_type(data)))
    if (!all(geom_types == "POINT")) {
      stop(
        "`data` is an sf object with non-POINT geometry (", paste(geom_types, collapse = ", "),
        "). st_make_google_map_link() only supports POINT geometries.",
        call. = FALSE
      )
    }

    xy <- sf::st_coordinates(data)
    lat <- xy[, "Y"]
    lng <- xy[, "X"]

    gmap_link <- paste0(
      "<a href='https://www.google.com/maps/search/?api=1&query=",
      lat, ",", lng,
      "'>Google Maps</a>"
    )
    class(gmap_link) <- c("html", "character")

    dplyr::mutate(data, gmap_link = gmap_link)

  } else {

    if (is.null(coords)) {
      stop(
        "`coords` must be supplied (character vector of length 2: c(lat_col, lng_col)) when `data` is not an sf object.",
        call. = FALSE
      )
    }
    coord_1 <- coords[1]
    coord_2 <- coords[2]

    gmap_link <- paste0(
      "<a href='https://www.google.com/maps/search/?api=1&query=",
      dplyr::pull(data, .data[[coord_1]]), ",", dplyr::pull(data, .data[[coord_2]]),
      "'>Google Maps</a>"
    )
    class(gmap_link) <- c("html", "character")

    dplyr::mutate(data, gmap_link = gmap_link)
  }
}
