









#' Read and Filter a Spatial Layer from an OSM PBF File
#'
#' Reads a single layer from a local OSM `.pbf` file using
#' [osmextract::oe_read()], optionally filtering rows via a spatial boundary,
#' a SQL `WHERE` clause, and/or a set of regex patterns pushed down into the
#' SQL query. The result is transformed to a target CRS and repaired with
#' [sf::st_make_valid()].
#'
#' @param pbf_path Character. Path to a local `.pbf` file, typically produced
#'   by [osmextract_get_pbf()].
#' @param boundary An `sf` or `sfc` object used to spatially filter the layer.
#'   Passed to [osmextract::oe_read()] as a `"spat"` boundary after applying
#'   [sf::st_union()].
#' @param layer Character. The OSM layer to read. Common values are
#'   `"multipolygons"`, `"lines"`, `"points"`, and `"other_relations"`.
#'   Defaults to `"multipolygons"`.
#' @param str_query Character. A SQL `WHERE` clause fragment used to filter
#'   rows at read time (e.g. `"amenity IS NOT NULL"`). Injected directly into
#'   the query as `SELECT * FROM <layer> WHERE <str_query>`. If `NULL`, no
#'   `WHERE` clause is added and all rows in the layer are returned.
#' @param regex_patterns A named list of field-pattern pairs used to build
#'   additional `LIKE`-based SQL filters, applied alongside `str_query`.
#'   Each name should be a column present in the layer (e.g. `"amenity"`,
#'   `"leisure"`) and each value a regex-style pattern. Patterns are combined
#'   with `OR`. Defaults to `NULL` (no regex filtering). Note that SQLite's
#'   `LIKE` operator (used internally) is case-insensitive for ASCII but does
#'   not support full regex syntax — patterns should use `%` wildcards
#'   (e.g. `"%park%"`).
#' @param extra_tags Character vector of additional OSM tag columns to include
#'   in the output beyond the defaults returned by [osmextract::oe_read()].
#'   Defaults to `character(0)`.
#' @param crs Numeric or character. EPSG code for the output CRS. Defaults to
#'   `4326` (WGS 84).
#'
#' @return An `sf` object containing the filtered features from the specified
#'   layer, transformed to `crs` and with geometries repaired via
#'   [sf::st_make_valid()].
#'
#' @note SQL `LIKE` patterns use `%` as a wildcard, not `.` or `.*` as in R
#'   regex. For example, to match any value containing "park" use `"%park%"`.
#'   If you need full R regex filtering, apply [dplyr::filter()] with
#'   [stringr::str_detect()] on the returned object.
#'
#' @seealso [osmextract_get_pbf()], [osmextract::oe_read()], [sf::st_make_valid()]
#'
#' @importFrom osmextract oe_read
#' @importFrom sf st_union st_transform st_make_valid
#' @importFrom stringr str_glue
#' @importFrom purrr imap
#'
#' @examples
#' \dontrun{
#' pbf_path <- osmextract_get_pbf(region = "us/hawaii")
#'
#' # Read all parks as polygons within a boundary
#' parks <- osmextract_spatial_layer(
#'   pbf_path       = pbf_path,
#'   boundary       = my_boundary_sf,
#'   layer          = "multipolygons",
#'   str_query      = "leisure IS NOT NULL",
#'   regex_patterns = list(leisure = "%park%")
#' )
#'
#' # Read road lines, no regex filter needed
#' roads <- osmextract_spatial_layer(
#'   pbf_path  = pbf_path,
#'   boundary  = my_boundary_sf,
#'   layer     = "lines",
#'   str_query = "highway IS NOT NULL"
#' )
#'
#' # Read points with extra OSM tags and a custom output CRS
#' cafes <- osmextract_spatial_layer(
#'   pbf_path       = pbf_path,
#'   boundary       = my_boundary_sf,
#'   layer          = "points",
#'   str_query      = "amenity IS NOT NULL",
#'   regex_patterns = list(amenity = "%cafe%", name = "%coffee%"),
#'   extra_tags     = c("opening_hours", "website"),
#'   crs            = 32604
#' )
#' }
#'
#' @export
osmextract_spatial_layer <- function(
    pbf_path,
    boundary,
    layer          = "multipolygons",
    str_query      = NULL,
    regex_patterns = NULL,
    extra_tags     = character(0),
    crs            = 4326) {

  # --- Build SQL WHERE clause -------------------------------------------
  # Start with the user-supplied predicate (if any)
  where_parts <- if (!is.null(str_query)) str_query else character(0)

  # Push regex patterns down as LIKE clauses combined with OR
  if (!is.null(regex_patterns) && length(regex_patterns) > 0) {
    like_clauses <- purrr::imap(
      regex_patterns,
      ~ stringr::str_glue("{.y} LIKE '{.x}'")
    )
    where_parts <- c(where_parts, paste0("(", paste(like_clauses, collapse = " OR "), ")"))
  }

  query <- if (length(where_parts) > 0) {
    stringr::str_glue(
      "SELECT * FROM {layer} WHERE {paste(where_parts, collapse = ' AND ')}"
    )
  } else {
    stringr::str_glue("SELECT * FROM {layer}")
  }

  message("SQL: ", query)

  # --- Read, transform, repair ------------------------------------------
  osmextract::oe_read(
    pbf_path,
    quiet         = FALSE,
    layer         = layer,
    query         = query,
    boundary      = sf::st_union(boundary),
    boundary_type = "spat",
    extra_tags    = extra_tags
  ) %>%
    sf::st_transform(crs) %>%
    sf::st_make_valid()
}










