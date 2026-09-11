#' Check Complexity Reduction from Spatial Simplification
#'
#' Evaluates the effect of a given simplification tolerance on an `sf` object
#' by comparing vertex (node) counts before and after applying
#' [sf::st_simplify()]. Node counts are used as the complexity metric as they
#' directly reflect what simplification removes, and are agnostic to coordinate
#' precision or storage format.
#'
#' @param data An `sf` object containing the spatial features to evaluate.
#' @param rad Numeric. The simplification tolerance in meters (passed to
#'   `dTolerance` in [sf::st_simplify()]). Larger values produce coarser
#'   geometries.
#' @param run_both Logical. If `TRUE`, also computes the node count of the
#'   original (unsimplified) geometry and prints the percentage reduction.
#'   Defaults to `FALSE`.
#' @param crs Numeric or character. The projected CRS to use before
#'   simplification, supplied as an EPSG code. Defaults to `32616` (UTM Zone
#'   16N). Should be a metric projection appropriate for your data's location.
#'
#' @return Called for its side effects. Prints messages reporting the node
#'   count of the simplified geometry, and optionally the raw node count and
#'   percentage reduction.
#'
#' @note [sf::st_simplify()] simplifies each feature independently and does
#'   not preserve shared boundaries. For polygon layers where topology matters,
#'   consider [rmapshaper::ms_simplify()] instead.
#'
#' @seealso [sf::st_simplify()], [rmapshaper::ms_simplify()]
#'
#' @importFrom sf st_geometry st_transform st_simplify st_coordinates
#' @importFrom purrr map_dbl
#' @importFrom scales percent
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(spData)
#'
#' # us_states is a polygon sf object bundled with spData
#' data("us_states", package = "spData")
#'
#' # Check node count after simplification only
#' st_check_complexity_reduction(us_states, rad = 1000)
#'
#' # Compare before and after, with % reduction
#' st_check_complexity_reduction(us_states, rad = 1000, run_both = TRUE)
#'
#' # Try a more aggressive simplification
#' st_check_complexity_reduction(us_states, rad = 5000, run_both = TRUE)
#'
#' # Use a different projected CRS (e.g. US Albers Equal Area)
#' st_check_complexity_reduction(us_states, rad = 1000, run_both = TRUE, crs = 5070)
#' }
#'
#' @export
st_check_complexity_reduction <- function(data, rad, run_both = FALSE,
                                          crs = 32616) {

  count_nodes <- function(sf_obj) {
    sf_obj %>%
      sf::st_geometry() %>%
      purrr::map_dbl(~ sf::st_coordinates(.x) %>% nrow()) %>%
      sum()
  }

  simplified <- data %>%
    sf::st_transform(crs) %>%
    sf::st_simplify(dTolerance = rad)

  n_simp <- count_nodes(simplified)

  message("Tolerance: ", rad, " meters")
  message("Nodes (simplified): ", n_simp)

  if (run_both) {
    n_raw <- data %>%
      sf::st_transform(crs) %>%
      count_nodes()

    message("Nodes (raw):        ", n_raw)
    message("Reduction:          ", scales::percent(1 - n_simp / n_raw))
  }
}
