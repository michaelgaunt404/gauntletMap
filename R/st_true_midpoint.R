#' Get the true midpoint along a curved line
#'
#' This function gets the true midpoint along a curved line represented by a spatial object.
#' It transforms the object to a new CRS, casts it to a linestring, selects the relevant columns,
#' extracts the coordinates, finds the middle point, and converts it to a spatial object with the
#' original CRS.
#'
#' @param sf_object A spatial object representing a curved line
#' @param crs An integer input indicating which CRS to use when extracting the midpoint - default is 2781
#' @return A spatial object representing the true midpoint along the curved line
#'
#' @importFrom sf st_transform st_cast st_line_sample st_coordinates st_drop_geometry st_as_sf
#'
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#'
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
#'   mapview::mapview(st_true_midpoint(curved_line))
#' }
st_true_midpoint = function(sf_object, crs = 2781){
  temp = sf_object %>%
    mutate(merge_id = row_number())

  sf_object_linestring = temp %>%
    st_transform(crs) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id)

  coords_extract = sf_object_linestring %>%
    st_line_sample(n = 5) %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    data.frame() %>%
    merge(sf_object_linestring %>%
            sf::st_drop_geometry(),
          by.x = "L1", by.y = "linestring_id") %>%
    group_by(merge_id) %>%
    mutate(n = ceiling(n()/2),
           index = row_number()) %>%
    filter(n == index) %>%
    ungroup() %>%
    select(X, Y, merge_id)

  temp %>%
    sf::st_drop_geometry() %>%
    merge(coords_extract,
          by = "merge_id") %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326)
}
