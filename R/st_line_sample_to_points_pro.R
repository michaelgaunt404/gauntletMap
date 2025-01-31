#' Sample Points Along a Linestring Geometry
#'
#' This function takes an `sf` object with LINESTRING geometries and generates
#' sampled points along each linestring at a specified distance.
#' It retains the original attributes from the input `sf` object and adds a new `index` column.
#'
#' @param sf_object An `sf` object containing LINESTRING geometries.
#' @param samp_dist A numeric value specifying the sampling distance in meters. Defaults to 100.
#'
#' @return An `sf` object containing POINT geometries, sampled at the specified distance,
#' with the original attributes and an added `index` column.
#'
#' @details This function uses `st_line_sample` to sample points along each LINESTRING geometry,
#' transforms the geometries to EPSG 4326, and retains the non-geometry attributes of the input.
#' The resulting points are assigned a sequential `index` for identification.
#'
#' @importFrom sf st_line_sample st_transform st_as_sf st_drop_geometry st_cast
#' @importFrom dplyr bind_cols select mutate row_number rename
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert LineString to sampled points
#' sampled_points <- st_line_sample_to_points_pro(line_string_data, samp_dist = 100)
#' }
st_line_sample_to_points_pro <- function(sf_object, samp_dist = 100) {
  # Function implementation
  sf_object_points = sf_object %>%
    sf::st_line_sample(density = 1/samp_dist) %>%
    sf::st_transform(4326) %>%
    sf::st_as_sf() %>%
    dplyr::bind_cols(sf_object %>%
                       sf::st_drop_geometry() %>%
                       dplyr::select(shape_id)) %>%
    sf::st_cast("POINT") %>%
    dplyr::mutate(index = row_number()) %>%
    dplyr::rename(geometry = x)

  return(sf_object_points)
}
