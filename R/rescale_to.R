#' Rescale a numeric column to a specified maximum value.
#'
#' This function takes a numeric column and scales it so that the values in
#' the column are rescaled to a specified maximum value. It's a general-purpose
#' function, but it's commonly used to control aesthetic controls like line widths, point sizes, or color scales in Leaflet maps.
#'
#' @param column A numeric vector or column that you want to rescale.
#' @param value The maximum value to which the column will be rescaled.
#'
#' @return A numeric vector with the values rescaled to the specified maximum value.
#'
#' @import mapview
#' @import magrittr
#' @import dplyr
#' @import tigris
#' @import sf
#'
#' @export
#' @examples
#' \dontrun{
#' # Rescale a numeric column to have a maximum value of 10
#' counties <- tigris::tracts(state = "WA", county = "king")
#' counties <- counties %>%
#'   filter(ALAND < median(ALAND)) %>%
#'   mutate(ALAND_rscld = rescale_to(ALAND, 10))
#'
#' # Create a map using mapview to visualize the rescaled data
#' sf::st_centroid(counties) %>% mapview(cex = "ALAND_rscld", z = "ALAND_rscld")

#'
#' }
rescale_to <- function(column, value) {
  # Rescale the column to the specified maximum value
  scaled_column <- value / max(column) * column
  return(scaled_column)
}
