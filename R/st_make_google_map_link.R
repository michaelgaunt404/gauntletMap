#' Create Google Maps links from coordinate columns
#'
#' Generates an HTML anchor tag linking to Google Maps for each row in a data
#' frame or sf object, using longitude and latitude coordinate columns.
#' This is useful for embedding clickable map links in tables, popups,
#' or HTML-based outputs (e.g., Shiny, DT, Quarto).
#'
#' @param data A data frame or \code{sf} object containing coordinate columns.
#' @param coords A character vector of length 2 giving the longitude and latitude
#'   column names, in that order (e.g., \code{c("lon", "lat")}).
#'
#' @return The input \code{data} with an additional column \code{gmap_link}
#'   containing an \code{htmltools::HTML} object linking to Google Maps.
#'
#' @details
#' The generated links use the Google Maps Search API format:
#' \url{https://www.google.com/maps/search/?api=1&query=lat,lon}.
#'
#' This function does not perform coordinate validation and assumes the
#' supplied columns are numeric and in decimal degrees.
#'
#' @examples
#' \dontRun{
#' breweries %>%
#'   gauntletMap::st_extract_coords() %>%
#'   gauntletMap::st_make_google_map_link(coords = c("lon", "lat"))
#' }
#'
#' @importFrom dplyr rowwise mutate ungroup
#' @importFrom htmltools HTML
#'
#' @export
st_make_google_map_link <- function(data, coords) {
  coord_1 <- coords[1]
  coord_2 <- coords[2]

  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      gmap_link = htmltools::HTML(
        paste0(
          "<a href='https://www.google.com/maps/search/?api=1&query=",
          !!as.symbol(coord_1), ",", !!as.symbol(coord_2),
          "' target='_blank'>Google Maps</a>"
        )
      )
    ) %>%
    dplyr::ungroup()
}
