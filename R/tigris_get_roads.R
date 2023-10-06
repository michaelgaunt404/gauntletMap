#' Acquire roads data for a specific state-county pair.
#'
#' This function takes a state (either numeric or a US state code) and a county FIPS code index.
#' It retrieves the roads data for the specified state-county pair using the TIGRIS R package,
#' which simplifies the process of obtaining US Tiger shapefiles.
#'
#' @param state A numeric representation of the state or a US state code (e.g., "WA" for Washington).
#' @param index The county FIPS code index for the desired county.
#'
#' @return A list of roads data for the specified state-county pair, with one element per county.
#'
#' @importFrom tigris roads
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' # Acquire roads data for King County, Washington (FIPS code 033)
#' king_county_roads <- tigris_get_roads("WA", 033)
#' }
#'
#' @export
tigris_get_roads = function(state, index){
  index %>%
    purrr::map(~tigris::roads(state = state, county = .x))
}



