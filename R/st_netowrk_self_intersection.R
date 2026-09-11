#' Identify and Cut Self-Intersections in a Spatial Network
#'
#' This function processes a spatial network represented as an `sf` object of 
#' line segments, identifies valid intersection points (vertices), and performs 
#' a network cutting operation at those intersections. It is designed to 
#' improve network connectivity by distinguishing between true intersections 
#' (e.g., surface-level crossings) and false intersections (e.g., flyovers, 
#' bridges, or ramps that overlap geometrically but do not intersect physically).
#'
#' The function:
#' \itemize{
#'   \item Validates the presence and correctness of a `flag_perform_intersect` column.
#'   \item Converts the network to a node-edge representation via [sfnetworks::as_sfnetwork()].
#'   \item Identifies potential intersection points using [sf::st_intersection()].
#'   \item Differentiates between original and new vertices, discarding false intersections.
#'   \item Snaps line segments to valid intersection points and performs a cutting
#'         operation using [lwgeom::st_split()].
#' }
#'
#' The result is a list containing the updated, cut network and data frames of
#' old and new vertices, filtered by whether they represent valid connection points.
#'
#' @param temp_network An `sf` object containing LINESTRING geometries representing
#'   a spatial network. Must include a column named `flag_perform_intersect` with
#'   values `"yes"` or `"no"`.
#'
#' @return A list with two main components:
#' \describe{
#'   \item{`network_cut`}{An `sf` object of cut network segments after processing.}
#'   \item{`points`}{A list of `sf` objects including:
#'     \itemize{
#'       \item `network_nodes_old`: Original vertices identified as valid.
#'       \item `network_nodes_new_snap_no`: New vertices not snapped (excluded).
#'       \item `network_nodes_new_snap`: New vertices snapped into the network.
#'     }}
#' }
#'
#' @importFrom sf st_as_sf st_intersection st_union st_filter st_cast st_collection_extract
#' @importFrom sfnetworks as_sfnetwork
#' @importFrom dplyr filter mutate select bind_rows across if_any
#' @importFrom readr parse_number
#' @importFrom stringr str_detect str_glue 
#' @importFrom tidyr separate
#' @importFrom janitor remove_empty
#' @importFrom lwgeom st_split
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' library(sf)
#' library(dplyr)
#'
#' # Load or create a simple line network
#' temp_network <- st_read("path/to/your/network_layer.gpkg") %>%
#'   mutate(flag_perform_intersect = if_else(row_number() %% 2 == 0, "yes", "no"))
#'
#' # Run the self-intersection process
#' result <- st_netowrk_self_intersection(temp_network)
#'
#' # Access results
#' plot(result$network_cut["geometry"])
#' }
st_netowrk_self_intersection <- function(temp_network) {
  message(str_glue("Starting self-intersection process \n{Sys.time()}"))
  
  # step_0=======================================================================
  stopifnot("Network needs a column named 'flag_perform_intersect' - currently missing...." =
              any(colnames(temp_network) %in% c("flag_perform_intersect")))
  
  stopifnot("Column 'flag_perform_intersect' must contain 'yes' and/or 'no' - currently missing...." =
              any(unique(temp_network$flag_perform_intersect) %in% c("yes", "no")))
  
  # step_1=======================================================================
  message(str_glue("Step 1: Extracting initial connection nodes"))
  nodes_original <- temp_network %>%
    sfnetworks::as_sfnetwork(FALSE) %>%
    sf::st_as_sf("nodes")
  
  # step_2=======================================================================
  message(str_glue("Step 2: Identifying intersection points"))
  temp_network_subset_int <- temp_network %>% sf::st_intersection()
  
  # step_3=======================================================================
  message(str_glue("Step 3: Processing network edges"))
  temp_lines_snap <- temp_network_subset_int %>%
    dplyr::filter(!stringr::str_detect(sf::st_as_text(geometry), "POINT")) %>%
    dplyr::filter(flag_perform_intersect == "yes") %>%
    tidyr::separate(col = "origins", into = paste0("org_", 1:10), sep = ", ") %>%
    janitor::remove_empty("cols")
  
  temp_lines_snap_no <- temp_network_subset_int %>%
    dplyr::filter(!stringr::str_detect(sf::st_as_text(geometry), "POINT")) %>%
    dplyr::filter(!(osm_id %in% temp_lines_snap$osm_id)) %>%
    tidyr::separate(col = "origins", into = paste0("org_", 1:10), sep = ", ") %>%
    janitor::remove_empty("cols")
  
  # step_4=======================================================================
  message(str_glue("Step 4: Processing network nodes"))
  temp_points <- temp_network_subset_int %>%
    dplyr::filter(stringr::str_detect(sf::st_as_text(geometry), "POINT")) %>%
    dplyr::mutate(index_snap_point = dplyr::row_number()) %>%
    tidyr::separate(col = "origins", into = paste0("org_", 1:10), sep = ", ") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("org_"), readr::parse_number)) %>%
    janitor::remove_empty("cols")
  
  temp_snap_og <- temp_points %>%
    sf::st_filter(nodes_original %>%
                    gauntletMap::st_quick_buffer(32605, 32605, rad = 3)) %>%
    dplyr::mutate(point_type = "old", flag_snap = "yes")
  
  temp_snap_new <- temp_points %>%
    dplyr::filter(!(index_snap_point %in% temp_snap_og$index_snap_point)) %>%
    dplyr::mutate(point_type = "new")
  
  temp_snap_new_snap_no <- temp_snap_new %>%
    dplyr::filter(dplyr::if_any(dplyr::starts_with("org_"),
                                ~ .x %in% temp_lines_snap_no$org_1)) %>%
    dplyr::select(index_snap_point, point_type) %>%
    dplyr::mutate(flag_snap = "no")
  
  temp_snap_new_snap <- temp_snap_new %>%
    dplyr::filter(!(index_snap_point %in% temp_snap_new_snap_no$index_snap_point)) %>%
    dplyr::select(index_snap_point, point_type) %>%
    dplyr::mutate(flag_snap = "yes")
  
  # step_5=======================================================================
  message(str_glue("Step 5: Performing WKT cutting procedure"))
  temp_points_use_to_snap <- dplyr::bind_rows(temp_snap_new_snap, temp_snap_og)
  
  temp_lines_snapped <- dplyr::bind_rows(temp_lines_snap, temp_lines_snap_no) %>%
    sf::st_snap(., sf::st_union(temp_points_use_to_snap), tolerance = 0.5)
  
  message(str_glue("Halfway through - splitting network: \n{Sys.time()}"))
  temp_output_cut <- lwgeom::st_split(
    temp_lines_snapped %>% sf::st_cast("LINESTRING"),
    temp_points_use_to_snap
  ) %>%
    sf::st_collection_extract("LINESTRING")
  
  temp_output <- list(
    network_cut = temp_output_cut,
    points = list(
      network_nodes_old = temp_snap_og,
      network_nodes_new_snap_no = temp_snap_new_snap_no,
      network_nodes_new_snap = temp_snap_new_snap
    )
  )
  
  return(temp_output)
}
