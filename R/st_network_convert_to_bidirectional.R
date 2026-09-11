#' st_network_convert_to_bidirectional
#'
#' Combine directional edges into undirected network representation
#'
#' This function takes a spatial network consisting of potentially duplicated, directional links (e.g., from Replica, where volumes are provided separately for each direction) and aggregates them into a single undirected representation.
#'
#' It does this by:
#' - Calculating min/max lat/lon to normalize the start and end point geometry.
#' - Dropping geometry and grouping by key identifying attributes to collapse matching directional links.
#' - Summing the count attribute and retaining only one record per grouped feature.
#' - Re-assigning spatial geometry after aggregation using st_as_sf().
#'
#' @param sf_network An sf object representing the directional network. Must contain the following columns:
#' - startLat, endLat
#' - startLon, endLon
#' - geometry column (LINESTRING)
#' - streetName, distance, osmid, flags, highway, mode, count
#'
#' @return An sf object where bidirectional links have been collapsed into single records, with summed count values.
#'
#' @details
#' This was developed to work with Replica’s network export, which includes two unidirectional links (A→B and B→A) that are geometrically identical but reversed.
#'
#' Only works correctly if:
#' - Geometries of opposing directions are identical (no offsets).
#' - Grouping variables uniquely identify a pair of opposite links (no other data differentiates them).
#'
#' ### Output structure
#' - Geometry: LINESTRING (sf object)
#' - Summed count column
#' - Added index column (always 1 after filter)
#' - Can add optional diagnostics such as n_links to flag when multiple directional links were merged
#'
#' ### Considerations for Generalization
#' - group_by() fields are hardcoded for Replica. Consider exposing these as arguments.
#' - Add optional column (n_links) to indicate how many records were collapsed.
#' - Consider checks for missing required fields (startLat, endLat, etc.).
#'
#' @examples
#' r #' st_network_convert_to_bidirectional(sf_network = sf_network_vols) #'
#'
#' @export
st_network_convert_to_bidirectional = function(sf_network){
  
  # Check required columns exist
  
  required_cols = c("startLat", "endLat", "startLon", "endLon", "geometry")
  missing_cols = setdiff(required_cols, names(sf_network))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  sf_network %>%
    mutate(
      min_lat = pmin(startLat, endLat),
      max_lat = pmax(startLat, endLat),
      min_lon = pmin(startLon, endLon),
      max_lon = pmax(startLon, endLon)
    ) %>%
    mutate(geo_text = st_as_text(geometry)) %>%
    st_drop_geometry() %>%
    group_by(
      streetName, distance, osmid, flags, highway, mode,
      min_lat, max_lat, min_lon, max_lon
    ) %>%
    mutate(
      count = sum(count),
      n_links = n(), # Optional diagnostic
      index = row_number()
    ) %>%
    filter(index == 1) %>%
    ungroup() %>%
    st_as_sf(wkt = "geo_text", crs = 4326) %>%
    rename(geometry = geo_text)
}