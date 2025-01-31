#' Spatially aggregate spatial data into counts via honeycombs or squares.
#'
#' @param data an SF object
#' @param map_back_crs an integer indicating which crs for the aggregated counts to be returned in - default is `4326`
#' @param honey_crs an integer indicating which crs to use in aggregation, aggregation cannot be perfomed useing lat/long, must be ft or m - default is `2285`
#' @param honey_dim a integer (in honey_crs units) indicating size of honeycomb or square
#' @param honey_type a boolean indicating what type of honeycomb to use - honeycomb (`F`) or square (`T`)
#'
#' @return an SF object (multipolygon) with counts per honeycomb unit
#' @export
#'
#' @examples
#' \dontrun{
#'mapview::breweries %>%
#'  make_honeycomb_counts(honey_type = T) %>%
#' filter(count > 0) %>%
#'  mapview::mapview(zcol = "count")
#'  }
make_honeycomb_counts = function(data, map_back_crs = 4326
                                 ,honey_crs = 2285, honey_dim = 10000, honey_type = F){

  area_honeycomb_grid =
    data %>%
    st_transform(honey_crs) %>%
    st_make_grid(c(dim, dim), what = "polygons", square = honey_type) %>%
    st_sf() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(grid_id = row.names(.)) %>%
    st_transform(map_back_crs)

  temp = st_join(area_honeycomb_grid, data %>%
                   mutate(flag = 1)) %>%
    mutate(flag = replace_na(flag, 0)) %>%
    sf::st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(count = sum(flag), .groups = "drop")


  area_honeycomb_grid_object = area_honeycomb_grid %>%
    merge(temp, by = "grid_id", all = T) %>%
    arrange(desc(count))

  return(area_honeycomb_grid_object)
}
