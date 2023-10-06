



#' link_locator
#'
#' A function that references/codes/identifies referenced network links using a known spatial network.
#'
#' @param search_list list of known links to use to reference unknown links with - used to map over reference network with known links
#' @param geo_data_known an sf object depicting network of known links
#' @param geo_data_known_col name of column that contains names contained in search_list
#' @param geo_data_unknown an sf object depicting network of unknown links that you want identified and located
#' @param geo_data_unknown_id_col name of primary key column in unknown link sf object
#' @param large_buffer a numeric value - should be sufficiently large
#' @param small_buffer a numeric value - should be small to ensure only link of interest is geolocated
#'
#' @return an sf object of referenced links
#' @export
#'
#' @examples
#' #work in progress
link_locator = function(search_list
                        ,geo_data_known, geo_data_known_col
                        ,geo_data_unknown, geo_data_unknown_id_col
                        ,large_buffer = 10000, small_buffer = 30){
  search_list %>%
    map(~{
      temp_buffer = geo_data_known %>%
        filter({{geo_data_known_col}} == .x) %>%
        self_buffer(rad_bg = large_buffer, rad_sm = small_buffer, nm = ".x")

      geo_data_unknown %>%
        select({{geo_data_unknown_id_col}}) %>%
        st_filter(temp_buffer[[1]]) %>%
        st_join(temp_buffer[[2]]) %>%
        filter(is.na(buffer)) %>%
        mutate(link_id = .x %>%
                 as.factor())
    }) %>%
    reduce(rbind)
}

show_identified_links = function(){
  link_ids = identified_links$link_id

  binpal = colorFactor(
    rev(viridis::plasma(
      length(
        levels( link_ids ))))
    ,link_ids)

  leaflet(identified_links #%>% st_jitter(factor = 0.00005)
  ) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolylines(color = ~binpal(link_id)
                 ,opacity = .5
                 ,label = ~str_glue("{link_id}")
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
                 ,popup = popupTable(identified_links)) %>%
    addLegend(
      position = "bottomright",
      title = "Link_id",
      pal = binpal,
      opacity = 0.7,
      values = ~link_id)
}













