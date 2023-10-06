#' Convert LineString to Sampled Points
#'
#' Converts a LineString spatial object into a set of sampled points along the LineString.
#'
#' @param sf_object A spatial object of class 'sf' representing LineString geometry.
#' @param samp_dist The sampling distance, specifying the distance between consecutive sampled points along the LineString. The default value is 100.
#' @param crs The Coordinate Reference System (CRS) for the LineString geometry. If not specified, the function assumes the CRS of the input sf_object.
#'
#' @return A spatial object of class 'sf' representing the sampled points along the LineString.
#'
#' @details The `st_line_sample_to_points` function takes a LineString spatial object and samples points along the LineString at a specified distance (`samp_dist`). The function first converts the input `sf_object` to LineString geometry by using the `st_cast` function and assigns a unique ID (`linestring_id`) to each LineString. It also creates a merge ID (`merge_id`) that corresponds to each LineString.
#'
#' Next, the function samples points along each LineString using the `st_line_sample` function. The density of sampling is determined by the inverse of the `samp_dist` parameter. The resulting sampled points are then transformed to EPSG code 4326 (WGS84) and converted to a spatial object of class 'sf'.
#'
#' Additional attributes, such as the `shape_id` from the original `sf_object_linestring`, are added to the sampled points using the `bind_cols` function. The resulting spatial object is then converted to Point geometry using the `st_cast` function and assigns an index (`index`) to each point. Finally, the function uses the `gauntlet::st_extract_coords` function to extract the longitude and latitude coordinates of the points, renaming them as `samp_lon` and `samp_lat`.
#'
#' This function simplifies the process of sampling points along LineString geometries, which can be useful for various spatial analysis and visualization tasks.
#'
#' @import sf
#' @import gauntlet
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr st_drop_geometry
#'
#' @examples
#' # Convert LineString to sampled points
#' sampled_points <- st_line_sample_to_points(line_string_data, samp_dist = 100)
#'
#' @export
st_line_sample_to_points = function(sf_object, samp_dist = 100, crs){
  sf_object_linestring = sf_object %>%
    mutate(merge_id = row_number()) %>%
    st_transform(crs) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id, shape_id)

  sf_object_points = sf_object_linestring %>%
    st_line_sample(density = 1/samp_dist) %>%
    st_transform(4326) %>%
    st_as_sf() %>%
    bind_cols(sf_object_linestring %>%
                st_drop_geometry() %>%
                select(shape_id)) %>%
    st_cast("POINT") %>%
    mutate(index = row_number()) %>%
    rename(geometry = x) %>%
    gauntlet::st_extract_coords() %>%
    rename(samp_lon = lon, samp_lat = lat)
}
