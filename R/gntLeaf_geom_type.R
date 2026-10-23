#' Detect the leaflet geometry type of an sf object
#'
#' Returns the most common geometry type in `data`, collapsed to the three
#' types the generators draw: `"point"` (POINT/MULTIPOINT), `"line"`
#' (LINESTRING/MULTILINESTRING), or `"polygon"` (POLYGON/MULTIPOLYGON).
#' Used by [gntLeaf_gen_map_layer()] so `geom` never has to be passed by hand.
#' Replaces the script-level `temp_geom_lookup()`.
#'
#' Warns when a layer mixes geometry types, since only the most common type
#' will be drawn correctly.
#'
#' @param data An sf object.
#'
#' @return Character scalar: `"point"`, `"line"`, or `"polygon"`.
#' @export
#'
#' @importFrom sf st_geometry_type
#'
#' @examples
#' \dontrun{
#' gntLeaf_geom_type(mapview::breweries) # "point"
#' gntLeaf_geom_type(mapview::franconia) # "polygon"
#' }
gntLeaf_geom_type = function(data){
  if(!inherits(data, "sf")) stop("data must be an sf object", call. = FALSE)

  types = as.character(sf::st_geometry_type(data))
  family = ifelse(types %in% c("POINT", "MULTIPOINT"), "point"
                  ,ifelse(types %in% c("LINESTRING", "MULTILINESTRING"), "line"
                          ,ifelse(types %in% c("POLYGON", "MULTIPOLYGON"), "polygon"
                                  ,NA_character_)))
  if(all(is.na(family))){
    stop(sprintf("unsupported geometry type(s): %s"
                 ,paste(unique(types), collapse = ", ")), call. = FALSE)
  }
  tbl = table(family)
  out = names(tbl)[which.max(tbl)]

  if(length(tbl) > 1 || anyNA(family)){
    warning(sprintf("mixed geometry types (%s); drawing as %s"
                    ,paste(unique(types), collapse = ", "), out), call. = FALSE)
  }
  out
}
