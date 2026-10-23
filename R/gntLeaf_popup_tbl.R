#' Create a formatted popup table for a leaflet layer
#'
#' Builds one HTML popup table per feature with [leafpop::popupTable()],
#' after dropping columns that don't belong in a popup. Replaces
#' `leaflet_popup_tbl_pretty()`.
#'
#' By default the HTML `label` column (already shown on hover) and any
#' leftover `wkt` text column are dropped, and geometry is always removed.
#' Works on sf objects, plain data frames / data.tables, and crosstalk
#' `SharedData` objects (their original data is used).
#'
#' Rows stay in the input's order, so the result lines up with the layer as
#' long as you pass the same object the layer draws (the generators emit
#' `gntLeaf_popup_tbl({data_name})` for you).
#'
#' @param data An sf object, data frame, or crosstalk `SharedData`.
#' @param drop character. Columns to remove (matched before name cleaning).
#'   Default `c("label", "wkt")`; missing columns are ignored.
#' @param keep character or NULL. If given, only these columns are kept
#'   (applied before `drop`).
#' @param clean_names logical. Run [janitor::clean_names()] on the column
#'   names shown in the popup. Default `TRUE`.
#' @param ... Passed to [leafpop::popupTable()] (e.g.
#'   `row.numbers = FALSE`, `feature.id = FALSE`).
#'
#' @return A character vector of HTML popup tables, one per row.
#' @export
#'
#' @importFrom leafpop popupTable
#' @importFrom janitor clean_names
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' \dontrun{
#' mapview::breweries %>%
#'   leaflet::leaflet() %>%
#'   leaflet::addCircleMarkers(popup = gntLeaf_popup_tbl(mapview::breweries))
#'
#' gntLeaf_popup_tbl(temp_cor, keep = c("street_std", "map_per_mile_injured"))
#' }
gntLeaf_popup_tbl = function(
    data
    ,drop = c("label", "wkt")
    ,keep = NULL
    ,clean_names = TRUE
    ,...
){
  if(inherits(data, "SharedData")) data = data$origData()
  if(inherits(data, "sf")) data = sf::st_drop_geometry(data)
  data = as.data.frame(data)

  if(!is.null(keep)){
    missing = setdiff(keep, names(data))
    if(length(missing) > 0){
      warning(sprintf("keep column(s) not found: %s", paste(missing, collapse = ", "))
              ,call. = FALSE)
    }
    data = data[, intersect(keep, names(data)), drop = FALSE]
  }
  data = data[, setdiff(names(data), drop), drop = FALSE]
  if(isTRUE(clean_names)) data = janitor::clean_names(data)

  leafpop::popupTable(data, ...)
}
