# Part of the gntLeaf_* family. Assembles one or more gntLeaf_gen_map_layer()
# results into the full crosstalk + leaflet code block, prints it, and
# (optionally) copies it to the clipboard.
#
# Depends on .emit() -- source gntLeaf_emit.R first if running these as
# loose scripts rather than as part of the gauntletMap package.
#
# Usage pattern:
#   layer_1 = gntLeaf_gen_map_layer(data_name = "breweries", palette_col = "number.of.types", geom = "point", ...)
#   layer_2 = gntLeaf_gen_map_layer(data_name = "franconia", palette_col = "district", geom = "polygon", col_type = "factor", ...)
#   gntLeaf_gen_leaflet_map(list(layer_1, layer_2))   # prints the full block

#' Assemble layer(s) into a full leaflet + crosstalk code block
#'
#' Combines one or more [gntLeaf_gen_map_layer()] results into the complete
#' generated code: all layers' palette/SharedData/filter setup, the full
#' `leaflet()` pipe chain (base tiles, each layer's marker/legend calls,
#' layers control, mouse coordinates), and -- if any layer has filters --
#' a `crosstalk::bscols()` wrapper laying out the filter panel next to the
#' map. If none of the layers have any filters, `bscols()` is skipped
#' entirely and the leaflet chain is emitted on its own. Prints the result
#' via [base::cat()] and, when `clip = TRUE`, also copies it to the
#' clipboard.
#'
#' @param layers list of lists, each the return value of
#'   [gntLeaf_gen_map_layer()].
#' @param height numeric. leaflet(height = ...).
#' @param base_tiles_pkg character, e.g. "gauntletMap".
#' @param widths numeric length-2. bscols(widths = ...).
#' @param clip logical. Copy the result to the clipboard via clipr.
#'
#' @return Invisibly, the full generated code block as a single character
#'   string (also printed to the console as a side effect, and copied to
#'   the clipboard when `clip = TRUE`).
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom purrr map_chr map
#'
#' @examples
#' \dontrun{
#' # `breweries` (points) and `franconia` (polygons) are the sample sf
#' # datasets bundled with the {mapview} package. Neither this function nor
#' # gntLeaf_gen_map_layer() evaluates the actual data -- only the object
#' # name and column names are used, as strings -- so these calls run fine
#' # even without the datasets loaded.
#'
#' layer_breweries = gntLeaf_gen_map_layer(
#'   data_name = "breweries"
#'   ,palette_col = "number.of.types"
#'   ,geom = "point"
#'   ,col_type = "numeric"
#'   ,slider_cols = c("number.of.types", "number.seasonal.beers")
#'   ,slider_types = c("n", "f")
#'   ,group = "BREWERIES"
#' )
#'
#' layer_franconia = gntLeaf_gen_map_layer(
#'   data_name = "franconia"
#'   ,palette_col = "district"
#'   ,geom = "polygon"
#'   ,col_type = "factor"
#'   ,slider_cols = character(0)
#'   ,group = "DISTRICT"
#' )
#'
#' # single layer, no filters -- bscols() skipped, plain leaflet chain
#' gntLeaf_gen_leaflet_map(list(layer_franconia))
#'
#' # single layer with filters -- wrapped in bscols()
#' gntLeaf_gen_leaflet_map(list(layer_breweries))
#'
#' # combined: both layers on one map, filters from either laid out together
#' gntLeaf_gen_leaflet_map(
#'   list(layer_franconia, layer_breweries)
#'   ,height = 700
#'   ,base_tiles_pkg = "gauntletMap"
#'   ,widths = c(3, 9)
#'   ,clip = TRUE
#' )
#' }
gntLeaf_gen_leaflet_map = function(
    layers
    ,height = 700
    ,base_tiles_pkg = "gauntletMap"
    ,widths = c(3, 9)
    ,clip = T
){
  
  layer_blocks = paste(purrr::map_chr(layers, "code"), collapse = "\n\n")
  marker_chain = paste(purrr::map_chr(layers, "marker_call"), collapse = " %>%\n    ")
  group_names = paste0('"', purrr::map_chr(layers, "group"), '"', collapse = ", ")
  
  map_chain = glue::glue(
    'leaflet::leaflet(height = {height}) %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    {base_tiles_pkg}::leaflet_default_tiles() %>%
    {marker_chain} %>%
    leaflet::addLayersControl(
      baseGroups = {base_tiles_pkg}::leaflet_default_tiles_index()
      ,overlayGroups = c({group_names})
      ,options = leaflet::layersControlOptions(collapsed = F, sortLayers = F)) %>%
    leafem::addMouseCoordinates()'
  )
  
  slider_vars = unlist(purrr::map(layers, "slider_vars"))
  
  full = if(length(slider_vars) == 0){
    glue::glue("{layer_blocks}\n\nmy_map = {map_chain}")
  } else {
    slider_list_lines = paste0(
      "    ", slider_vars[1]
      ,if(length(slider_vars) > 1){
        paste0("\n    ,", slider_vars[-1], collapse = "")
      } else ""
    )
    glue::glue(
      '{layer_blocks}

crosstalk::bscols(
  widths = c({paste(widths, collapse = ", ")})
  ,list(
{slider_list_lines}
  )
  ,{map_chain}
)'
    )
  }
  
  .emit(as.character(full), clip)
}
