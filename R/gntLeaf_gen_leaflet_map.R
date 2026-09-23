#' Assemble layer(s) into a full leaflet + crosstalk code block
#'
#' Combines one or more [gntLeaf_gen_map_layer()] results into the complete
#' generated code: every layer's palette/SharedData/filter setup, the full
#' `leaflet()` pipe chain (Esri basemaps via [gntLeaf_add_esri_basemaps()],
#' each layer's marker/legend calls, a layers control, and mouse
#' coordinates), and -- if any layer has filters -- a `crosstalk::bscols()`
#' wrapper laying out the filter panel next to the map. If none of the
#' layers have filters, `bscols()` is skipped and the leaflet chain is
#' emitted on its own.
#'
#' The generated block is always:
#' \itemize{
#'   \item wrapped in `{ }`, so it runs as one unit and folds in the editor;
#'   \item assigned to `temp_map` (leaflet widget or `bscols()` layout);
#'   \item finished with a bare `temp_map` so the map prints when run.
#' }
#'
#' Helper calls in the output (`gntLeaf_add_esri_basemaps()`,
#' `gntLeaf_esri_basemap_names()`, `rescale_vector()`) are emitted without a
#' package prefix, so those functions must be loaded or sourced wherever the
#' generated code is run.
#'
#' Prints the result via [gntLeaf_emit()] and, when `clip = TRUE`, also
#' copies it to the clipboard.
#'
#' @param layers list of lists, each the return value of
#'   [gntLeaf_gen_map_layer()].
#' @param height numeric. `leaflet(height = ...)`, in pixels. Default 700.
#' @param basemap_default character. Basemap shown on load; one of
#'   `"gray"` (default), `"streets"`, `"dark"`, `"imagery"`. Also listed
#'   first in the layers control.
#' @param widths numeric length-2. `bscols(widths = ...)` for the filter
#'   panel and map, on Bootstrap's 12-column grid. Only used when at least
#'   one layer has filters. Default `c(3, 9)`.
#' @param clip logical. Copy the result to the clipboard via clipr.
#'   Default `TRUE`.
#'
#' @return Invisibly, the full generated code block as a single character
#'   string (also printed to the console as a side effect, and copied to
#'   the clipboard when `clip = TRUE`).
#'
#' @seealso [gntLeaf_gen_map_layer()], [gntLeaf_add_esri_basemaps()],
#'   [gntLeaf_esri_basemap_names()], [gntLeaf_emit()]
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom purrr map_chr map
#'
#' @examples
#' \dontrun{
#' # `breweries` (points) and `franconia` (polygons) are the sample sf
#' # datasets bundled with {mapview}. Neither generator evaluates the data --
#' # only object and column names are used, as strings -- so these calls run
#' # without the datasets loaded. Running the *generated* code does need them.
#'
#' layer_breweries = gntLeaf_gen_map_layer(
#'   data_name = "breweries"
#'   ,palette_col = "number.of.types"
#'   ,geom = "point"
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
#' # single layer, no filters -- plain leaflet chain, no bscols()
#' gntLeaf_gen_leaflet_map(list(layer_franconia))
#'
#' # single layer with filters -- wrapped in bscols()
#' gntLeaf_gen_leaflet_map(list(layer_breweries))
#'
#' # combined, dark basemap, wider filter panel, no clipboard
#' gntLeaf_gen_leaflet_map(
#'   list(layer_franconia, layer_breweries)
#'   ,height = 800
#'   ,basemap_default = "dark"
#'   ,widths = c(4, 8)
#'   ,clip = FALSE
#' )
#' }
gntLeaf_gen_leaflet_map = function(
    layers
    ,height = 700
    ,basemap_default = c("gray", "streets", "dark", "imagery")
    ,widths = c(3, 9)
    ,clip = T
){

  basemap_default = match.arg(basemap_default)

  layer_blocks = paste(purrr::map_chr(layers, "code"), collapse = "\n\n")
  marker_chain = paste(purrr::map_chr(layers, "marker_call"), collapse = " %>%\n    ")
  group_names = paste0('"', purrr::map_chr(layers, "group"), '"', collapse = ", ")

  map_chain = glue::glue(
    'leaflet::leaflet(height = {height}) %>%
    gntLeaf_add_esri_basemaps(default = "{basemap_default}", control = FALSE) %>%
    {marker_chain} %>%
    leaflet::addLayersControl(
      baseGroups = gntLeaf_esri_basemap_names("{basemap_default}")
      ,overlayGroups = c({group_names})
      ,options = leaflet::layersControlOptions(collapsed = F, sortLayers = F)) %>%
    leafem::addMouseCoordinates()'
  )

  slider_vars = unlist(purrr::map(layers, "slider_vars"))

  map_block = if(length(slider_vars) == 0){
    glue::glue("temp_map = {map_chain}")
  } else {
    slider_list_lines = paste0(
      "    ", slider_vars[1]
      ,if(length(slider_vars) > 1){
        paste0("\n    ,", slider_vars[-1], collapse = "")
      } else ""
    )
    glue::glue(
      'temp_map = crosstalk::bscols(
  widths = c({paste(widths, collapse = ", ")})
  ,list(
{slider_list_lines}
  )
  ,{map_chain}
)'
    )
  }

  # wrap everything in { } so it runs (and folds) as one block;
  # plain paste0 here because literal braces would clash with glue syntax
  full = paste0(
    "{\n"
    ,layer_blocks, "\n\n"
    ,map_block, "\n\n"
    ,"temp_map\n"
    ,"}"
  )

  gntLeaf_emit(as.character(full), clip)
}
