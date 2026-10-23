#' Assemble layer(s) into a full leaflet + crosstalk code block
#'
#' Combines [gntLeaf_gen_map_layer()] / [gntLeaf_gen_map_layers()] results
#' into the complete generated code: every layer's setup block (palette,
#' `SharedData`, filter widgets), the `leaflet()` pipe chain (Esri basemaps
#' via [gntLeaf_add_esri_basemaps()], each layer's folded section, a layers
#' control, `hideGroup()` for layers not visible on load, a scale bar, and
#' mouse coordinates), and optionally a `crosstalk::bscols()` wrapper with
#' the filter panel beside the map.
#'
#' **Two modes, one source.** With `eval = FALSE` (default) the code is
#' printed for reading and fiddling. With `eval = TRUE` the *same* text is
#' parsed and evaluated, returning the map object -- so the printed code and
#' the object can never disagree. Evaluation happens in `envir`, so the
#' generated `pal_*`, `*_sd`, and `flt_*` objects are left there too (that's
#' how a dashboard sidebar can pick up the filter widgets).
#'
#' **Any input shape.** `layers` can be a single layer, a list of layers,
#' the output of [gntLeaf_gen_map_layers()], or any nesting of those, e.g.
#' `list(layer_cor, vzn_layers, layer_aps)`. Everything is flattened in
#' order; names are ignored.
#'
#' The generated block is always wrapped in `{ }` (runs and folds as one
#' unit), assigns the leaflet to `{out_name}_leaflet` and the final object
#' to `{out_name}`, and ends with a bare `{out_name}` so it prints when run.
#'
#' Helper calls in the output (`gntLeaf_add_esri_basemaps()`,
#' `gntLeaf_esri_basemap_names()`, `rescale_vector()`,
#' `gntLeaf_popup_tbl()`) are emitted without a package prefix, so those
#' functions must be loaded wherever the generated code is run.
#'
#' @param layers A `gntLeaf_layer`, or a (nested) list of them.
#' @param height numeric. `leaflet(height = ...)`, in pixels. Default 700.
#' @param basemap_default character. Basemap shown on load; one of
#'   `"streets"` (default), `"gray"`, `"dark"`, `"imagery"`.
#' @param layout character. `"bscols"` (default) wraps filters + map in
#'   `crosstalk::bscols()` when any layer has filters; `"map"` returns the
#'   leaflet alone (use this for dashboards that place the `flt_*` widgets
#'   in their own sidebar).
#' @param widths numeric length-2. `bscols(widths = ...)` for the filter
#'   panel and map, on Bootstrap's 12-column grid. Default `c(3, 9)`.
#' @param scale_bar logical. Add `addScaleBar(position = "bottomright")`.
#'   Default `TRUE`.
#' @param out_name character. Name the generated code assigns the result
#'   to. Default `"temp_map"`.
#' @param eval logical. Evaluate the generated code and return the map
#'   object. Default `FALSE`.
#' @param print logical. Print the code via [gntLeaf_emit()]. Defaults to
#'   `!eval`.
#' @param clip logical. Also copy the printed code to the clipboard.
#'   Defaults to `print`.
#' @param envir environment. Where the code is evaluated when
#'   `eval = TRUE`. Default: the caller's environment.
#'
#' @return With `eval = FALSE`, invisibly, the generated code as a single
#'   character string. With `eval = TRUE`, the map object (a leaflet
#'   widget, or a `bscols()` layout when `layout = "bscols"` and filters
#'   exist).
#' @export
#'
#' @seealso [gntLeaf_gen_map_layer()], [gntLeaf_gen_map_layers()],
#'   [gntLeaf_add_esri_basemaps()], [gntLeaf_esri_basemap_names()],
#'   [gntLeaf_emit()]
#'
#' @examples
#' \dontrun{
#' # print one layer to fiddle with
#' gntLeaf_gen_leaflet_map(layer_cor)
#'
#' # dashboard: everything, evaluated, leaflet only (filters go in sidebar)
#' temp_leaflet = gntLeaf_gen_leaflet_map(
#'   list(layer_strava, layer_cor, vzn_layers, layer_viva, layer_aps)
#'   ,layout = "map"
#'   ,eval = TRUE
#' )
#' flt_corridor_crash_stats_boro_name # widget now exists for the sidebar
#' }
gntLeaf_gen_leaflet_map = function(
    layers
    ,height = 700
    ,basemap_default = c("streets", "gray", "dark", "imagery")
    ,layout = c("bscols", "map")
    ,widths = c(3, 9)
    ,scale_bar = TRUE
    ,out_name = "temp_map"
    ,eval = FALSE
    ,print = !eval
    ,clip = print
    ,envir = parent.frame()
){
  basemap_default = match.arg(basemap_default)
  layout = match.arg(layout)

  layers = .gntLeaf_flatten_layers(layers)
  if(length(layers) == 0) stop("no layers supplied", call. = FALSE)

  ids = vapply(layers, `[[`, character(1), "id")
  if(anyDuplicated(ids)){
    stop(sprintf("duplicate layer id(s): %s -- give the layers distinct groups"
                 ,paste(unique(ids[duplicated(ids)]), collapse = ", ")), call. = FALSE)
  }

  groups = vapply(layers, `[[`, character(1), "group")
  visible = vapply(layers, `[[`, logical(1), "visible")
  hidden = groups[!visible]
  filter_vars = unlist(lapply(layers, `[[`, "filter_vars"))
  f = .gntLeaf_fmt

  vec_txt = function(x, indent = "      "){
    paste0("c(\n", indent, paste(vapply(x, f, character(1))
                                 ,collapse = paste0("\n", indent, ",")), ")")
  }

  #setup + chain================================================================
  setup_blocks = paste(vapply(layers, `[[`, character(1), "setup_code")
                       ,collapse = "\n\n")
  layer_blocks = paste(vapply(layers, `[[`, character(1), "layer_code")
                       ,collapse = "\n")
  leaf_name = paste0(out_name, "_leaflet")

  chain = paste0(
    leaf_name, " = leaflet::leaflet(height = ", height, ") %>%\n"
    ,"  gntLeaf_add_esri_basemaps(default = ", f(basemap_default)
    ,", control = FALSE) %>%\n"
    ,layer_blocks, "\n"
    ,"  leaflet::addLayersControl(\n"
    ,"    baseGroups = gntLeaf_esri_basemap_names(", f(basemap_default), ")\n"
    ,"    ,overlayGroups = ", vec_txt(groups), "\n"
    ,"    ,options = leaflet::layersControlOptions(collapsed = F, sortLayers = F)) %>%\n"
    ,if(length(hidden) > 0) paste0("  leaflet::hideGroup(", vec_txt(hidden, "    "), ") %>%\n")
    ,if(isTRUE(scale_bar)) '  leaflet::addScaleBar(position = "bottomright") %>%\n'
    ,"  leafem::addMouseCoordinates()"
  )

  #output object================================================================
  out_block = if(layout == "bscols" && length(filter_vars) > 0){
    paste0(
      out_name, " = crosstalk::bscols(\n"
      ,"  widths = c(", paste(widths, collapse = ", "), ")\n"
      ,"  ,list(\n    ", paste(filter_vars, collapse = "\n    ,"), "\n  )\n"
      ,"  ,", leaf_name, "\n)"
    )
  } else {
    paste0(out_name, " = ", leaf_name)
  }

  full = paste0(
    "{\n"
    ,setup_blocks, "\n\n"
    ,chain, "\n\n"
    ,out_block, "\n\n"
    ,out_name, "\n"
    ,"}"
  )

  if(isTRUE(print)) gntLeaf_emit(full, clip)
  if(isTRUE(eval)) return(eval(parse(text = full), envir = envir))
  invisible(full)
}


#' @keywords internal
#' @noRd
.gntLeaf_flatten_layers = function(x){
  if(inherits(x, "gntLeaf_layer")) return(list(x))
  if(is.null(x)) return(list())
  if(!is.list(x)){
    stop("layers must be gntLeaf_layer objects or (nested) lists of them"
         ,call. = FALSE)
  }
  out = list()
  for(el in x) out = c(out, .gntLeaf_flatten_layers(el))
  out
}
