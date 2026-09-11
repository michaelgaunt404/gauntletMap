# Part of the gntLeaf_* family. Scaffolds the crosstalk + leaflet boilerplate
# that keeps coming up: a colorNumeric()/colorFactor() palette, a SharedData
# wrap, filter_slider()/filter_select()s, a geometry-appropriate
# marker/line/polygon layer, and its addLegend(). These only ever build
# strings -- they never look at your actual data -- so there's nothing to
# load or evaluate up front. Point this at column *names*, run, copy the
# printed block into your script, edit from there. Combine one or more
# results with gntLeaf_gen_leaflet_map() (see that function's own script).
#
# Usage pattern:
#   layer_1 = gntLeaf_gen_map_layer(data_name = "breweries", palette_col = "number.of.types", geom = "point", ...)
#   layer_2 = gntLeaf_gen_map_layer(data_name = "franconia", palette_col = "district", geom = "polygon", col_type = "factor", ...)
#   gntLeaf_gen_leaflet_map(list(layer_1, layer_2))   # prints the full block

#' Generate palette + SharedData + filter + marker/legend code for one layer
#'
#' Builds the palette (`colorNumeric()`/`colorFactor()`), `SharedData` wrap,
#' any `filter_slider()`/`filter_select()` widgets, the geometry-appropriate
#' marker/line/polygon call, and its `addLegend()` -- all as generated code
#' text, for a single dataset/layer. Combine the result with one or more
#' others via [gntLeaf_gen_leaflet_map()] to assemble the full crosstalk +
#' leaflet block.
#'
#' @param data_name character. Name of the sf/df object as it should appear
#'   in the generated code (e.g. "franconia") -- NOT the object itself.
#' @param palette_col character. Column the palette is built from.
#' @param geom one of "point", "line", "polygon". Determines which leaflet
#'   add*() function is used and which argument the palette color is bound
#'   to (`color` for point/line, `fillColor` for polygon).
#' @param col_type one of "numeric", "factor". "numeric" emits
#'   `leaflet::colorNumeric()` (the original behavior). "factor" emits
#'   `leaflet::colorFactor()` instead -- no `n_colors` needed there, since
#'   colorFactor derives its color count from the domain's factor levels
#'   when given a named palette string, rather than from an explicit
#'   pre-built color vector.
#' @param slider_cols character vector of columns to build filter widgets
#'   for. Defaults to `palette_col`. Pass `character(0)` for a layer that
#'   should be wrapped in SharedData but have no filters at all -- it's
#'   left out of the filter panel entirely.
#' @param slider_types character vector, positionally aligned with
#'   `slider_cols`, giving each column's filter widget: `"n"` / `"numeric"`
#'   -> `filter_slider()`, `"f"` / `"factor"` -> `filter_select()`. Recycled
#'   if shorter than `slider_cols` (so a single `"f"` applies to all of
#'   them). Example: `slider_cols = c("kde", "district")`, `slider_types =
#'   c("n", "f")` gives a numeric slider for `kde` and a select dropdown
#'   for `district`. If `NULL` (default), falls back per-column to
#'   `slider_opts[[col]]$type`, then to `col_type` when the column is
#'   `palette_col`, then to `"numeric"`.
#' @param group character. Leaflet overlay group / legend group.
#' @param slider_opts named list, keyed by column name, each element an
#'   optional list with `round`, `min`, `max`, `label`, `type` overrides.
#'   `type` works the same as `slider_types` but keyed by name instead of
#'   position; `slider_types` takes precedence when both are given for the
#'   same column.
#' @param id_col character or NULL. Column for `layerId`. NULL omits it.
#' @param palette character. viridis palette name, default "inferno". Used
#'   as a raw color vector (`viridis::{palette}(n = n_colors)`) for numeric
#'   columns, and as the bare palette-name string for factor columns since
#'   `colorFactor()` accepts viridis names directly.
#' @param n_colors integer, default 50. Only used when `col_type == "numeric"`.
#' @param reverse logical, default TRUE.
#' @param fill logical. Draw a fill (point/polygon only).
#' @param fill_opacity numeric. fillOpacity (point/polygon only).
#' @param color character. STATIC value -- for point/line this is unused
#'   (color there is palette-driven); for polygon it's the border stroke
#'   color, default "white".
#' @param weight numeric. Marker stroke width (point) or border width
#'   (polygon). Not used for lines -- see `lwd`.
#' @param lwd numeric. Line width, used only when `geom == "line"`.
#' @param opacity numeric. Stroke/line opacity for all geometries.
#' @param legend logical. Add an addLegend() for this layer's palette.
#' @param legend_title character or NULL. Defaults to a title-cased version
#'   of `palette_col`.
#' @param legend_position character, default "bottomleft".
#'
#' @return A list with `code` (palette + SharedData + filter block),
#'   `marker_call` (the add*() + addLegend() chain, no leading pipe),
#'   `slider_vars`, and `group`.
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom purrr map2_chr
#' @importFrom tools toTitleCase
#'
#' @examples
#' \dontrun{
#' # `breweries` (points) and `franconia` (polygons) are the sample sf
#' # datasets bundled with the {mapview} package, commonly used in leaflet/
#' # crosstalk tutorials. This function never evaluates the actual data --
#' # it only needs the object's name and column names as strings -- so
#' # these calls run fine even without the datasets loaded.
#'
#' # numeric palette, two filters of different types on the same layer
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
#' # factor palette, no filters at all (SharedData wrap only)
#' layer_franconia = gntLeaf_gen_map_layer(
#'   data_name = "franconia"
#'   ,palette_col = "district"
#'   ,geom = "polygon"
#'   ,col_type = "factor"
#'   ,slider_cols = character(0)
#'   ,group = "DISTRICT"
#' )
#'
#' # each layer_* is a list of generated-code pieces; combine with
#' # gntLeaf_gen_leaflet_map() to print/copy the full block
#' gntLeaf_gen_leaflet_map(list(layer_breweries))
#' gntLeaf_gen_leaflet_map(list(layer_franconia))
#' gntLeaf_gen_leaflet_map(list(layer_franconia, layer_breweries))
#' }
gntLeaf_gen_map_layer = function(
    data_name
    ,palette_col
    ,geom = c("point", "line", "polygon")
    ,col_type = c("numeric", "factor")
    ,slider_cols = NULL
    ,slider_types = NULL
    ,group = toupper(palette_col)
    ,slider_opts = list()
    ,id_col = NULL
    ,palette = "inferno"
    ,n_colors = 50
    ,reverse = TRUE
    ,fill = TRUE
    ,fill_opacity = .7
    ,color = "white"
    ,weight = 1
    ,lwd = 2
    ,opacity = .9
    ,legend = TRUE
    ,legend_title = NULL
    ,legend_position = "bottomleft"
){
  
  geom = match.arg(geom)
  col_type = match.arg(col_type)
  if(is.null(slider_cols)) slider_cols = palette_col
  if(is.null(legend_title)) legend_title = tools::toTitleCase(gsub("_", " ", palette_col))
  
  .norm_filter_type = function(x){
    ifelse(x %in% c("n", "numeric"), "numeric"
           ,ifelse(x %in% c("f", "factor"), "factor", NA_character_))
  }
  
  # slider_types is positional against slider_cols (recycled if shorter);
  # NA slots fall through to slider_opts[[col]]$type, then col_type, then
  # "numeric" inside the loop below.
  slider_types_resolved = if(length(slider_cols) == 0){
    character(0)
  } else if(is.null(slider_types)){
    rep(NA_character_, length(slider_cols))
  } else {
    if(!length(slider_types) %in% c(1, length(slider_cols))){
      stop("slider_types must be length 1 or length(slider_cols)")
    }
    out = .norm_filter_type(rep_len(slider_types, length(slider_cols)))
    if(anyNA(out)) stop('slider_types entries must be one of "n"/"numeric" or "f"/"factor"')
    out
  }
  
  sd_name = paste0(data_name, "_sd")
  pal_name = paste0("pal_", palette_col)
  
  # -- palette: colorNumeric() vs colorFactor() -----------------------------
  pal_code = if(col_type == "numeric"){
    glue::glue(
      '{pal_name} = leaflet::colorNumeric(
  palette = viridis::{palette}(n = {n_colors})
  ,{data_name}${palette_col}
  ,reverse = {reverse}
)'
    )
  } else {
    # colorFactor() picks its color count from the number of levels in the
    # domain, so a bare palette *name* is passed instead of a pre-built
    # n_colors-length vector -- viridis names are supported directly here.
    glue::glue(
      '{pal_name} = leaflet::colorFactor(
  palette = "{palette}"
  ,domain = {data_name}${palette_col}
  ,reverse = {reverse}
)'
    )
  }
  
  sd_code = glue::glue(
    '{sd_name} = {data_name} %>%
    crosstalk::SharedData$new()'
  )
  
  # -- filters: filter_slider() (numeric) vs filter_select() (factor) ------
  # per-column type precedence: slider_types[i]  >  slider_opts[[col]]$type
  #   >  col_type (only when col == palette_col)  >  "numeric"
  slider_lines = if(length(slider_cols) == 0) character(0) else purrr::map2_chr(slider_cols, slider_types_resolved, function(col, forced_type){
    opts = slider_opts[[col]]
    if(is.null(opts)) opts = list()
    label = if(is.null(opts$label)) col else opts$label
    input_id = paste0("inpt_", col)
    var_name = paste0("slider_input_", col)
    
    col_filter_type = if(!is.na(forced_type)){
      forced_type
    } else if(!is.null(opts$type)){
      .norm_filter_type(opts$type)
    } else if(identical(col, palette_col)){
      col_type
    } else "numeric"
    
    if(col_filter_type == "factor"){
      glue::glue(
        '{var_name} = crosstalk::filter_select("{input_id}", "{label}", {sd_name}, ~{col})'
      )
    } else {
      round_val = if(is.null(opts$round)) 2 else opts$round
      extras_list = list()
      if(!is.null(opts$min)) extras_list$min = opts$min
      if(!is.null(opts$max)) extras_list$max = opts$max
      extras = if(length(extras_list) > 0){
        paste0(", ", paste(names(extras_list), extras_list, sep = " = ", collapse = ", "))
      } else ""
      
      glue::glue(
        '{var_name} = crosstalk::filter_slider("{input_id}", "{label}", {sd_name}, "{col}", round = {round_val}{extras})'
      )
    }
  })
  slider_code = paste(slider_lines, collapse = "\n")
  
  layer_id_line = if(!is.null(id_col)) glue::glue("\n      ,layerId = ~{id_col}") else ""
  
  marker_call = switch(geom,
                       
                       point = as.character(glue::glue(
                         'leaflet::addCircleMarkers(
      data = {sd_name}{layer_id_line}
      ,color = ~{pal_name}({data_name}${palette_col})
      ,fill = {fill}
      ,fillOpacity = {fill_opacity}
      ,opacity = {opacity}
      ,weight = {weight}
      ,group = "{group}"
    )'
                       ))
                       
                       ,line = as.character(glue::glue(
                         'leaflet::addPolylines(
      data = {sd_name}{layer_id_line}
      ,color = ~{pal_name}({data_name}${palette_col})
      ,opacity = {opacity}
      ,weight = {lwd}
      ,group = "{group}"
    )'
                       ))
                       
                       ,polygon = as.character(glue::glue(
                         'leaflet::addPolygons(
      data = {sd_name}{layer_id_line}
      ,fillColor = ~{pal_name}({data_name}${palette_col})
      ,color = "{color}"
      ,fill = {fill}
      ,fillOpacity = {fill_opacity}
      ,opacity = {opacity}
      ,weight = {weight}
      ,group = "{group}"
    )'
                       ))
  )
  
  legend_call = if(isTRUE(legend)){
    as.character(glue::glue(
      'leaflet::addLegend(
      position = "{legend_position}"
      ,title = "{legend_title}"
      ,group = "{group}"
      ,pal = {pal_name}
      ,values = {data_name}${palette_col}
    )'
    ))
  } else NULL
  
  full_marker_call = if(!is.null(legend_call)){
    paste(marker_call, legend_call, sep = " %>%\n    ")
  } else marker_call
  
  code_sections = c(as.character(pal_code), as.character(sd_code))
  if(length(slider_lines) > 0) code_sections = c(code_sections, slider_code)
  code = paste(code_sections, collapse = "\n\n")
  
  invisible(list(
    code = code
    ,marker_call = full_marker_call
    ,slider_vars = if(length(slider_cols) == 0) character(0) else paste0("slider_input_", slider_cols)
    ,group = group
  ))
}
