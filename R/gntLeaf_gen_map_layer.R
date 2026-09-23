#' Generate palette + SharedData + filter + marker/legend code for one layer
#'
#' Builds the palette (`colorNumeric()`/`colorFactor()`), `SharedData` wrap,
#' any `filter_slider()`/`filter_select()` widgets, the geometry-appropriate
#' marker/line/polygon call, and its `addLegend()` -- all as generated code
#' text, for a single dataset/layer. Combine the result with one or more
#' others via [gntLeaf_gen_leaflet_map()].
#'
#' @param data_name character. Name of the sf/df object as it should appear
#'   in the generated code (e.g. "franconia") -- NOT the object itself.
#' @param palette_col character. Column the palette is built from.
#' @param geom one of "point", "line", "polygon".
#' @param col_type one of "numeric", "factor".
#' @param slider_cols character vector of columns to build filter widgets
#'   for. Defaults to `palette_col`. `character(0)` for no filters.
#' @param slider_types character vector, positionally aligned with
#'   `slider_cols`: `"n"`/`"numeric"` -> `filter_slider()`, `"f"`/`"factor"`
#'   -> `filter_select()`. Recycled if length 1.
#' @param group character. Leaflet overlay group / legend group.
#' @param slider_opts named list, keyed by column name, each an optional list
#'   with `round`, `min`, `max`, `label`, `type` overrides.
#' @param id_col character or NULL. Column for `layerId`.
#' @param palette character. viridis palette name, default "inferno".
#' @param n_colors integer, default 50. Numeric palettes only.
#' @param reverse logical, default TRUE.
#' @param size_col character or NULL. Column that drives marker radius
#'   (point) or line width (line) via [rescale_vector()]. Defaults to
#'   `palette_col`, so size tracks color. Ignored for polygons, and dropped
#'   automatically when it is `palette_col` and `col_type == "factor"`.
#'   `NULL` turns sizing off (static `radius` / `lwd` used instead).
#' @param size_range numeric length-2, `c(lower, upper)` passed to
#'   [rescale_vector()]. `NULL` (default) -> `c(3, 10)` for points,
#'   `c(5, 10)` for lines.
#' @param radius numeric. Static circle radius when sizing is off.
#' @param fill logical. Draw a fill (point/polygon only).
#' @param fill_opacity numeric. fillOpacity (point/polygon only).
#' @param color character. Polygon border stroke color, default "white".
#' @param weight numeric or NULL. Circle stroke width (point) or border width
#'   (polygon). `NULL` (default) -> 0 for points (no outline), 1 for polygons.
#' @param lwd numeric. Static line width when sizing is off (line only).
#' @param opacity numeric. Stroke/line opacity.
#' @param legend logical. Add an addLegend() for this layer's palette.
#' @param legend_title character or NULL. Defaults to title-cased `palette_col`.
#' @param legend_position character, default "bottomleft".
#'
#' @return A list with `code`, `marker_call`, `slider_vars`, and `group`.
#' @export
#'
#' @importFrom glue glue
#' @importFrom purrr map2_chr
#' @importFrom tools toTitleCase
#'
#' @examples
#' \dontrun{
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
    ,size_col = palette_col
    ,size_range = NULL
    ,radius = 6
    ,fill = TRUE
    ,fill_opacity = .7
    ,color = "white"
    ,weight = NULL
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
  if(is.null(weight)) weight = if(geom == "point") 0 else 1

  # -- value-based sizing (point radius / line width) -----------------------
  if(geom == "polygon") size_col = NULL
  if(!is.null(size_col) && identical(size_col, palette_col) && col_type == "factor") size_col = NULL
  if(is.null(size_range)) size_range = if(geom == "point") c(3, 10) else c(5, 10)
  if(length(size_range) != 2) stop("size_range must be length 2: c(lower, upper)")

  size_expr = if(!is.null(size_col)){
    as.character(glue::glue(
      'rescale_vector({data_name}${size_col}, lower = {size_range[1]}, upper = {size_range[2]})'
    ))
  } else NULL

  .norm_filter_type = function(x){
    ifelse(x %in% c("n", "numeric"), "numeric"
           ,ifelse(x %in% c("f", "factor"), "factor", NA_character_))
  }

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

  # -- palette ----------------------------------------------------------------
  pal_code = if(col_type == "numeric"){
    glue::glue(
      '{pal_name} = leaflet::colorNumeric(
  palette = viridis::{palette}(n = {n_colors})
  ,{data_name}${palette_col}
  ,reverse = {reverse}
)'
    )
  } else {
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

  # -- filters ----------------------------------------------------------------
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

  radius_val = if(!is.null(size_expr)) size_expr else radius
  line_weight_val = if(!is.null(size_expr)) size_expr else lwd

  marker_call = switch(geom,

                       point = as.character(glue::glue(
                         'leaflet::addCircleMarkers(
      data = {sd_name}{layer_id_line}
      ,color = ~{pal_name}({data_name}${palette_col})
      ,radius = {radius_val}
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
      ,weight = {line_weight_val}
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

