#' Generate the setup + leaflet code for one map layer
#'
#' Builds, as generated code text, everything one layer needs: the palette
#' (`colorNumeric()`/`colorFactor()`), the `SharedData` wrap and filter
#' widgets (only when crosstalk is on), the geometry-appropriate
#' `addCircleMarkers()`/`addPolylines()`/`addPolygons()` call, and its
#' `addLegend()`. Combine one or more results with
#' [gntLeaf_gen_leaflet_map()], which prints the code for fiddling or
#' evaluates it into a map object. For a named list of layers, use
#' [gntLeaf_gen_map_layers()].
#'
#' **The data is evaluated.** `data_name` is looked up in `envir`, so the
#' generator can detect geometry, palette type, and filter widget types,
#' and can check that every column you name exists. The object must
#' therefore be loaded when you call this.
#'
#' **Printed code shows every slot.** Slots that are off (static fill when
#' a palette is used, the rescale line when sizing is off, `layerId`,
#' `popup`, a disabled legend) are printed commented out, so any of them
#' can be switched on while fiddling. Once the result looks right, move the
#' change back into this call (`def_style`, `def_pal`, etc.) and the layer
#' definition is "done".
#'
#' **Object naming is per layer.** All generated objects are named from a
#' layer id cleaned from `group` (`"Corridor Crash Stats"` ->
#' `corridor_crash_stats`): `pal_{id}`, `{id}_sd`, and `flt_{id}_{col}`
#' (widget ids `inpt_{id}_{col}`). Two layers can filter on the same column
#' without colliding.
#'
#' **Crosstalk.** By default crosstalk is on exactly when `def_filter` has
#' filters. `def_filter = NULL` (the default) means no `SharedData`, no
#' widgets, and the layer reads `data = {data_name}` directly. The setup
#' header in the printed code states which: `[crosstalk: on, 6 filters]` or
#' `[crosstalk: off]`.
#'
#' **Labels** fall back in order: `label_col` (wrapped in
#' `purrr::map(..., htmltools::HTML)`, so HTML label columns render and
#' plain text is unchanged), then the palette column, then
#' `"no_data_label"`.
#'
#' @param data_name character. Name of the sf object as it should appear
#'   in the generated code (e.g. `"temp_cor"` or
#'   `'nyc_portal_layers_vzn[["VZN Priority Intr"]]'`) -- NOT the object
#'   itself.
#' @param group character. Leaflet overlay/legend group, and the source of
#'   the layer id. Defaults to `data_name`.
#' @param palette_col character or NULL. Column colored by the palette.
#'   `NULL` (default) draws the layer in static colors from `def_style`
#'   with no legend.
#' @param label_col character or NULL. Column used for hover labels; may
#'   contain HTML.
#' @param popup logical. Add a click popup table via
#'   [gntLeaf_popup_tbl()]. Default `FALSE` (printed commented out).
#' @param def_pal [gntLeaf_def_pal()] or a partial list. Palette settings,
#'   including `reverse`. Ignored without `palette_col`.
#' @param def_size [gntLeaf_def_size()], a partial list, or `NULL` to turn
#'   sizing off. On by default, tracking `palette_col`.
#' @param def_filter Filters, or `NULL` (default) for none. A character
#'   vector (`c("Street" = "street_std", "vol_mean")`), or a list mixing
#'   such strings with [gntLeaf_def_filter()] calls for per-widget options.
#'   Names are display labels; unnamed entries use the column name.
#' @param crosstalk logical or NULL. `NULL` (default) -> on when
#'   `def_filter` has filters. Set `TRUE` to get a `SharedData` with no
#'   widgets (e.g. to link the layer to a table). `FALSE` with filters is
#'   an error.
#' @param def_style [gntLeaf_def_style()] or a partial list. Static styling
#'   and any extra leaflet arguments.
#' @param def_legend [gntLeaf_def_legend()], a partial list, or `NULL` to
#'   turn the legend off.
#' @param visible logical. Show the layer on load. Default `FALSE`; the
#'   assembler adds hidden layers to `hideGroup()`.
#' @param id_col character or NULL. Column for `layerId`.
#' @param geom character or NULL. Override geometry detection: `"point"`,
#'   `"line"`, or `"polygon"`.
#' @param col_type character or NULL. Override palette type detection:
#'   `"numeric"` or `"factor"`.
#' @param envir environment. Where `data_name` is evaluated. Default: the
#'   caller's environment.
#'
#' @return Invisibly, a `gntLeaf_layer` list with `id`, `group`,
#'   `data_name`, `geom`, `crosstalk`, `visible`, `setup_code`,
#'   `layer_code`, and `filter_vars`.
#' @export
#'
#' @seealso [gntLeaf_gen_map_layers()], [gntLeaf_gen_leaflet_map()],
#'   [gntLeaf_def_pal()], [gntLeaf_def_size()], [gntLeaf_def_filter()],
#'   [gntLeaf_def_style()], [gntLeaf_def_legend()]
#'
#' @examples
#' \dontrun{
#' # palette, sizing, HTML labels, and filters (crosstalk on)
#' layer_cor = gntLeaf_gen_map_layer(
#'   data_name = "temp_cor"
#'   ,group = "Corridor Crash Stats"
#'   ,palette_col = "map_per_mile_injured"
#'   ,label_col = "label"
#'   ,def_pal = list(end = .5)
#'   ,def_filter = list(
#'     "Street" = "street_std", "Facility Class" = "facilitycl"
#'     ,"Borough" = "boro_name"
#'     ,"Injured per Mile" = "map_per_mile_injured"
#'     ,gntLeaf_def_filter("map_per_mile_killed", "Killed per Mile", round = 1))
#'   ,def_legend = list(title = "Injuries per Mile")
#' )
#'
#' # static colors, no filters (crosstalk off), sizing off
#' layer_xings = gntLeaf_gen_map_layer(
#'   data_name = 'nyc_portal_layers_vzn[["VZN Enhncd Xings"]]'
#'   ,group = "VZN Enhncd Xings"
#'   ,def_size = NULL
#' )
#'
#' gntLeaf_gen_leaflet_map(list(layer_cor, layer_xings))
#' }
gntLeaf_gen_map_layer = function(
    data_name
    ,group = data_name
    ,palette_col = NULL
    ,label_col = NULL
    ,popup = FALSE
    ,def_pal = gntLeaf_def_pal()
    ,def_size = gntLeaf_def_size()
    ,def_filter = NULL
    ,crosstalk = NULL
    ,def_style = gntLeaf_def_style()
    ,def_legend = gntLeaf_def_legend()
    ,visible = FALSE
    ,id_col = NULL
    ,geom = NULL
    ,col_type = NULL
    ,envir = parent.frame()
){

  #data + detection=============================================================
  data = .gntLeaf_resolve_data(data_name, envir)
  if(!inherits(data, "sf")){
    stop(sprintf("`%s` must be an sf object", data_name), call. = FALSE)
  }
  geom = if(is.null(geom)) gntLeaf_geom_type(data) else {
    match.arg(geom, c("point", "line", "polygon"))
  }
  id = .gntLeaf_clean_id(group)

  .chk_col = function(col, arg){
    if(!is.null(col) && !col %in% names(data)){
      stop(sprintf("%s '%s' not found in %s", arg, col, data_name), call. = FALSE)
    }
  }
  .chk_col(palette_col, "palette_col")
  .chk_col(label_col, "label_col")
  .chk_col(id_col, "id_col")

  has_pal = !is.null(palette_col)
  if(has_pal){
    col_type = if(is.null(col_type)){
      if(is.numeric(data[[palette_col]])) "numeric" else "factor"
    } else match.arg(col_type, c("numeric", "factor"))
  }

  #defs=========================================================================
  def_pal = .gntLeaf_resolve_def(def_pal, gntLeaf_def_pal, "def_pal")
  if(has_pal && is.null(def_pal)) def_pal = gntLeaf_def_pal()
  def_size = .gntLeaf_resolve_def(def_size, gntLeaf_def_size, "def_size")
  def_style = .gntLeaf_resolve_def(def_style, gntLeaf_def_style, "def_style")
  if(is.null(def_style)) def_style = gntLeaf_def_style()
  def_legend = .gntLeaf_resolve_def(def_legend, gntLeaf_def_legend, "def_legend")
  filters = .gntLeaf_resolve_filters(def_filter, data, data_name)

  #crosstalk====================================================================
  if(is.null(crosstalk)) crosstalk = length(filters) > 0
  if(!isTRUE(crosstalk) && length(filters) > 0){
    stop("crosstalk = FALSE but def_filter has filters; drop one", call. = FALSE)
  }

  sd_name = paste0(id, "_sd")
  pal_name = paste0("pal_", id)
  data_ref = if(crosstalk) sd_name else data_name
  col_ref = function(col) sprintf('%s[["%s"]]', data_name, col)

  #size=========================================================================
  #size_candidate is printed (commented) even when sizing is off
  size_col = NULL
  size_candidate = NULL
  size_range = NULL
  if(geom != "polygon"){
    explicit = !is.null(def_size) && !is.null(def_size$col)
    cand = if(explicit) def_size$col else palette_col
    .chk_col(cand, "def_size$col")
    if(!is.null(cand) && is.numeric(data[[cand]])){
      size_candidate = cand
      if(!is.null(def_size)) size_col = cand
    } else if(explicit){
      stop(sprintf("def_size$col '%s' must be numeric", cand), call. = FALSE)
    }
    size_range = if(!is.null(def_size) && !is.null(def_size$range)){
      def_size$range
    } else if(geom == "point") c(3, 10) else c(5, 10)
  }
  rescale_txt = if(!is.null(size_candidate)){
    sprintf("~rescale_vector(%s, lower = %s, upper = %s)"
            ,.gntLeaf_sym(size_candidate), size_range[1], size_range[2])
  } else NULL

  #style defaults===============================================================
  st = def_style
  dflt = switch(
    geom
    ,point = if(has_pal){
      list(radius = 6, fill_opacity = .7, opacity = .9
           ,weight = if(is.null(st$stroke_color)) 0 else 2)
    } else {
      list(radius = 10, fill_opacity = .4, opacity = .9, weight = 2)
    }
    ,line = list(opacity = .9, weight = 2)
    ,polygon = list(fill_opacity = .3, opacity = .9, weight = 1)
  )
  sv = function(nm) if(!is.null(st[[nm]])) st[[nm]] else dflt[[nm]]
  fill = if(is.null(st$fill)) TRUE else st$fill
  f = .gntLeaf_fmt
  pal_txt = if(has_pal) sprintf("~%s(%s)", pal_name, .gntLeaf_sym(palette_col)) else NULL

  #slots========================================================================
  #each slot: c(name, value, active); inactive slots print commented out
  slots = list()
  add = function(name, value, active = TRUE){
    slots[[length(slots) + 1]] <<- list(name = name, value = value, active = active)
  }

  add("layerId", if(!is.null(id_col)) paste0("~", .gntLeaf_sym(id_col)) else "~id_col"
      ,!is.null(id_col))

  if(geom == "point"){
    if(has_pal && !is.null(st$stroke_color)){
      add("color", f(st$stroke_color))
      add("fillColor", pal_txt)
    } else if(has_pal){
      add("color", pal_txt)
      add("fillColor", f(if(is.null(st$fill_color)) "blue" else st$fill_color)
          ,!is.null(st$fill_color))
    } else {
      add("color", f(if(is.null(st$stroke_color)) "black" else st$stroke_color))
      add("fillColor", f(if(is.null(st$fill_color)) "blue" else st$fill_color))
    }
    if(!is.null(rescale_txt)) add("radius", rescale_txt, !is.null(size_col))
    add("radius", f(sv("radius")), is.null(size_col))
    add("fill", f(fill))
    add("fillOpacity", f(sv("fill_opacity")))
    add("opacity", f(sv("opacity")))
    add("weight", f(sv("weight")))
  }

  if(geom == "line"){
    static_col = if(!is.null(st$stroke_color)) st$stroke_color else {
      if(!is.null(st$fill_color)) st$fill_color else "blue"
    }
    add("color", if(has_pal) pal_txt else f(static_col))
    add("opacity", f(sv("opacity")))
    if(!is.null(rescale_txt)) add("weight", rescale_txt, !is.null(size_col))
    add("weight", f(sv("weight")), is.null(size_col))
  }

  if(geom == "polygon"){
    add("fillColor", if(has_pal) pal_txt else {
      f(if(is.null(st$fill_color)) "blue" else st$fill_color)
    })
    add("color", f(if(is.null(st$stroke_color)) "white" else st$stroke_color))
    add("fill", f(fill))
    add("fillOpacity", f(sv("fill_opacity")))
    add("opacity", f(sv("opacity")))
    add("weight", f(sv("weight")))
  }

  label_txt = if(!is.null(label_col)){
    sprintf("~purrr::map(%s, htmltools::HTML)", .gntLeaf_sym(label_col))
  } else if(has_pal && col_type == "numeric"){
    sprintf("~as.character(round(%s, 2))", .gntLeaf_sym(palette_col))
  } else if(has_pal){
    sprintf("~as.character(%s)", .gntLeaf_sym(palette_col))
  } else '"no_data_label"'
  add("label", label_txt)
  add("popup", sprintf("gntLeaf_popup_tbl(%s)", data_name), isTRUE(popup))
  add("group", f(group))
  for(nm in names(st$extras)) add(nm, f(st$extras[[nm]]))

  add_fn = switch(geom, point = "addCircleMarkers", line = "addPolylines"
                  ,polygon = "addPolygons")
  slot_lines = vapply(slots, function(s){
    paste0("    ", if(!s$active) "# " else "", ",", s$name, " = ", s$value)
  }, character(1))

  marker_call = paste0(
    "  leaflet::", add_fn, "(\n"
    ,"    data = ", data_ref, "\n"
    ,paste(slot_lines, collapse = "\n"), "\n"
    ,"  ) %>%"
  )

  #legend=======================================================================
  legend_call = NULL
  if(has_pal){
    title = if(!is.null(def_legend) && !is.null(def_legend$title)){
      def_legend$title
    } else tools::toTitleCase(gsub("_", " ", palette_col))
    position = if(!is.null(def_legend)) def_legend$position else "bottomleft"
    legend_call = paste0(
      "  leaflet::addLegend(\n"
      ,"    position = ", f(position), "\n"
      ,"    ,title = ", f(title), "\n"
      ,"    ,group = ", f(group), "\n"
      ,"    ,pal = ", pal_name, "\n"
      ,"    ,values = ", col_ref(palette_col), "\n"
      ,"  ) %>%"
    )
    if(is.null(def_legend)) legend_call = .gntLeaf_comment(legend_call)
  }

  layer_code = paste(
    c(.gntLeaf_header(sprintf("###mapLyr_start: %s ", id))
      ,marker_call
      ,legend_call
      ,.gntLeaf_header(sprintf("###mapLyr_end: %s ", id)))
    ,collapse = "\n"
  )

  #setup========================================================================
  pal_code = NULL
  if(has_pal){
    pal_vec = if(length(def_pal$palette) > 1){
      f(def_pal$palette)
    } else {
      sprintf("viridis::%s(n = %s, begin = %s, end = %s)"
              ,def_pal$palette, def_pal$n, def_pal$begin, def_pal$end)
    }
    pal_code = paste0(
      pal_name, " = leaflet::"
      ,if(col_type == "numeric") "colorNumeric" else "colorFactor", "(\n"
      ,"  palette = ", pal_vec, "\n"
      ,"  ,domain = ", col_ref(palette_col), "\n"
      ,"  ,reverse = ", f(def_pal$reverse), "\n"
      ,")"
    )
  }

  sd_code = if(crosstalk){
    paste0(sd_name, " = ", data_name, " %>%\n  crosstalk::SharedData$new()")
  } else NULL

  filter_vars = character(0)
  filter_lines = character(0)
  for(flt in filters){
    var = paste0("flt_", id, "_", .gntLeaf_clean_id(flt$col))
    inpt = paste0("inpt_", id, "_", .gntLeaf_clean_id(flt$col))
    extras = if(length(flt$extras) > 0){
      paste0(", ", paste(names(flt$extras), vapply(flt$extras, f, character(1))
                         ,sep = " = ", collapse = ", "))
    } else ""
    line = if(flt$type == "slider"){
      step = if(!is.null(flt$step)) paste0(", step = ", f(flt$step)) else ""
      sprintf('%s = crosstalk::filter_slider(%s, %s, %s, %s, round = %s%s%s)'
              ,var, f(inpt), f(flt$label), sd_name, f(flt$col)
              ,if(is.null(flt$round)) 2 else flt$round, step, extras)
    } else {
      sprintf('%s = crosstalk::filter_select(%s, %s, %s, ~%s%s)'
              ,var, f(inpt), f(flt$label), sd_name, .gntLeaf_sym(flt$col), extras)
    }
    filter_vars = c(filter_vars, var)
    filter_lines = c(filter_lines, line)
  }

  ct_tag = if(crosstalk){
    sprintf("[crosstalk: on, %d filter%s]", length(filters)
            ,if(length(filters) == 1) "" else "s")
  } else "[crosstalk: off]"

  setup_body = c(pal_code, sd_code
                 ,if(length(filter_lines) > 0) paste(filter_lines, collapse = "\n"))
  if(length(setup_body) == 0) setup_body = "# nothing to set up (no palette, no crosstalk)"

  setup_code = paste(
    c(.gntLeaf_header(sprintf("###lyrSetup_start: %s %s ", id, ct_tag))
      ,paste(setup_body, collapse = "\n\n")
      ,.gntLeaf_header(sprintf("###lyrSetup_end: %s ", id)))
    ,collapse = "\n"
  )

  invisible(structure(
    list(
      id = id
      ,group = group
      ,data_name = data_name
      ,geom = geom
      ,crosstalk = crosstalk
      ,visible = visible
      ,setup_code = setup_code
      ,layer_code = layer_code
      ,filter_vars = filter_vars
    )
    ,class = c("gntLeaf_layer", "list")
  ))
}


#' @keywords internal
#' @noRd
.gntLeaf_resolve_filters = function(def_filter, data, data_name){
  if(is.null(def_filter) || length(def_filter) == 0) return(list())
  if(inherits(def_filter, "gntLeaf_def_filter")) def_filter = list(def_filter)
  if(is.character(def_filter)) def_filter = as.list(def_filter)

  nms = names(def_filter)
  if(is.null(nms)) nms = rep("", length(def_filter))

  out = lapply(seq_along(def_filter), function(i){
    x = def_filter[[i]]
    flt = if(inherits(x, "gntLeaf_def_filter")){
      x
    } else if(is.character(x) && length(x) == 1){
      gntLeaf_def_filter(x, label = if(nms[i] == "") x else nms[i])
    } else if(is.list(x)){
      do.call(gntLeaf_def_filter, x)
    } else {
      stop("each def_filter entry must be a column name or gntLeaf_def_filter()"
           ,call. = FALSE)
    }
    if(!flt$col %in% names(data)){
      stop(sprintf("filter column '%s' not found in %s", flt$col, data_name)
           ,call. = FALSE)
    }
    if(is.null(flt$type)){
      v = data[[flt$col]]
      flt$type = if(is.numeric(v) || inherits(v, c("Date", "POSIXt"))) "slider" else "select"
    }
    flt
  })

  cols = vapply(out, `[[`, character(1), "col")
  if(anyDuplicated(cols)){
    stop(sprintf("duplicate filter column(s): %s"
                 ,paste(unique(cols[duplicated(cols)]), collapse = ", ")), call. = FALSE)
  }
  out
}
