#' Define one crosstalk filter widget
#'
#' Builds a single filter definition for [gntLeaf_gen_map_layer()]'s
#' `def_filter` argument. Most filters don't need this -- a plain column
#' name (optionally named with its display label) is enough:
#'
#' \preformatted{
#' def_filter = list(
#'   "Street" = "street_std"
#'   ,"Borough" = "boro_name"
#'   ,gntLeaf_def_filter("map_per_mile_killed", "Killed per Mile"
#'                       ,round = 1, step = .5)
#' )
#' }
#'
#' Use the constructor in the rare cases you need per-widget control
#' (rounding, step size, forcing a type, or any other crosstalk argument).
#'
#' Widget type is detected from the column unless `type` is given: numeric,
#' Date, and POSIXct columns get `crosstalk::filter_slider()`; everything
#' else (character, factor, logical) gets `crosstalk::filter_select()`.
#'
#' @param col character. Column to filter on.
#' @param label character. Display label. Defaults to `col`.
#' @param type character or NULL. Force the widget type: `"slider"` (also
#'   `"n"`/`"numeric"`) or `"select"` (also `"f"`/`"factor"`). `NULL`
#'   (default) detects it.
#' @param round integer or NULL. Slider rounding. `NULL` -> 2. Ignored for
#'   selects.
#' @param step numeric or NULL. Slider step (tick) size. `NULL` lets
#'   crosstalk choose. Ignored for selects. Note crosstalk's own `ticks`
#'   argument is a TRUE/FALSE for showing tick marks, not the step size;
#'   pass it through `...` if needed.
#' @param ... Any other argument to `filter_slider()` (`min`, `max`,
#'   `ticks`, `sep`, `pre`, `post`, `width`, ...) or `filter_select()`
#'   (`multiple`, `allLevels`, ...), written into the generated code as-is.
#'
#' @return A `gntLeaf_def_filter` list.
#' @export
#'
#' @examples
#' gntLeaf_def_filter("map_per_mile_killed", "Killed per Mile", round = 1)
#' gntLeaf_def_filter("sip_year", "SIP Year", type = "select")
#' gntLeaf_def_filter("vol_mean", "Volume", step = 10, ticks = FALSE)
gntLeaf_def_filter = function(
    col
    ,label = col
    ,type = NULL
    ,round = NULL
    ,step = NULL
    ,...
){
  if(!is.null(type)){
    type = if(type %in% c("slider", "n", "numeric")){
      "slider"
    } else if(type %in% c("select", "f", "factor")){
      "select"
    } else {
      stop('type must be "slider" or "select"', call. = FALSE)
    }
  }
  extras = list(...)
  if(length(extras) > 0 && (is.null(names(extras)) || any(names(extras) == ""))){
    stop("extra def_filter arguments must be named", call. = FALSE)
  }
  structure(
    list(col = col, label = label, type = type, round = round, step = step
         ,extras = extras)
    ,class = c("gntLeaf_def_filter", "list")
  )
}
