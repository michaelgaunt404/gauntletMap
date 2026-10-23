#' Define static styling for a layer
#'
#' Bundles the non-data-driven style settings for [gntLeaf_gen_map_layer()]'s
#' `def_style` argument. Every entry defaults to `NULL`, meaning "use the
#' default for this geometry", so you only set what you want to change:
#' `def_style = list(fill_opacity = .5)`.
#'
#' Defaults when left `NULL`:
#' \itemize{
#'   \item **point, with palette**: radius 6, fill_opacity .7, opacity .9,
#'     weight 0 (weight 2 if `stroke_color` is set);
#'   \item **point, no palette** (NYC/VZN template): fill_color `"blue"`,
#'     stroke_color `"black"`, radius 10, fill_opacity .4, opacity .9,
#'     weight 2;
#'   \item **line**: color from palette, else `"blue"`; opacity .9,
#'     weight 2;
#'   \item **polygon**: fill from palette, else `"blue"`; stroke_color
#'     `"white"`, fill_opacity .3, opacity .9, weight 1.
#' }
#'
#' Color interplay with a palette: on points, the palette colors both
#' stroke and fill unless `stroke_color` is set, in which case the palette
#' fills and `stroke_color` outlines (good for small markers on busy
#' basemaps). On polygons the palette always fills.
#'
#' @param radius numeric. Static circle radius (points). Used when sizing
#'   is off.
#' @param weight numeric. Stroke width (points/polygons) or line width
#'   (lines; used when sizing is off).
#' @param opacity numeric. Stroke/line opacity.
#' @param fill logical. Draw a fill (points/polygons).
#' @param fill_opacity numeric. Fill opacity (points/polygons).
#' @param fill_color character. Static fill color.
#' @param stroke_color character. Static stroke/outline color.
#' @param ... Any other leaflet argument, passed straight into the
#'   `add*()` call (e.g. `dashArray = "4"`, `smoothFactor = 2`). Values are
#'   written into the generated code with [base::deparse()]; pass a formula
#'   (`~some_col`) to reference a column.
#'
#' @return A `gntLeaf_def_style` list.
#' @export
#'
#' @examples
#' gntLeaf_def_style(fill_opacity = .5, stroke_color = "black")
#' gntLeaf_def_style(dashArray = "4")
gntLeaf_def_style = function(
    radius = NULL
    ,weight = NULL
    ,opacity = NULL
    ,fill = NULL
    ,fill_opacity = NULL
    ,fill_color = NULL
    ,stroke_color = NULL
    ,...
){
  extras = list(...)
  if(length(extras) > 0 && (is.null(names(extras)) || any(names(extras) == ""))){
    stop("extra def_style arguments must be named", call. = FALSE)
  }
  structure(
    list(radius = radius, weight = weight, opacity = opacity, fill = fill
         ,fill_opacity = fill_opacity, fill_color = fill_color
         ,stroke_color = stroke_color, extras = extras)
    ,class = c("gntLeaf_def_style", "list")
  )
}
