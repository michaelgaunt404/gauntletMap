#' Define value-based sizing for a layer
#'
#' Bundles the sizing settings for [gntLeaf_gen_map_layer()]'s `def_size`
#' argument. Sizing rescales a numeric column with [rescale_vector()] and
#' drives marker radius (points) or line width (lines). Polygons are never
#' sized.
#'
#' Sizing is **on by default** and tracks the palette column. To turn it
#' off, pass `def_size = NULL` to the generator; the static `radius` /
#' `weight` from `def_style` is then used, and the rescale line is printed
#' commented out so it's one keystroke to switch back on.
#'
#' A plain partial list works too: `def_size = list(range = c(2, 6))`.
#'
#' @param col character or NULL. Numeric column to size by. `NULL`
#'   (default) uses the layer's `palette_col`; if that column is a factor,
#'   or there is no palette column, sizing quietly turns off. An explicitly
#'   named non-numeric column is an error.
#' @param range numeric length-2, `c(lower, upper)` passed to
#'   [rescale_vector()]. `NULL` (default) -> `c(3, 10)` for points,
#'   `c(5, 10)` for lines.
#'
#' @return A `gntLeaf_def_size` list.
#' @export
#'
#' @examples
#' gntLeaf_def_size()
#' gntLeaf_def_size(col = "vol_mean", range = c(2, 8))
gntLeaf_def_size = function(
    col = NULL
    ,range = NULL
){
  if(!is.null(range) && length(range) != 2){
    stop("range must be length 2: c(lower, upper)", call. = FALSE)
  }
  structure(
    list(col = col, range = range)
    ,class = c("gntLeaf_def_size", "list")
  )
}
