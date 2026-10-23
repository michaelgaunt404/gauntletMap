#' Define the legend for a layer
#'
#' Bundles legend settings for [gntLeaf_gen_map_layer()]'s `def_legend`
#' argument. A legend is only drawn for layers with a `palette_col`.
#' Pass `def_legend = NULL` to the generator to turn it off; the
#' `addLegend()` call is then printed commented out.
#'
#' @param title character or NULL. Legend title. `NULL` (default) uses the
#'   title-cased palette column (`"map_per_mile_injured"` ->
#'   `"Map per Mile Injured"`).
#' @param position character. One of `"bottomleft"` (default),
#'   `"bottomright"`, `"topleft"`, `"topright"`.
#'
#' @return A `gntLeaf_def_legend` list.
#' @export
#'
#' @examples
#' gntLeaf_def_legend("Injuries per Mile")
gntLeaf_def_legend = function(
    title = NULL
    ,position = c("bottomleft", "bottomright", "topleft", "topright")
){
  structure(
    list(title = title, position = match.arg(position))
    ,class = c("gntLeaf_def_legend", "list")
  )
}
