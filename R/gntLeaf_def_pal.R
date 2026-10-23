#' Define a color palette for a layer
#'
#' Bundles every palette setting for [gntLeaf_gen_map_layer()]'s `def_pal`
#' argument. You can pass this constructor or a plain partial list -- any
#' entry you leave out takes the default below, so
#' `def_pal = list(end = .5)` and `def_pal = gntLeaf_def_pal(end = .5)` are
#' the same thing. An unnamed first entry is read as `palette`, so
#' `def_pal = list("mako", end = .85)` also works.
#'
#' The same definition drives numeric (`colorNumeric()`) and factor
#' (`colorFactor()`) palettes; which one is used is detected from the
#' palette column.
#'
#' @param palette character. Either a viridis option name (`"inferno"`
#'   (default), `"magma"`, `"plasma"`, `"viridis"`, `"cividis"`, `"rocket"`,
#'   `"mako"`, `"turbo"`), emitted as `viridis::{palette}(n, begin, end)`; or
#'   a character vector of 2+ colors (e.g. hex codes), emitted as-is, in
#'   which case `n`, `begin`, and `end` are ignored. The vector form is the
#'   escape hatch for hand-picked, colorblind-checked palettes.
#' @param n integer. Number of colors drawn from the viridis option.
#'   Default 50.
#' @param begin,end numeric in `[0, 1]`. Portion of the viridis range used;
#'   e.g. `end = .85` drops the near-white top of inferno. Defaults 0 and 1.
#' @param reverse logical. Flip the palette direction. Passed to
#'   `colorNumeric()`/`colorFactor()` -- this is the single place to flip
#'   colors (don't also use viridis's `direction`). Default `TRUE`.
#'
#' @return A `gntLeaf_def_pal` list.
#' @export
#'
#' @examples
#' gntLeaf_def_pal()
#' gntLeaf_def_pal("mako", end = .85, reverse = FALSE)
#' gntLeaf_def_pal(palette = c("#1b9e77", "#d95f02", "#7570b3"))
gntLeaf_def_pal = function(
    palette = "inferno"
    ,n = 50
    ,begin = 0
    ,end = 1
    ,reverse = TRUE
){
  viridis_opts = c("magma", "inferno", "plasma", "viridis", "cividis"
                   ,"rocket", "mako", "turbo")
  if(length(palette) == 1 && !palette %in% viridis_opts){
    stop(sprintf("palette must be one of %s, or a vector of 2+ colors"
                 ,paste(viridis_opts, collapse = ", ")), call. = FALSE)
  }
  if(begin < 0 || end > 1 || begin >= end){
    stop("begin/end must satisfy 0 <= begin < end <= 1", call. = FALSE)
  }

  structure(
    list(palette = palette, n = n, begin = begin, end = end, reverse = reverse)
    ,class = c("gntLeaf_def_pal", "list")
  )
}
