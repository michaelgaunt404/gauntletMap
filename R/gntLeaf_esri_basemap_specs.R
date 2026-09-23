#' Esri basemap definitions used by the gntLeaf_* basemap helpers
#'
#' Single source of truth for the Esri basemaps that
#' [gntLeaf_add_esri_basemaps()] adds and [gntLeaf_esri_basemap_names()]
#' lists. Each element is one basemap. The list names (`streets`, `gray`,
#' `dark`, `imagery`) are the keys accepted by the `default` argument of
#' both functions.
#'
#' Tiles come from Esri's public ArcGIS Online tile server:
#' `https://server.arcgisonline.com/ArcGIS/rest/services/{svc}/MapServer/tile/{z}/{y}/{x}`
#' (note `{y}` before `{x}`). They load without an API key, including from
#' HTML files opened locally (`file://`).
#'
#' The grayscale and dark variants reuse `World_Street_Map` tiles. Their
#' look comes from a CSS `filter` that [gntLeaf_add_esri_basemaps()] applies
#' through the `cls` class name:
#' \itemize{
#'   \item `esri-gray`: `grayscale(1) contrast(1.1)`
#'   \item `esri-dark`: `invert(1) hue-rotate(180deg) grayscale(0.5) brightness(0.9)`
#'   \item `esri-raw`, `esri-img`: no filter (class exists as a styling hook)
#' }
#'
#' To add a basemap, add an element here with a unique `name` and `cls`,
#' add its key to the `default` choices in both functions, and add a CSS
#' rule in [gntLeaf_add_esri_basemaps()] if it needs a filter.
#'
#' @format A named list of 4 elements, each a list with:
#' \describe{
#'   \item{name}{character. Leaflet group name, shown in the layers control.}
#'   \item{svc}{character. ArcGIS Online MapServer service path, e.g.
#'     `"World_Street_Map"` or `"Canvas/World_Light_Gray_Base"`.}
#'   \item{cls}{character. CSS class set on the tile layer via
#'     `tileOptions(className = ...)`, used to target the CSS filter.}
#' }
#'
#' @seealso [gntLeaf_add_esri_basemaps()], [gntLeaf_esri_basemap_names()]
#'
#' @keywords internal
gntLeaf_esri_basemap_specs = list(
  streets = list(name = "Streets",        svc = "World_Street_Map", cls = "esri-raw")
  ,gray   = list(name = "Streets (gray)", svc = "World_Street_Map", cls = "esri-gray")
  ,dark   = list(name = "Streets (dark)", svc = "World_Street_Map", cls = "esri-dark")
  ,imagery = list(name = "Imagery",       svc = "World_Imagery",    cls = "esri-img")
)
