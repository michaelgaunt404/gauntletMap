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
#' The gray and dark basemaps are Esri's own Light Gray and Dark Gray
#' Canvas services (they replace the earlier CSS-filtered street tiles).
#' Canvas basemaps ship as two services: an unlabeled `Base` and a
#' `Reference` layer with place and street labels. When `ref` is set,
#' [gntLeaf_add_esri_basemaps()] draws the reference tiles on top of the
#' base in the same group, so both toggle together.
#'
#' Canvas tiles are only cached to a lower zoom than the street and imagery
#' services; `max_native` tells leaflet to stop requesting new tiles there
#' and upscale instead (otherwise Esri returns "Map data not yet available"
#' tiles). If you see those tiles at high zoom, lower `max_native`.
#'
#' To add a basemap, add an element here with a unique `name` and add its
#' key to the `default` choices in [gntLeaf_esri_basemap_names()].
#'
#' @format A named list of 4 elements, each a list with:
#' \describe{
#'   \item{name}{character. Leaflet group name, shown in the layers control.}
#'   \item{svc}{character. ArcGIS Online MapServer service path, e.g.
#'     `"World_Street_Map"` or `"Canvas/World_Light_Gray_Base"`.}
#'   \item{ref}{character or NULL. Optional reference (labels) service drawn
#'     over `svc` in the same group.}
#'   \item{max_native}{integer. Highest zoom the service has tiles for.}
#' }
#'
#' @seealso [gntLeaf_add_esri_basemaps()], [gntLeaf_esri_basemap_names()]
#'
#' @keywords internal
gntLeaf_esri_basemap_specs = list(
  streets = list(
    name = "Streets", svc = "World_Street_Map"
    ,ref = NULL, max_native = 19)
  ,gray = list(
    name = "Light Gray Canvas"
    ,svc = "Canvas/World_Light_Gray_Base"
    # ,ref = "Canvas/World_Light_Gray_Reference"
    ,ref = NULL
    ,max_native = 16)
  ,dark = list(
    name = "Dark Gray Canvas"
    ,svc = "Canvas/World_Dark_Gray_Base"
    # ,ref = "Canvas/World_Dark_Gray_Reference"
    ,ref = NULL
    ,max_native = 16)
  ,imagery = list(
    name = "Imagery", svc = "World_Imagery"
    ,ref = NULL, max_native = 19)
)

