#' Esri basemap group names, with the chosen default first
#'
#' Returns the display/group names of the Esri basemaps added by
#' [gntLeaf_add_esri_basemaps()], reordered so `default` comes first. Use
#' it for `leaflet::addLayersControl(baseGroups = ...)` when you build your
#' own control (`gntLeaf_add_esri_basemaps(control = FALSE)`). The first
#' name listed is the one checked in the control, so it should match the
#' `default` passed to `gntLeaf_add_esri_basemaps()`.
#'
#' Names come from `gntLeaf_esri_basemap_specs`, so they always match the
#' groups `gntLeaf_add_esri_basemaps()` creates. Add or rename a basemap
#' there and it shows up here automatically.
#'
#' @param default character. Basemap to list first; one of `"streets"`
#'   (default), `"gray"`, `"dark"`, `"imagery"`.
#'
#' @return Unnamed character vector of basemap group names, e.g.
#'   `c("Streets (gray)", "Streets", "Streets (dark)", "Imagery")` for
#'   `default = "gray"`.
#'
#' @seealso [gntLeaf_add_esri_basemaps()], [gntLeaf_gen_leaflet_map()]
#'
#' @export
#'
#' @examples
#' gntLeaf_esri_basemap_names()
#' gntLeaf_esri_basemap_names("dark")
#'
#' \dontrun{
#' leaflet::leaflet() |>
#'   gntLeaf_add_esri_basemaps(default = "gray", control = FALSE) |>
#'   leaflet::addLayersControl(
#'     baseGroups = gntLeaf_esri_basemap_names("gray")
#'     ,overlayGroups = c("BREWERIES")
#'   )
#' }
gntLeaf_esri_basemap_names = function(default = c("streets", "gray", "dark", "imagery")){
  default = match.arg(default)
  nms = vapply(gntLeaf_esri_basemap_specs, `[[`, "", "name")
  c(nms[[default]], unname(nms[names(nms) != default]))
}
