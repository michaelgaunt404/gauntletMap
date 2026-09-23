#' Add Esri basemaps (streets raw / grayscale / dark + imagery) to a leaflet map
#'
#' Pipe-friendly helper that adds every basemap defined in
#' [gntLeaf_esri_basemap_specs] as its own leaflet group, shows only
#' `default`, and (optionally) adds a layers control to switch between them.
#' A drop-in replacement for `leaflet::addTiles()` now that
#' tile.openstreetmap.org blocks requests without a Referer (e.g. HTML files
#' opened from disk).
#'
#' Tiles come from Esri's public ArcGIS Online tile server and load without
#' an API key, including from local `file://` HTML. The grayscale and dark
#' variants reuse `World_Street_Map` tiles with a CSS `filter` applied
#' through each layer's class name (see [gntLeaf_esri_basemap_specs]), so
#' they cost no extra tile requests beyond the one layer being viewed.
#'
#' Max zoom: tiles are requested up to zoom 19 (`maxNativeZoom`) and
#' upscaled to 20 (`maxZoom`) instead of going blank.
#'
#' Only one layers control can exist per map, and a later
#' `addLayersControl()` call replaces this one. If you need your own control,
#' set `control = FALSE` and pass [gntLeaf_esri_basemap_names()] as
#' `baseGroups`.
#'
#' @param map a leaflet map (htmlwidget) from [leaflet::leaflet()].
#' @param default character. Basemap shown on load; one of `"streets"`
#'   (default), `"gray"`, `"dark"`, `"imagery"` -- i.e. any name in
#'   [gntLeaf_esri_basemap_specs].
#' @param overlay_groups character vector or NULL. Your data layer group
#'   names, listed as checkboxes in the same control. They may refer to
#'   groups added later in the pipe.
#' @param control logical. Add the layers control? Default `TRUE`.
#' @param position character. Control position: `"topright"` (default),
#'   `"topleft"`, `"bottomright"`, `"bottomleft"`.
#' @param collapsed logical. Collapse the control to an icon until hovered?
#'   Default `TRUE`.
#'
#' @return The modified leaflet map, for further piping.
#'
#' @seealso [gntLeaf_esri_basemap_names()], [gntLeaf_esri_basemap_specs],
#'   [gntLeaf_gen_leaflet_map()]
#'
#' @export
#'
#' @importFrom leaflet addTiles tileOptions hideGroup addLayersControl layersControlOptions
#' @importFrom htmlwidgets prependContent
#' @importFrom htmltools tags
#'
#' @examples
#' \dontrun{
#' # basemaps only, streets on load
#' leaflet::leaflet() |>
#'   gntLeaf_add_esri_basemaps() |>
#'   leaflet::setView(-73.98, 40.75, zoom = 12)
#'
#' # gray on load, data layers in the same menu
#' leaflet::leaflet() |>
#'   gntLeaf_add_esri_basemaps(
#'     default = "gray"
#'     ,overlay_groups = c("Tracts", "Crashes")
#'     ,collapsed = FALSE
#'   ) |>
#'   leaflet::addPolygons(data = tracts, group = "Tracts") |>
#'   leaflet::addCircleMarkers(data = crashes, group = "Crashes", radius = 3)
#'
#' # your own control
#' leaflet::leaflet() |>
#'   gntLeaf_add_esri_basemaps(default = "dark", control = FALSE) |>
#'   leaflet::addLayersControl(baseGroups = gntLeaf_esri_basemap_names("dark"))
#' }
gntLeaf_add_esri_basemaps = function(
    map
    ,default = "streets"
    ,overlay_groups = NULL
    ,control = TRUE
    ,position = "topright"
    ,collapsed = TRUE
){
  default = match.arg(default, names(gntLeaf_esri_basemap_specs))

  esri = "https://server.arcgisonline.com/ArcGIS/rest/services/"
  url = function(svc) paste0(esri, svc, "/MapServer/tile/{z}/{y}/{x}")

  for(l in gntLeaf_esri_basemap_specs){
    map = leaflet::addTiles(
      map, url(l$svc), group = l$name, attribution = "Tiles &copy; Esri"
      ,options = leaflet::tileOptions(className = l$cls, maxNativeZoom = 19, maxZoom = 20)
    )
  }

  map = htmlwidgets::prependContent(map, htmltools::tags$style(
    ".esri-gray { filter: grayscale(1) contrast(1.1); }
     .esri-dark { filter: invert(1) hue-rotate(180deg) grayscale(0.5) brightness(0.9); }"
  ))

  base_names = gntLeaf_esri_basemap_names(default)
  map = leaflet::hideGroup(map, base_names[-1])

  if(control){
    map = leaflet::addLayersControl(
      map
      ,baseGroups = base_names
      ,overlayGroups = overlay_groups
      ,position = position
      ,options = leaflet::layersControlOptions(collapsed = collapsed)
    )
  }

  map
}
