#' Add Esri basemaps (streets / light gray / dark gray / imagery) to a leaflet map
#'
#' Pipe-friendly helper that adds every basemap defined in
#' [gntLeaf_esri_basemap_specs] as its own leaflet group, shows only
#' `default`, and (optionally) adds a layers control to switch between them.
#' A drop-in replacement for `leaflet::addTiles()` now that
#' tile.openstreetmap.org blocks requests without a Referer (e.g. HTML files
#' opened from disk).
#'
#' Tiles come from Esri's public ArcGIS Online tile server and load without
#' an API key, including from local `file://` HTML. The gray and dark
#' options are Esri's Light Gray and Dark Gray Canvas basemaps, each drawn
#' as an unlabeled base plus a labels (reference) layer in the same group;
#' set `labels = FALSE` for the bare canvas. Leaflet loads tiles only for
#' visible groups, so the hidden basemaps cost no tile requests.
#'
#' Max zoom: each service is requested up to its own native zoom
#' (`max_native` in [gntLeaf_esri_basemap_specs]) and upscaled to 20
#' (`maxZoom`) instead of going blank.
#'
#' Reference labels sit in leaflet's tile pane, so data layers draw over
#' them.
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
#' @param labels logical. Draw the reference (labels) layer on basemaps that
#'   have one (gray, dark). Default `TRUE`.
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
#'
#' @examples
#' \dontrun{
#' # basemaps only, streets on load
#' leaflet::leaflet() |>
#'   gntLeaf_add_esri_basemaps() |>
#'   leaflet::setView(-73.98, 40.75, zoom = 12)
#'
#' # light gray canvas on load, no labels, data layers in the same menu
#' leaflet::leaflet() |>
#'   gntLeaf_add_esri_basemaps(
#'     default = "gray"
#'     ,labels = FALSE
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
    ,labels = TRUE
    ,overlay_groups = NULL
    ,control = TRUE
    ,position = "topright"
    ,collapsed = TRUE
){
  default = match.arg(default, names(gntLeaf_esri_basemap_specs))

  esri = "https://server.arcgisonline.com/ArcGIS/rest/services/"
  url = function(svc) paste0(esri, svc, "/MapServer/tile/{z}/{y}/{x}")
  attr = "Tiles &copy; Esri"

  for(l in gntLeaf_esri_basemap_specs){
    opts = leaflet::tileOptions(maxNativeZoom = l$max_native, maxZoom = 20)
    map = leaflet::addTiles(map, url(l$svc), group = l$name
                            ,attribution = attr, options = opts)
    if(isTRUE(labels) && !is.null(l$ref)){
      map = leaflet::addTiles(map, url(l$ref), group = l$name
                              ,attribution = attr, options = opts)
    }
  }

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
