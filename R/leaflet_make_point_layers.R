#' Add a circle-marker layer with palette and legend
#'
#' Convenience helper for quickly adding **point-based circle-marker layers**
#' to a \code{leaflet} map. The function bundles several repetitive leaflet
#' steps into a single call:
#' \enumerate{
#' \item Create a color palette (factor or numeric)
#' \item Add a \code{addCircleMarkers()} layer
#' \item Add a corresponding \code{addLegend()} entry
#' }
#'
#' The primary goal is rapid iteration when working with many similar point
#' layers, where defining palettes, layers, and legends individually becomes
#' tedious. The function is intentionally lightweight and can also serve as
#' a template for more customized or UI-polished leaflet layers.
#'
#' @param map_obj A \code{leaflet} map object.
#' @param map_layer An \code{sf} object (POINT geometry) or data frame containing
#'   point coordinates (and any attributes used for coloring/labels).
#' @param map_color_attr A single character string naming the column in \code{map_layer}
#'   used for coloring (factor or numeric).
#' @param flag_factor Logical. If \code{TRUE}, \code{map_color_attr} is treated as a
#'   discrete/factor variable and a factor palette is used. If \code{FALSE},
#'   \code{map_color_attr} is treated as numeric and a numeric palette is used.
#' @param layer_name Character string. Leaflet group name and legend title.
#' @param pal_name Character string naming the viridis palette to use.
#'   Common options include \code{"rocket"}, \code{"viridis"}, \code{"magma"},
#'   \code{"inferno"}, \code{"plasma"}, \code{"cividis"}, \code{"turbo"}.
#'   Default is \code{"rocket"}.
#' @param popup_string A glue-style string used to create popup HTML content
#'   when \code{popup_attribute} is \code{NULL}. Evaluated with
#'   \code{stringr::str_glue()} in the context of \code{map_layer}.
#'   Example: \code{"<strong>{name}</strong><br>Type: {type}"}.
#' @param popup_attribute Optional character string naming an existing column in
#'   \code{map_layer} that already contains popup content. If provided, the
#'   function uses that column directly and ignores \code{popup_string}.
#' @param label_string A glue-style string used to create hover label content
#'   when \code{label_attribute} is \code{NULL}. Evaluated with
#'   \code{stringr::str_glue()} in the context of \code{map_layer}.
#'   Example: \code{"{name}"} or \code{"{state} - {type}"}.
#' @param label_attribute Optional character string naming an existing column in
#'   \code{map_layer} that already contains hover label content. If provided, the
#'   function uses that column directly and ignores \code{label_string}.
#'
#' @return A \code{leaflet} map object with the circle-marker layer and legend added.
#'
#' @examples
#' \dontRun{
#' library(leaflet)
#' library(mapview)
#' library(tidyverse)
#' data("breweries", package = "mapview")
#'
#' m <- leaflet(breweries) %>%
#'   addProviderTiles(providers$CartoDB.Positron)
#'
#' # Example: color by a (made-up) factor, popups from a glue string
#' breweries2 <- breweries %>%
#'   dplyr::mutate(type = sample(c("micro", "brewpub", "regional"), dplyr::n(), TRUE)) %>%
#'   dplyr::mutate(popup_pre = str_glue("<strong>{state}</strong><br>Type: {type}"))
#'
#' leaflet_make_point_layers(
#'   map_obj        = m
#'   ,map_layer     = breweries2
#'   ,map_color_attr = "type"
#'   ,flag_factor   = TRUE
#'   ,layer_name    = "Breweries: Type"
#'   ,pal_name      = "rocket"
#'   ,popup_attribute = "popup_pre"
#'   ,label_string  = "{state}"
#'   ,label_attribute = NULL
#' ) %>%
#'   leaflet_make_point_layers(
#'     map_obj        = m
#'     ,map_layer     = breweries2
#'     ,map_color_attr = "number.of.types"
#'     ,flag_factor   = F
#'     ,layer_name    = "Breweries: Beers"
#'     ,pal_name      = "rocket"
#'     ,popup_attribute = "popup_pre"
#'     ,label_string  = "{state}"
#'     ,label_attribute = NULL
#'   )
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_glue
#' @importFrom purrr map
#' @importFrom htmltools HTML
#' @importFrom leaflet addCircleMarkers addLegend
#'
#' @export
leaflet_make_point_layers = function(
    map_obj
    ,map_layer
    ,map_color_attr
    ,flag_factor = TRUE
    ,layer_name
    ,pal_name = "rocket"
    ,popup_string
    ,popup_attribute = NULL
    ,label_string
    ,label_attribute = NULL
){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # prep data + popup/label
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp_data_pro = map_layer

  # popup: either build from glue string OR pull from an existing attribute
  if(is.null(popup_attribute)){
    temp_data_pro = temp_data_pro %>%
      dplyr::mutate(popup = stringr::str_glue(popup_string))
  } else {
    temp_data_pro = temp_data_pro %>%
      dplyr::mutate(popup = .data[[popup_attribute]])
  }

  # label: either build from glue string OR pull from an existing attribute
  if(is.null(label_attribute)){
    temp_data_pro = temp_data_pro %>%
      dplyr::mutate(label = stringr::str_glue(label_string))
  } else {
    temp_data_pro = temp_data_pro %>%
      dplyr::mutate(label = .data[[label_attribute]])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # palette
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pal_fun = switch(
    pal_name
    ,"rocket"  = viridis::rocket
    ,"magma"   = viridis::magma
    ,"inferno" = viridis::inferno
    ,"plasma"  = viridis::plasma
    ,"viridis" = viridis::viridis
    ,"cividis" = viridis::cividis
    ,"turbo"   = viridis::turbo
    ,viridis::rocket
  )

  if(flag_factor){

    bins = length(unique(temp_data_pro[[map_color_attr]]))

    temp_pal = leaflet::colorFactor(
      palette = pal_fun(n = bins, direction = 1)
      ,domain = temp_data_pro[[map_color_attr]]
      ,reverse = FALSE
    )

  } else {

    temp_pal = leaflet::colorNumeric(
      palette = pal_fun(20)
      ,domain = temp_data_pro[[map_color_attr]]
      ,reverse = FALSE
    )

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # layer + legend
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp_output = map_obj %>%
    leaflet::addCircleMarkers(
      data = temp_data_pro
      ,fillColor  = ~temp_pal(temp_data_pro[[map_color_attr]])
      ,fillOpacity = 0.6
      ,color = "black"
      ,opacity  = 0.6
      ,weight = 1
      ,radius = 5
      ,group = layer_name
      ,label = temp_data_pro[["label"]]
      ,popup  = temp_data_pro[["popup"]] %>% purrr::map(htmltools::HTML)
    ) %>%
    leaflet::addLegend(
      position = "bottomleft"
      ,title = layer_name
      ,group = layer_name
      ,pal = temp_pal
      ,values = temp_data_pro[[map_color_attr]]
    )

  return(temp_output)

}



