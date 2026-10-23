#' Generate map layers for every element of a named list
#'
#' Plural wrapper around [gntLeaf_gen_map_layer()] for named lists of sf
#' objects, e.g. `nyc_portal_layers_vzn`. For each element it builds the
#' `data_name` (`'lst[["name"]]'`), uses the element name as the `group`,
#' and passes the shared arguments in `...`. Geometry, palette type, and
#' filter types are detected per element, so lists that mix points, lines,
#' and polygons just work.
#'
#' Replaces the script-level pattern of
#' `names() %>% keep(nrow > 0) %>% map(~gntLeaf_gen_map_layer(...))` plus
#' `temp_geom_lookup()` and `unname()`.
#'
#' @param list_name character. Name of the list object (e.g.
#'   `"nyc_portal_layers_vzn"`) -- NOT the object itself. Every element must
#'   be named.
#' @param ... Arguments shared by every layer, passed to
#'   [gntLeaf_gen_map_layer()] (e.g. `palette_col = "layer_type"`,
#'   `def_size = NULL`). `data_name` and `group` are set per element and
#'   can't be passed here.
#' @param overrides named list, keyed by element name, each a list of
#'   arguments that replace the shared ones for that element only, e.g.
#'   `list("NYC Bike Counters" = list(def_style = list(fill_color = "violet")))`.
#'   An override replaces the whole argument (a `def_style` override is not
#'   merged with a shared `def_style`). Use `group` here to rename a layer.
#' @param drop_empty logical. Skip elements with zero rows (with a message)
#'   instead of erroring. Default `TRUE`.
#' @param envir environment. Where `list_name` is evaluated. Default: the
#'   caller's environment.
#'
#' @return A named list of `gntLeaf_layer` objects, one per kept element.
#'   Pass it straight to [gntLeaf_gen_leaflet_map()], alone or nested with
#'   other layers.
#' @export
#'
#' @seealso [gntLeaf_gen_map_layer()], [gntLeaf_gen_leaflet_map()]
#'
#' @examples
#' \dontrun{
#' vzn_layers = gntLeaf_gen_map_layers(
#'   "nyc_portal_layers_vzn"
#'   ,def_size = NULL
#'   ,overrides = list(
#'     "NYC Bike Counters" = list(def_style = list(fill_color = "violet"))
#'     ,"VZN Priority Intr" = list(label_col = "label")
#'   )
#' )
#'
#' gntLeaf_gen_leaflet_map(list(layer_cor, vzn_layers))
#'
#' # to exclude an element, subset the list first and pass that name
#' vzn_keep = nyc_portal_layers_vzn[setdiff(names(nyc_portal_layers_vzn), "VZN Sip Corr")]
#' vzn_layers = gntLeaf_gen_map_layers("vzn_keep", def_size = NULL)
#' }
gntLeaf_gen_map_layers = function(
    list_name
    ,...
    ,overrides = list()
    ,drop_empty = TRUE
    ,envir = parent.frame()
){
  lst = .gntLeaf_resolve_data(list_name, envir)
  if(!is.list(lst) || inherits(lst, "data.frame")){
    stop(sprintf("`%s` must be a named list of sf objects", list_name), call. = FALSE)
  }
  nms = names(lst)
  if(is.null(nms) || any(nms == "")){
    stop(sprintf("every element of `%s` must be named", list_name), call. = FALSE)
  }

  shared = list(...)
  if(any(c("data_name", "group") %in% names(shared))){
    stop("data_name and group are set per element; use overrides to rename"
         ,call. = FALSE)
  }

  unknown = setdiff(names(overrides), nms)
  if(length(unknown) > 0){
    warning(sprintf("overrides for unknown element(s): %s"
                    ,paste(unknown, collapse = ", ")), call. = FALSE)
  }

  n_rows = vapply(lst, function(x) if(is.null(x)) 0L else nrow(x), integer(1))
  if(any(n_rows == 0)){
    if(!drop_empty){
      stop(sprintf("empty element(s): %s", paste(nms[n_rows == 0], collapse = ", "))
           ,call. = FALSE)
    }
    message(sprintf("skipping empty element(s): %s"
                    ,paste(nms[n_rows == 0], collapse = ", ")))
    nms = nms[n_rows > 0]
  }

  out = lapply(nms, function(nm){
    args = c(list(data_name = sprintf('%s[["%s"]]', list_name, nm), group = nm)
             ,shared)
    ov = overrides[[nm]]
    #manual merge so an override can set an argument to NULL
    for(k in names(ov)) args[k] = list(ov[[k]])
    args$envir = envir
    do.call(gntLeaf_gen_map_layer, args)
  })
  names(out) = nms
  out
}
