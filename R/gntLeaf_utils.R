#' Internal helpers shared by the gntLeaf_* generators
#'
#' Small, unexported utilities used by [gntLeaf_gen_map_layer()],
#' [gntLeaf_gen_map_layers()], and [gntLeaf_gen_leaflet_map()]:
#' \itemize{
#'   \item `.gntLeaf_clean_id()` -- turns a group name into a safe object id
#'     (`"VZN Priority Zns (Bikes)"` -> `"vzn_priority_zns_bikes"`), used to
#'     name every generated object (`pal_{id}`, `{id}_sd`, `flt_{id}_{col}`)
#'     so layers never collide;
#'   \item `.gntLeaf_resolve_data()` -- evaluates a `data_name` string in the
#'     caller's environment (so `lst[["name"]]` works);
#'   \item `.gntLeaf_resolve_def()` -- accepts a `gntLeaf_def_*()` object, a
#'     plain (partial) list, or `NULL`, and returns a validated def object;
#'   \item `.gntLeaf_fmt()` -- formats an R value as code text;
#'   \item `.gntLeaf_sym()` -- backticks non-syntactic column names;
#'   \item `.gntLeaf_header()` -- pads a section header to 80 characters;
#'   \item `.gntLeaf_comment()` -- comments out lines of generated code,
#'     keeping their indentation.
#' }
#'
#' @keywords internal
#' @name gntLeaf_utils
NULL

.gntLeaf_clean_id = function(x){
  x = tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x = gsub("^_+|_+$", "", x)
  if(grepl("^[0-9]", x)) x = paste0("l_", x)
  x
}

.gntLeaf_resolve_data = function(data_name, envir){
  tryCatch(
    eval(parse(text = data_name), envir = envir)
    ,error = function(e){
      stop(sprintf("could not find `%s` in the calling environment: %s"
                   ,data_name, conditionMessage(e)), call. = FALSE)
    }
  )
}

.gntLeaf_resolve_def = function(x, constructor, arg){
  cls = paste0("gntLeaf_", arg)
  if(is.null(x)) return(NULL)
  if(inherits(x, cls)) return(x)
  if(!is.list(x)){
    stop(sprintf("%s must be gntLeaf_%s(), a list, or NULL", arg, arg), call. = FALSE)
  }
  ok = setdiff(names(formals(constructor)), "...")
  has_dots = "..." %in% names(formals(constructor))
  nms = names(x)
  if(!has_dots && !is.null(nms)){
    bad = setdiff(nms[nms != ""], ok)
    if(length(bad) > 0){
      stop(sprintf("unknown %s entries: %s (allowed: %s)"
                   ,arg, paste(bad, collapse = ", "), paste(ok, collapse = ", "))
           ,call. = FALSE)
    }
  }
  do.call(constructor, x)
}

.gntLeaf_fmt = function(x){
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

.gntLeaf_sym = function(col){
  if(make.names(col) == col) col else paste0("`", col, "`")
}

.gntLeaf_header = function(label, width = 80){
  paste0(label, strrep("=", max(3, width - nchar(label))))
}

.gntLeaf_comment = function(txt){
  lines = strsplit(txt, "\n", fixed = TRUE)[[1]]
  paste(sub("^(\\s*)", "\\1# ", lines), collapse = "\n")
}
