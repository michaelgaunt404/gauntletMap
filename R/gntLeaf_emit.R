#' Print (and optionally clipboard-copy) a generated code block
#'
#' Internal helper shared by [gntLeaf_gen_map_layer()] and
#' [gntLeaf_gen_leaflet_map()]. Prints `txt` to the console via [base::cat()]
#' and, when `clip = TRUE`, also copies it to the clipboard via
#' [clipr::write_clip()] -- falling back to a message (rather than an error)
#' if `clipr` isn't installed, since printing still succeeds either way.
#'
#' @param txt Character scalar, the generated code block to print.
#' @param clip Logical, whether to also copy `txt` to the clipboard.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly, `txt`.
#'
#' @keywords internal
#'
#' @importFrom clipr write_clip
#'
#' @examples
#' \dontrun{
#' .emit("leaflet::leaflet() %>% leaflet::addTiles()")
#' .emit("leaflet::leaflet() %>% leaflet::addTiles()", clip = TRUE)
#' }
gntLeaf_emit = function(txt, clip = FALSE){
  cat(txt, "\n")
  if(isTRUE(clip)){
    if(requireNamespace("clipr", quietly = TRUE)){
      clipr::write_clip(txt)
      message("-- copied to clipboard --")
    } else {
      message("clipr not installed; skipping clipboard copy")
    }
  }
  invisible(txt)
}






