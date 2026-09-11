#' Download or Locate a Local OSM PBF File
#'
#' Checks a local directory for an existing OSM `.pbf` file and returns its
#' path. If no `.pbf` is found, downloads one for the specified region using
#' [osmextract::oe_get()]. This avoids redundant downloads across sessions by
#' treating the directory as a local cache.
#'
#' @param region Character. The region to download if no `.pbf` is found
#'   locally. Passed directly to [osmextract::oe_get()]. Can be a place name
#'   (e.g. `"us/hawaii"`), a geographic identifier, or an `sfc` object.
#'   Defaults to `"us/hawaii"`.
#' @param dir_osm Character. Path to the directory used to store and look up
#'   `.pbf` files. Created recursively if it does not exist. Defaults to
#'   `"data/osmextract"`.
#'
#' @return Character. The file path to the first `.pbf` file found in
#'   `dir_osm`. If multiple `.pbf` files are present, only the first (as
#'   returned by [base::list.files()]) is returned.
#'
#' @note If multiple `.pbf` files exist in `dir_osm`, only the first is
#'   returned. This may be non-deterministic if the directory contains `.pbf`
#'   files for multiple regions. Consider using a dedicated per-region
#'   subdirectory to avoid ambiguity.
#'
#' @seealso [osmextract::oe_get()], [osmextract::oe_download()]
#'
#' @importFrom osmextract oe_get
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Download Hawaii OSM data to the default cache directory
#' pbf_path <- osmextract_get_pbf()
#'
#' # Use a custom region and directory
#' pbf_path <- osmextract_get_pbf(
#'   region  = "us/washington",
#'   dir_osm = "data/osm/washington"
#' )
#'
#' }
#'
#' @export
osmextract_get_pbf <- function(region = "us/hawaii", dir_osm = "data/osmextract") {

  if (!dir.exists(dir_osm)) dir.create(dir_osm, recursive = TRUE)

  pbf_files <- list.files(dir_osm, pattern = "\\.pbf$", full.names = TRUE)

  if (length(pbf_files) == 0) {
    message(glue::glue("No .pbf found in {dir_osm}, downloading {region}..."))
    osmextract::oe_get(region, download_directory = dir_osm, download_only = TRUE)

    pbf_files <- list.files(dir_osm, pattern = "\\.pbf$", full.names = TRUE)
    if (length(pbf_files) == 0) {
      stop("Failed to download .pbf file via oe_get().")
    }
  } else {
    message(glue::glue("Found existing .pbf: {basename(pbf_files)}"))
  }

  return(pbf_files[1])
}
