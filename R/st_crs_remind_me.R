#' st_crs_remind: Generate a Data Frame with EPSG Codes for Mapping Purposes
#'
#' This function creates a data frame containing information about different EPSG codes commonly used in mapping applications. The data frame includes columns for the name of the coordinate reference system (CRS), the corresponding EPSG code, and the unit of measurement associated with the CRS.
#'
#' @return A data frame with columns:
#'   - Name: Name of the coordinate reference system (CRS).
#'   - Code: EPSG code associated with the CRS.
#'   - Unit: Unit of measurement for the CRS.
#'
#' @examples
#' st_crs_remind()
#'
#' @export
st_crs_remind_me = function(){
  df = data.frame(rbind(
    c("WGS 84", 4326, "lat/lon"),
    c("NAD83 / Washington North (ftUS)", 4326, "US survey foot"),
    c("WGS 84 / UTM zone 18N", 32618, "metre"),
    c("WGS 84 / UTM zone 10N", 32610, "metre")
  ))

  colnames(df) = c("Name", "Code", "Unit")

  return(df)
}
