#' Calculate Bearing for Polylines in Spatial Data Frame
#'
#' This function calculates the bearing or heading in degrees for polylines in a Spatial Data Frame (SF).
#'
#' @param df A Spatial Data Frame (SF) containing start and end coordinates for polylines.
#' @param startLatCol Column name for the start latitude coordinates in the SF.
#' @param startLonCol Column name for the start longitude coordinates in the SF.
#' @param endLatCol Column name for the end latitude coordinates in the SF.
#' @param endLonCol Column name for the end longitude coordinates in the SF.
#' @param floor_divide An optional parameter to divide the calculated bearings for simplicity. Default is 1.
#'
#' @return A modified SF with an additional "bearing" attribute representing the directionality of the polylines.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result_sf <- st_calculate_heading_dataframe(input_sf, "start_lat", "start_lon", "end_lat", "end_lon", floor_divide = 5)
#' }
#'
#' @importFrom sf st_set_geometry
#' @importFrom dplyr select
#' @importFrom gauntlet floor_divide
#'
#' @export
st_calculate_heading_dataframe <- function(df, startLatCol, startLonCol, endLatCol, endLonCol
                                           ,floor_divide = 1) {
  # Convert degrees to radians
  df$lat1_rad <- df[[startLatCol]] * pi / 180
  df$lon1_rad <- df[[startLonCol]] * pi / 180
  df$lat2_rad <- df[[endLatCol]] * pi / 180
  df$lon2_rad <- df[[endLonCol]] * pi / 180

  # Calculate differences in coordinates
  df$dlat <- df$lat2_rad - df$lat1_rad
  df$dlon <- df$lon2_rad - df$lon1_rad

  # Apply Haversine formula
  df$central_angle <- acos(sin(df$lat1_rad) * sin(df$lat2_rad) + cos(df$lat1_rad) * cos(df$lat2_rad) * cos(df$dlon))
  df$bearing <- (atan2(sin(df$dlon) * cos(df$lat2_rad), cos(df$lat1_rad) * sin(df$lat2_rad) - sin(df$lat1_rad) * cos(df$lat2_rad) * cos(df$dlon))) * (180 / pi)

  # Adjust bearing to be between 0 and 360 degrees
  df$bearing <- (df$bearing + 360) %% 360
  df$bearing = gauntlet::floor_divide(df$bearing, floor_divide)

  df = df %>%
    select(!c(lat1_rad, lon1_rad, lat2_rad, lon2_rad
              ,dlat, dlon, central_angle))

  return(df)
}
