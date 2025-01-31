#' Calculate Radius of Curvature for Spatial Data
#'
#' This function calculates the radius of curvature and curvature for spatial data points using a sliding window of lagged and lead points. It is designed to work with data that includes latitude and longitude coordinates and requires the data to be in a projected CRS, such as EPSG:32610.
#'
#' @param data A data frame or tibble containing at least two columns: \code{lat} (latitude) and \code{lon} (longitude).
#' @param n Integer. The number of points to lag and lead when calculating the radius of curvature. Default is 5.
#' @param radial_limit Numeric. The maximum allowable radius of curvature. Any radius exceeding this value will be capped. Default is 20000.
#'
#' @return A data frame with additional columns:
#'   \describe{
#'     \item{\code{lat1, lon1}}{The lagged latitude and longitude values.}
#'     \item{\code{lat3, lon3}}{The lead latitude and longitude values.}
#'     \item{\code{d12, d23, d31}}{Distances between lag, middle, and lead points.}
#'     \item{\code{s}}{The semi-perimeter of the triangle formed by the three points.}
#'     \item{\code{area}}{The area of the triangle formed by the three points.}
#'     \item{\code{radius}}{The radius of curvature, capped at \code{radial_limit}.}
#'     \item{\code{curvature}}{The curvature, calculated as the reciprocal of the radius of curvature.}
#'   }
#'
#' @details
#' The function works as follows:
#' 1. Calculates lagged and lead points based on the input \code{n}.
#' 2. Computes the distances between the three points: lag, current, and lead.
#' 3. Calculates the semi-perimeter and area of the triangle formed by these points.
#' 4. Determines the radius of curvature as \( R = \frac{d12 \cdot d23 \cdot d31}{4 \cdot \text{area}} \).
#' 5. Caps the radius of curvature at \code{radial_limit} to avoid extreme values.
#' 6. Computes curvature as the reciprocal of the radius of curvature. For degenerate triangles (area = 0), curvature is set to 0.
#'
#' @note
#' This function assumes the input data is in a projected CRS (e.g., EPSG:32610). If using a geographic CRS (e.g., latitude/longitude), distances will not be accurate, and the function may yield incorrect results.
#'
#' @importFrom dplyr filter lag lead mutate
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- st_radius_of_curvature(data = my_data, n = 5, radial_limit = 20000)
#' }
st_radius_of_curvature <- function(data, n = 5, radial_limit = 20000) {
  # Ensure the required columns exist
  if (!all(c("lat", "lon") %in% colnames(data))) {
    stop("The input data must contain columns named 'lat' and 'lon'.")
  }

  # Add lag and lead points
  data <- data %>%
    dplyr::mutate(
      lat1 = dplyr::lag(lat, n),  # Lag (past point)
      lon1 = dplyr::lag(lon, n),
      lat3 = dplyr::lead(lat, n), # Lead (future point)
      lon3 = dplyr::lead(lon, n)
    ) %>%
    dplyr::filter(!is.na(lat1) & !is.na(lat3)) # Remove rows where lag/lead points are NA

  # Calculate distances between the three points
  data <- data %>%
    dplyr::mutate(
      d12 = sqrt((lat - lat1)^2 + (lon - lon1)^2),  # Distance between lag and middle
      d23 = sqrt((lat3 - lat)^2 + (lon3 - lon)^2),  # Distance between middle and lead
      d31 = sqrt((lat3 - lat1)^2 + (lon3 - lon1)^2) # Distance between lag and lead
    )

  # Calculate the semi-perimeter (s) and the area of the triangle
  data <- data %>%
    dplyr::mutate(
      s = (d12 + d23 + d31) / 2,  # Semi-perimeter
      area = sqrt(s * (s - d12) * (s - d23) * (s - d31))  # Area of the triangle
    )

  # Calculate radius of curvature (R)
  data <- data %>%
    dplyr::mutate(
      radius = as.numeric((d12 * d23 * d31) / (4 * area)) %>%
        ifelse(.>radial_limit, radial_limit, .)
      ,curvature = ifelse(area == 0, Inf, 1 / radius) %>%
        ifelse(is.infinite(.), 0, .)# Curvature is reciprocal of radius
    )

  return(data)
}
