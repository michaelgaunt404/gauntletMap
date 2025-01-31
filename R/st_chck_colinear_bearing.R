#' Check Colinearity of Bearings
#'
#' This function checks whether two columns of bearings (in degrees) in a data frame are collinear within a specified error margin. It can be used to evaluate the directional consistency of network link bearings.
#'
#' @param df A data frame containing the bearing columns.
#' @param bearing_col_1 A string specifying the name of the first bearing column.
#' @param bearing_col_2 A string specifying the name of the second bearing column.
#' @param error A numeric value specifying the error margin (in degrees) for collinearity checks.
#'
#' @return A data frame with an added column `is_colinear`, indicating whether the bearings in each row are collinear within the specified error margin.
#'
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @export
#'
#' @examples
#' \dontrun{
#' data.frame(
#'   bearing_1 = c(0),
#'   bearing_2 = c(5, 6, 10, 11, 15, 355, 0)
#' ) %>%
#'   st_chck_colinear_bearing(
#'     "bearing_1", "bearing_2", error = 5
#'   )
#' }
st_chck_colinear_bearing <- function(df, bearing_col_1, bearing_col_2, error) {
  # Normalize bearings to the range [0, 360)
  normalize_bearing <- function(bearing) {
    return ((bearing %% 360 + 360) %% 360)
  }

  # Calculate the difference between bearings and normalize to the range [0, 360)
  bearing_difference <- function(bearing1, bearing2) {
    diff <- normalize_bearing(bearing2) - normalize_bearing(bearing1)
    return ((diff %% 360 + 360) %% 360)
  }

  # Check if bearings are collinear within the error
  check_colinear <- function(bearing1, bearing2, error) {
    diff <- bearing_difference(bearing1, bearing2)
    return (diff <= error || diff >= (360 - error) || abs(diff - 180) <= error)
  }

  # Apply the normalization and colinear check to the dataframe
  df$normalized_bearing_1 <- normalize_bearing(df[[bearing_col_1]])
  df$normalized_bearing_2 <- normalize_bearing(df[[bearing_col_2]])
  df$bearing_difference <- mapply(bearing_difference, df$normalized_bearing_1, df$normalized_bearing_2)
  df$is_colinear <- mapply(check_colinear, df$normalized_bearing_1, df$normalized_bearing_2, MoreArgs = list(error = error))

  df = df %>%
    dplyr::select(!dplyr::starts_with("normalized_bearing"))

  return (df)
}
