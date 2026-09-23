#' Linearly rescale a numeric vector to [lower, upper]
#'
#' NAs stay NA. If every value is identical (zero range), returns the
#' midpoint of `lower`/`upper` for all non-NA entries instead of NaN.
#'
#' @param column numeric vector.
#' @param lower,upper numeric. Output range.
#' @return numeric vector, same length as `column`.
#' @export
rescale_vector = function(column, lower = 1, upper = 10){
  rng = range(column, na.rm = TRUE)
  if(!all(is.finite(rng)) || diff(rng) == 0){
    out = rep((lower + upper) / 2, length(column))
    out[is.na(column)] = NA
    return(out)
  }
  lower + (column - rng[1]) / diff(rng) * (upper - lower)
}
