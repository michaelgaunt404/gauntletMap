#TODO PUT INTO GAUNTLET MAP
st_reproject_coords <- function(df, lng, lat, to, from = 4326, suffix = NULL) {
  lng <- rlang::ensym(lng)
  lat <- rlang::ensym(lat)
  
  m <- sf::sf_project(
    from = sf::st_crs(from)
    ,to = sf::st_crs(to)
    ,pts = cbind(dplyr::pull(df, !!lng), dplyr::pull(df, !!lat))
  )
  
  out_lng <- if (is.null(suffix)) lng else rlang::sym(paste0(rlang::as_string(lng), suffix))
  out_lat <- if (is.null(suffix)) lat else rlang::sym(paste0(rlang::as_string(lat), suffix))
  
  df %>%
    dplyr::mutate(
      !!out_lng := m[, 1]
      ,!!out_lat := m[, 2]
    )
}
