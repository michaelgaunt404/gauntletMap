% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_true_midpoint.R
\name{st_true_midpoint}
\alias{st_true_midpoint}
\title{Get the true midpoint along a curved line}
\usage{
st_true_midpoint(sf_object, crs = 2781)
}
\arguments{
\item{sf_object}{A spatial object representing a curved line}

\item{crs}{An integer input indicating which CRS to use when extracting the midpoint - default is 2781}
}
\value{
A spatial object representing the true midpoint along the curved line
}
\description{
This function gets the true midpoint along a curved line represented by a spatial object.
It transforms the object to a new CRS, casts it to a linestring, selects the relevant columns,
extracts the coordinates, finds the middle point, and converts it to a spatial object with the
original CRS.
}
\examples{
\dontrun{
library(sf)

curved_line = st_sfc(
  st_linestring(
    rbind(
      c(-122.68, 45.52)
      ,c(-122.63, 45.55)
      ,c(-122.58, 45.57)
      ,c(-122.55, 45.54)
    ))
  ,crs = 4326) \%>\%
  st_as_sf()

mapview::mapview(curved_line) +
  mapview::mapview(st_true_midpoint(curved_line))
}
}
