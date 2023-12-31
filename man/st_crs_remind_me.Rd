% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_crs_remind_me.R
\name{st_crs_remind_me}
\alias{st_crs_remind_me}
\title{st_crs_remind: Generate a Data Frame with EPSG Codes for Mapping Purposes}
\usage{
st_crs_remind_me()
}
\value{
A data frame with columns:
  - Name: Name of the coordinate reference system (CRS).
  - Code: EPSG code associated with the CRS.
  - Unit: Unit of measurement for the CRS.
}
\description{
This function creates a data frame containing information about different EPSG codes commonly used in mapping applications. The data frame includes columns for the name of the coordinate reference system (CRS), the corresponding EPSG code, and the unit of measurement associated with the CRS.
}
\examples{
st_crs_remind()

}
