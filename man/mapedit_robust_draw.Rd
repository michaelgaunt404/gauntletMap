% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapedit_robust_draw.R
\name{mapedit_robust_draw}
\alias{mapedit_robust_draw}
\title{Function to Robustly Draw Features Using mapedit Package}
\usage{
mapedit_robust_draw()
}
\value{
A Spatial or Simple Features geometry representing the user-created features.
}
\description{
This function provides a robust way to draw features using the mapedit package. It includes try-catch blocks to handle errors and prevent improper generation of shapes. The user is prompted to repeat the shape generation process in case of errors or review the generated geometry before continuing.
}
\examples{
\dontrun{
  user_created_geometry <- my_draw_features_function()
}

\dontrun{
  user_created_geometry <- my_draw_features_function()
}
}
\references{
Author(s): Mike Gaunt
Year: 2023

Author(s): Your Name
Year: 2023
}
\seealso{
\code{\link{drawFeatures}}, \code{\link{st_transform}}, \code{\link{robust_prompt_used}}, \code{\link{mapview}}
}
\keyword{data}
\keyword{spatial}
\keyword{visualization}
