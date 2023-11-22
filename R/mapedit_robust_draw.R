#' Function to Robustly Draw Features Using mapedit Package
#'
#' This function provides a robust way to draw features using the mapedit package. It includes try-catch blocks to handle errors and prevent improper generation of shapes. The user is prompted to repeat the shape generation process in case of errors or review the generated geometry before continuing.
#'
#' @return A Spatial or Simple Features geometry representing the user-created features.
#' @import gauntlet mapedit sf mapview
#'
#' @examples
#' \dontrun{
#'   user_created_geometry <- my_draw_features_function()
#' }
#'
#' @export
#' @importFrom mapedit drawFeatures
#' @importFrom sf st_transform
#' @importFrom gauntlet robust_prompt_used
#' @importFrom mapview mapview
#'
#' @seealso \code{\link{drawFeatures}}, \code{\link{st_transform}}, \code{\link{robust_prompt_used}}, \code{\link{mapview}}
#'
#' @keywords spatial data visualization
#' @references
#' Author(s): Mike Gaunt
#' Year: 2023
#'
#' @examples
#' \dontrun{
#'   user_created_geometry <- my_draw_features_function()
#' }
#' @export
#' @import gauntlet mapedit sf mapview
#'
#' @keywords spatial data visualization
#' @references
#' Author(s): Your Name
#' Year: 2023
#'
mapedit_robust_draw <- function() {
  check_repeat = TRUE

  while (check_repeat) {
    user_created_geometry = tryCatch({
      mapedit::drawFeatures() %>% sf::st_transform(4326)
    }, error = function(e) {
      message("User geometry NA!!!!")
      message(paste("Error occurred with shape generation:\n", e$message))
      message("Likely that you did not press ''Finish'' before pressing ''Done''")

      return(NA)
    })

    if (is.na(user_created_geometry)) {
      check_repeat = gauntlet::robust_prompt_used("repeat the shape generation process again")
    } else {
      message("User geometry not NA - good\n")

      check_review = gauntlet::robust_prompt_used("review your geometry (Y) or continue (N)")
      check_repeat = check_review

      if (check_review) {
        mapview::mapview(user_created_geometry) %>% print()

        check_repeat = gauntlet::robust_prompt_used("repeat the shape generation process again")
      }
    }
  }

  return(user_created_geometry)
}
