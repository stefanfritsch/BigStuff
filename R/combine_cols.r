#' Combine Factors Correctly
#'
#' Just c(.x, .y) will produce incorrect results of .x or .y is a factor. This
#' is a replacement that handles factors correctly (hopefully).
#'
#' @param .x A vector
#' @param .y A vector
#' @param .keep.factor Nothing yet
#'
combine_cols <- function(.x, .y, .keep.factor = TRUE)
{
  if(!is.factor(.x) && !is.factor(.y)) {
    c(.x, .y)
  } else {
    factor(c(as.character(.x), as.character(.y)))
  }
}
