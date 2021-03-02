#' A function for calculating a condtional probability to a dichotomous item.
#'
#' @param theta an estimated ability value
#' @param D an unfolding parameter
#' @param a an estimated discrimination parameter
#' @param b a difficulty parameter
#' @param cx a guessing parameter
#'
#' @noRd
Pr <- function(theta, D = 1, a = 1,
               cx = 0, b = 0) {
  
  e <- exp(D * a * (theta - b))
  cx + (1 - cx) * e/(1 + e)
}