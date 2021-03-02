#' Internal functions from eRm for calculating binomial confidence intervals
#'     in dichotomous Rasch models.
#'
#' @param x a proportion value.
#' @param n total number.
#' @param alpha alpha level for CI.
#' @param conf.level Confidence level.
#' 
#' @noRd
# 00.1)
ci.L <- function(x, n, alpha) {
  if (x <= 0)
    0
  else qbeta(alpha, x, n - x + 1)
}
# 00.2)
ci.U <- function(x, n, alpha) {
  if (x >= n)
    1
  else qbeta(1 - alpha, x + 1, n - x)
}
# 00.3)
CINT <- function(x, n, conf.level) {
  # Alpha level (two tail)
  alpha <- (1 - conf.level)/2
  c(ci.L(x, n, alpha), ci.U(x, n, alpha))
}