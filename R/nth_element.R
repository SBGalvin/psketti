#' @noRd
#'
#' @description A function that gets every nth element from a list
#'
#' @keywords internal
#'
#' @export

# https://stackoverflow.com/questions/5237557/extract-every-nth-element-of-a-vector
nth_element <- function(vector, starting_position, n) {
  vector[seq(starting_position, length(vector), n)]
}
