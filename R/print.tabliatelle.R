#' @noRd
#'
#' @keywords internal
#'
#' @export

print.tabliatelle <- function(x, ...){

  # https://stackoverflow.com/questions/5237557/extract-every-nth-element-of-a-vector
  nth_element <- function(vector, starting_position, n) {
    vector[seq(starting_position, length(vector), n)]
  }


  # rounded df with the last column dropped
  y <- data.frame(lapply(x$Proportions.table,
                         function(x) if(is.numeric(x)) round(x, 2) else x))[, -ncol(x$Proportions.table)]
  y_names <- colnames(y)# <- gsub(pattern = "Theta_mean", replacement = "\U03D1", x = y )
  y_names <- gsub(pattern = "Theta_mean", replacement = "Theta", y_names)
  y_names <- gsub(pattern = "Class_Interval", replacement = "Class Interval", y_names)
  y_names <- gsub(pattern = "Prop.", replacement = "", y_names)

  colnames(y)<- y_names
  # Order the y dataframe
  #y$Class_Interval <- factor(y$Class_Interval)
  #y <- y[order(y$Item, y$Class_Interval),]

  # format the item column
  i_list <- nth_element(as.character(y$Item), 1, 3)
  i_out <- NULL
  i <- NULL
  for (i in 1:length(i_list)) {
    item_i <- c(i_list[i], rep(" ", 2))
    i_out <- append(i_out, item_i)
  }

  # format the beta column
  b_list <- nth_element(as.character(y$Beta), 1, 3)
  b_out  <- NULL
  b      <- NULL
  
  for (b in 1:length(b_list)) {
    beta_b <- c(b_list[b], rep(" ", 2))
    b_out <- append(b_out, beta_b)
  }
  b_out

  y$Item <- i_out
  y$Beta <- b_out


  cat("Class Interval Proportion Table", "\nN Class Intervals: 3", "\n")
  cat("Class size (n): ",  sprintf("%s: %s", names(x$Class.Size), x$Class.Size), "\n\n")
  cat("Response Options: ", sprintf("%s: %s", names(x$Response.Options), x$Response.Options), "\n\n")

  cat("Data:\n")
  print(y)
}
