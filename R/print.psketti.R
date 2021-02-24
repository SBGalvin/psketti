#' @noRd
#'
#' @keywords internal
#'
#' @export
# print function for output so the object doesn't clog up the R session
print.psketti <- function(x,...){
  xlblout <- x$xlbl
  y  <- data.frame(Item = sprintf("%s", names(x$Plot.List)),
                   Call =  sprintf("objectName$Plot.List[['%s']][[1]]",
                                   names(x$Plot.List)))

  cat(xlblout, "\n\n",
      "Model: ", x$model.type, "\n",
      "N items: ", length(x$Plot.List), "\n\n\n",
      "How To call item plots:", "\n\n")

  print(y)
}
