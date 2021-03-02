#' @noRd
#'
#' @keywords internal
#'
#' @export

print.pskettified <- function(x,...){
  
  x_out <- x$ItemDF
  
  if (class(x)[[2]] == "RM"){
    x_out$Beta <- round(x_out$Beta, 3)
    x_out$Se <- round(x_out$Se, 3)
  } else if( class(x)[[2]] == "PCM"){
    x_out$Beta <- round(x_out$Beta, 3)
    x_out$Se <- round(x_out$Se, 3)
    x_out$tau <- round(x_out$tau, 3)
  }
  
  
  cat("Model: ", class(x)[[2]], "\n")
  print(x_out)
}
