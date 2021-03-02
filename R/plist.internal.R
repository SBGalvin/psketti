#' @title Rasch Probabilties
#'
#' Internal function from eRm for calculating Rasch IRF. In polytomous models
#'     this estimates the 0th category values.
#'     
#' @param object eRm object, an estimated model
#' @param theta a list of theta values, usually a sequence
#' @noRd

plist.internal <- function(object, theta){
  
  X <- object$X
  mt_vek <- apply(X, 2L, max, na.rm = TRUE)   # number of categories - 1 for each item
  mt_ind <- rep(seq_along(mt_vek), mt_vek)
  
  #--------compute list matrix of probabilites for fixed theta)
  p.list <- tapply(object$betapar, mt_ind, function(beta.i){
    beta.i <- c(0, beta.i)
    ind.h <- 0:(length(beta.i)-1)
    theta.h <- tcrossprod(ind.h, theta) # ind.h %*% t(theta) # multiply category with 
    tb <- exp(theta.h + beta.i)
    denom <- colSums(tb)
    pi.mat <- apply(tb, 1L, function(y){ y/denom })
    return(pi.mat)
  })
  return(p.list)
}
