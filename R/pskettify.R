#' @title pskettify your data
#'
#' @name pskettify
#'
#' @description This function extracts data from an eRm object of class 'RM' and
#'     converts to a format for plotting data. This also computes the empirical
#'     response values and empirical confidence intervals.
#'
#' @usage \code{pskettify(eRm.obj)}
#'
#' @param eRm.obj an eRm object of class 'RM'.
#' @param conf.level the confidence level for empirical response curve. Defaults to 0.95.
#' @param Theta.lwr The lowest limit of the latent dimension. Defaults to -6.
#' @param Theta.upr The highest limit of the latent dimension. Defaults to 6.

#' @return output_list containing presp and emp_ICC.
#' @return presp a data frame of ability (Theta) and conditional response
#'     probabilities to each item.
#' @return emp_ICC a dataframe containing proportion values and confidence intervals
#'    for ability class intervals to each item.
#'
#' @export
#'
#' @examples
#' library(eRm)
#' data(FakeData)
#'
#' #source('R/pskettify.R')
#'
#' Fake_Data_scores <- reshape(FakeData[, c("ID", "Item", "X")],
#'                             timevar = "Item",
#'                             idvar = "ID",
#'                             direction = "wide")
#'
#' names(Fake_Data_scores) <- c("ID", paste0("i", sprintf(fmt  = "%02d", 1:23)))
#' row.names(Fake_Data_scores) <- Fake_Data_scores$ID
#' Fake_Data_scores$ID <- NULL
#'
#' fake_rm <- RM(Fake_Data_scores)
#'
#' psk_data  <- pskettify(fake_rm)

pskettify <- function(eRm.obj, conf.level = .95, Theta.lwr = -6, Theta.upr = 6){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # later add to internal function
  # Confidence interval functions ~~#
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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Rasch Response Probability functions ~#
  Pr <- function(theta, D = 1, a = 1,
                 cx = 0, b = 0) {

    e <- exp(D * a * (theta - b))
    cx + (1 - cx) * e/(1 + e)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Checks:
  obj.eRm         <- eRm.obj
  obj.model       <- obj.eRm$model  # check ordering of this section


  # warning messages for function
  if(!"eRm" %in% class(obj.eRm)) stop("Object is not of class eRm: Please ensure your input object is an eRm object with model RM.")
  # Change (1) to (2) when PCM functionality is built in:
  # (1):
  if(!obj.eRm$model == "RM") stop("Model not of type RM. Please ensure eRm object is a fitted Dichotmous Rasch Model; use eRm::RM().")
  # (2)
  #if(!obj.eRm$model %in% c("RM", "PCM")) stop("Model not of type RM or PCM. Please ensure eRm object is a fitted dichotmous Rasch Model or Partial Credit Model")
  # if pass checks do this:
  X               <- data.frame(obj.eRm$X)
  difficulty      <- obj.eRm$betapar *-1
  item.names      <- colnames(X)
  itm.df          <- data.frame(Item = item.names,
                                Beta = difficulty)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Data 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  theta   <- seq(Theta.lwr, Theta.upr, .1)
  presp   <- NULL

  for (i in 1: length(difficulty)) {

    Probs <- Pr(theta = theta, b = difficulty[i])
    presp <- cbind(presp, Probs)

  }

  colnames(presp) <- paste0(item.names)

  presp       <- as.data.frame(presp)
  presp$Theta <- theta
  presp       <- presp[, c("Theta", item.names)]
  presp       <- reshape(presp, direction = "long",
                         v.names = "Probs",
                         varying = item.names,
                         times = item.names,
                         timevar = "Item")

  rownames(presp) <- NULL
  presp$id        <- NULL
  presp           <- presp[, c("Item", "Theta", "Probs")]

  presp <- merge(presp, itm.df)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Data 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ppar          <- eRm::person.parameter(obj.eRm)
  theta_score   <- ppar$pred.list[[1]]$y

  freq.table      <- NULL
  rel.freq        <- NULL
  Item            <- NULL
  Theta           <- NULL
  crt             <- NULL
  cy              <- NULL
  ci              <- NULL
  freq.bound      <- NULL
  freq_list       <- NULL

  for (j in 1:ncol(X)) {

    # Frequency tables
    freq.table <- as.matrix(table(rowSums(X), X[, j]))
    rel.freq   <- freq.table[, 2]/rowSums(freq.table)

    idx  <- as.numeric(rownames(freq.table))
    nn   <- rowSums(freq.table)


    # Confidence interval
    # Item Name
    Item  <- rep(item.names[j], length(idx)) # this is failing?
    Theta <- theta_score[idx + 1]

    crt <- cbind(rel.freq*nn, nn)
    cy  <- apply(crt, 1L, function(x) {CINT(x[1L], x[2L], 0.95)})
    ci  <- t(cy)

    colnames(ci) <- c("lci", "uci")

    # Bind together
    freq.bound <- cbind(Item, idx, Theta, freq.table, nn, rel.freq, ci)


    # Append to list
    freq_list[[j]] <- freq.bound

  }

  dfs <- lapply(freq_list, data.frame, stringsAsFactors = FALSE)
  emp_ICC_df <- dplyr::bind_rows(dfs)  # useful

  # added 21/02/2021
  emp_ICC_df[-1] <- data.frame(lapply(emp_ICC_df[-1], function(x) as.numeric(as.character(x))))
  row.names(emp_ICC_df) <- NULL # remove row names

  names(emp_ICC_df)[names(emp_ICC_df) == 'X0'] <- 'Correct'
  names(emp_ICC_df)[names(emp_ICC_df) == 'X1'] <- 'Inorrect'


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output_list <- list()
  output_list[[1]] <- presp
  output_list[[2]] <- emp_ICC_df
  names(output_list) <- c("presp", "emp_ICC")

  # apply class name
  class(output_list) <- c("pskettified", "dRM")
  # if pcm was used return: ADD LATER
  #class(output_list) <- c("pskettified", "PCM")

  return(output_list)

}
