#' @title Plot your pskettified data
#'
#' @name psketti
#'
#' @description This function extracts data from an eRm object of class 'RM' and converts to a format for plotting data.
#'     This also computes the empirical response values and empirical confidence intervals
#'
#' @usage \code{psketti(pskettified_data)}
#'
#' @param pskettified_data a list object generated from eRm object class'RM' using \code{pskettify()}.
#' @param p.style a character string for plotting style options are present for
#'     coloured plots, or print for black and white plots. Defaults to "present".
#' @param p.IRFLocation logical, plots reference lines for Rasch IRF location. Defaults to TRUE.
#' @param p.empCI logical, plots confidence intervals for empirical points, calculated using \code{pskettify()}.
#'     Defaults to \code{TRUE}.
#' @param p.empICC logical, plots empirical ICC for item. Defaults to TRUE.
#' @param p.empPoints logical, plots empirical points for based on class intervals/ score
#'     groups generated with \code{pskettify()}. Defaults to \code{TRUE}.
#'
#' @return A list object containing multiple psketto plots each element contains the plot
#'     object[[1]] and item name object[[2]]
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_line scale_colour_manual theme_minimal theme ylab xlab element_blank
#' @importFrom psketti print.psketti
#'
#' @export
#'
#' @examples
#' library(eRm)
#' library(ggplot2)
#' library(psketti)
#' data(FakeData)  # load dataset fake data
#'
#' # Set up dataframe for eRm, long to wide with dichotmous data
#' Fake_Data_scores <- reshape(FakeData[, c("ID", "Item", "X")],
#'                             timevar = "Item",
#'                             idvar = "ID",
#'                             direction = "wide")
#' names(Fake_Data_scores) <- c("ID", paste0("i", sprintf(fmt  = "%02d", 1:23)))
#' row.names(Fake_Data_scores) <- Fake_Data_scores$ID
#' Fake_Data_scores$ID <- NULL
#'
#' fake_rm <- RM(Fake_Data_scores) # Fit Rasch Model
#'
#' psk_data <-pskettify(eRm.obj = fake_rm)
#'
#' # plot ICC for all item
#' psk_IRF <- psketti(psk_data)
#' psk_IRF[["i06"]]
#' psk_IRF[["i12"]]


psketti <- function(pskettified_data, p.style = "present",
                    p.IRFLocation = TRUE, p.empCI = TRUE, p.empICC = TRUE, p.empPoints = TRUE){

  x <- pskettified_data
  Model_type <- class(x)[2]



  # include list of item names in input object
  j.list <- unique(x$presp$Item)

  ICC_out <- list()
  j <- NULL

  for (j in 1:length(j.list)) {

    x_j <- j.list[j]
    # create psketto function for gg_single_ICC2
    ICC_plot <- psketto(x, item = x_j, item.label = x_j,
                        style = p.style,
                        IRFLocation = p.IRFLocation, empCI = p.empCI,
                        empICC = p.empICC, empPoints = p.empPoints)

    # append the plot (resid.plot) to the plot list with the item name
    ICC_out[[j]] <-  list(ICC_plot)
  }

  ICC_out <- setNames(ICC_out, j.list)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output_list <- list()
  output_list[[1]] <- paste0("Empirical and Theoretical Rasch IRF")
  output_list[[2]] <- Model_type
  output_list[[3]] <- ICC_out

  names(output_list) <- c("xlbl", "Model.Type", "Plot.List")
  class(output_list) <- "psketti"


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  return(invisible(output_list)) # invisible prevents plots from automatically printing
  print.psketti(x = output_list)
}

