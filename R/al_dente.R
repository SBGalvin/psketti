#' Plot Dichotomous Rasch Model Residuals
#' 
#' @param eRm.obj an eRm object of class 'RM'.
#' 
#' @return A list object containing multiple residual plots and residuals
#'     Pearson correlations matrix.
#' @param style Options for plot colour; a character string for plotting style
#'     options are present for coloured plots, or print for black and white
#'     plots. Defaults to "present".
#'
#' @importFrom ggplot2 scale_shape_manual  guide_legend scale_y_continuous guides element_text
#' @importFrom stats cor
#' 
#' @export
#'     
#' @examples 
#' library(eRm)
#' library(psketti)
#' data("FakeData")
#' # restructure fake data
#' Fake_Data_scores <- reshape(FakeData[, c("ID", "Item", "X")],
#'                             timevar = "Item",
#'                             idvar = "ID",
#'                             direction = "wide")
#' # for eRm col names and row names
#' names(Fake_Data_scores) <- c("ID",
#'                              paste0("i",
#'                                     sprintf(fmt  = "%02d", 1:23)))
#'                                     
#' row.names(Fake_Data_scores) <- Fake_Data_scores$ID
#' Fake_Data_scores$ID         <- NULL
#'
#' fake_rm   <- RM(Fake_Data_scores) # Estimate Rasch model
#' 
#' residusals_plt <- al_dente(eRm.obj = fake_rm, style = "print)
#' residusals_plt$Plot.List[[1]][[1]]
# Rasch standardised residuals plot


al_dente <- function(eRm.obj, style = "present"){

  Item <- P_ind <- resid.cormat <- x1 <- x2 <- resid_matrix <- NULL

  # Stop if use entered incorrect style arg
  if (!style %in% c("print", "present")){
    stop("Incorrect option chosen for style. Please select one of 'present' or 'print'")
  }

  # stop if not eRM RM
  if(!eRm.obj$model %in% c("RM", "PCM")){
    stop(
      paste0(
        "Error: Model not of type RM.",
        "\n",
        "Please ensure eRm object is an estimated Dichotmous Rasch Model",
        "\n",
        "using eRm::RM()"
      )
    )
  }

  # correlations matrix
  # Rasch Residuals plots
  x1           <- eRm::person.parameter(eRm.obj)
  x2           <- eRm::itemfit(x1)
  resid_matrix <- x2$st.res

  # Residuals Correlation Matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  resid.cormat <- cor(resid_matrix)

  # Residuals Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # note: it is a good idea to ensure the items are named with a letter first!
  item.names <- colnames(resid_matrix)

  max.resid <- max(resid_matrix)
  min.resid <- min(resid_matrix)


  p_list     <- list()                              # null list
  item       <- NULL                                # null iterator

  for (item in item.names) {

    itm        <- item.names[item]
    resids.tmp <- resid_matrix[, item]
    plot_data  <- data.frame(P_ind = 1:length(resids.tmp), Item = resids.tmp)

    resid.plot <-  ggplot(data = plot_data, aes(y = Item, x = P_ind)) +
      geom_hline(aes(yintercept = -2, colour = 'myline1'), lty = 'dashed') +
      geom_hline(aes(yintercept = 2, colour = 'myline1'), lty = 'dashed') +
      geom_hline(aes(yintercept = 0, colour = 'myline2'), lty = 'solid')+
      geom_point(aes(shape = 'resid'), show.legend = FALSE) +

      ylab("Standardized Residuals")+
      xlab("Person Index")+
      scale_y_continuous(limits = c(min.resid, max.resid))+
      
      scale_shape_manual(values = c(resid = 21), labels = c("Std. Residual"), name = "")+
      guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))+

      theme_bw()+
      theme(panel.grid = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            plot.title = element_text(hjust = 0.5))
    
    if (style == "present") {
      resid.plot <- resid.plot + scale_colour_manual(values = c(myline1 = "tomato", myline2 = "steelblue"),
                                       labels = c("+/- 2 SD", "Expected"),
                                       name = "")
    } else if (style == "print"){
      resid.plot <- resid.plot + scale_colour_manual(values = c(myline1 = "grey", myline2 = "grey50"),
                                                     labels = c("+/- 2 SD", "Expected"),
                                                     name = "")
    }

    # append the plot (resid.plot) to the plot list with the item name
    p_list[[item]] <-  list(resid.plot, item.names[item])

  }

  # names of list elements
  p_list <- setNames(p_list, item.names)

  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output_list <- list()
  output_list[[1]] <- paste0("Rasch dichotomous model residuals")
  output_list[[2]] <- resid.cormat
  output_list[[3]] <- p_list

  names(output_list) <- c("details", "Std.Resid.Mat", "Plot.List")
  class(output_list) <- "psketti"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  return(invisible(output_list)) # invisible prevents plots from printing
  print.psketti(x = output_list)

}




