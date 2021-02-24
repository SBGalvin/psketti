#' @title Plot a single ICC
#'
#' @name psketto
#'
#' @description psketto is singlular of psketti; (spaghetti <--> spaghetto).
#'     This function plots the data for a single item from the output of pskettify()
#'     psketto() is also used in psketti to create multiple ICC plots
#'
#' @usage \code{psketto(pskettified_data = df, item = "ItemName", item.label = "ItemLabel")}
#'
#' @param pskettified_data input data, generated using \code{pskettify()}
#' @param item character name of the item to be plotted
#' @param style a character string for plotting style options are present for coloured plots,
#'     or print for black and white plots. Defaults to "present".
#' @param item.label a character string of the item name prefix.
#' @param IRFLocation logical, plots reference lines for Rasch IRF location. Defaults to TRUE.
#' @param empCI logical, plots confidence intervals for empirical points, calculated using \code{pskettify()}.
#'     Defaults to TRUE.
#' @param empICC logical, plots empirical ICC for item. Defaults to \code{TRUE}.
#' @param empPoints logical, plots empirical points for based on class intervals/score groups
#'     generated with \code{pskettify()}. Defaults to \code{TRUE}
#'
#' @return psketto plot.
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_line scale_colour_manual theme_minimal theme ylab xlab ggtitle element_blank
#'
#' @export
#'
#' @examples
#' library(eRm)
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
#' # plot IRF in default colours
#' psk_1_present <- psketto(psk.data, style = "present", item = 1, item.label = "Item 1")
#' psk_1_present
#'
#' # plot IRF in default greyscale colours
#' psk_1_print   <- psketto(psk.data, style = "print", item = 1, item.label = "Item 1")
#' psk_1_print

psketto <- function(pskettified_data, item, item.label, style = "present",
                    IRFLocation = TRUE, empCI = TRUE, empICC = TRUE, empPoints = TRUE){
  # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  x <- pskettified_data
  # warning messages for function
  if(!"pskettified" %in% class(x)) stop("Object is not of class pskettified: Please ensure your input object is pskettified with psketti::pskettify()")

  # Setup colours and labels for plot!!!
  # set stuff for reference lines first
  p_types <- c("Reference")
  psk_colours_1 <- "grey"
  psk_colours_2 <- "grey"
  p_labels <- c("Rasch IRF")


  if(empICC == TRUE){
    p_types <- c(p_types, "EmpICC")
    psk_colours_1 <- c(psk_colours_1, "steelblue")
    psk_colours_2 <- c(psk_colours_2, "#020202")
    p_labels <- c(p_labels, "Empirical ICC")
  }else{
    p_types <- p_types
    psk_colours_1 <- psk_colours_1
    psk_colours_2 <- psk_colours_2
    p_labels <- p_labels
  }

  if(empCI == TRUE){
    p_types <- c(p_types, "CI")
    psk_colours_1 <- c(psk_colours_1, "tomato")
    psk_colours_2 <- c(psk_colours_2, "#545454")
    p_labels <- c(p_labels, "95% ci")
  }else{
    p_types <- p_types
    psk_colours_1
    psk_colours_2
    p_labels <- p_labels
  }


  if(empPoints == TRUE){
    p_types <- c(p_types, "EmpPoints")
    psk_colours_1 <- c(psk_colours_1, "tomato4")
    psk_colours_2 <- c(psk_colours_2, "#303030")
    p_labels <- c(p_labels, "Class Interval Proportions")
  }else{
    p_types <- p_types
    psk_colours_1
    psk_colours_2
    p_labels <- p_labels
  }

  # labels and colours for plots
  p_types <- factor(p_types, ordered = T, levels = p_types)
  names(psk_colours_1) <- levels(p_types)
  names(psk_colours_2) <- levels(p_types)
  names(p_labels) <- levels(p_types)
  # order vectors
  psk_colours_1 <- psk_colours_1[order(match(psk_colours_1,levels(p_types)))]
  psk_colours_2 <- psk_colours_2[order(match(psk_colours_2,levels(p_types)))]
  p_labels      <- p_labels[order(match(p_labels, levels(p_types)))]


  # Data subset by item
  tmp1 <- x$emp_ICC[x$emp_ICC$Item == item,]
  tmp2 <- x$presp[x$presp$Item == item,]

  # Item Difficulty/ Location
  Beta_tmp <- round(unique(tmp2$Beta),2)

  # Item label
  if (nchar(item.label) == 0){
    I_label <- ifelse(
      grepl(x = item, "Item") | grepl(x = item, "item"),
      item,
      paste0("Item ", item)
    )

  } else {
    I_label <- item.label
  }

  # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  # base plot: Rasch ICC and Location reference lines
  plt <- ggplot(data = tmp2, aes(x = Theta, y = Probs, group = Item))

  if(IRFLocation == TRUE){
    plt <- plt +
      # Reference lines for P = .5 and item location (Beta)
      ## Pr =0.5 reference line
      geom_segment(data = tmp2,
                   aes(y = .5, yend = .5, x = min(Theta), xend = Beta, group = Item, colour = "Reference"),
                   linetype = "solid")+
      ## Item Difficulty/ Location reference line
      geom_segment(data = tmp2,
                   aes(y = 0, yend = .5, x = Beta, xend = Beta, group = Item, colour = "Reference"),
                   linetype = "solid")+

      # Theoretical Rasch ICC
      geom_line(aes(colour = "Reference"))

  } else{
   plt <-  plt +
      # Theoretical Rasch ICC
      geom_line(aes(x = Theta, y = Probs, group = Item, colour = "Reference"))
  }




  # add empirical ICC
  if ( empICC == TRUE){
    plt <- plt + ## Empirical ICC for data
      geom_line(data = tmp1, aes(x = Theta, y = rel.freq, colour = "EmpICC"), linetype = 'dashed')

  } else{
    plt <- plt


  }

  # add empirical points
  if (empPoints == TRUE){
    plt <- plt +
      # Theta and naive probs averages per score group
      geom_point(data = tmp1, aes(x = Theta, y  = rel.freq, group = Item, colour = 'EmpPoints'))
  } else{
    plt <- plt

  }

  # add empirical CI
  if (empCI == TRUE){
    plt <- plt+
      ## Lower Ci for score point
      geom_segment(data = tmp1,
                   aes(y = rel.freq, yend = lci, x = Theta, xend = Theta, group = Item, colour = "CI"),
                   linetype = "dotted")+
      ## Upper Ci for score point
      geom_segment(data = tmp1,
                   aes(y = rel.freq, yend = uci, x = Theta, xend = Theta, group = Item, colour = "CI"),
                   linetype = "dotted")
  } else{
    plt <- plt

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # add theme and colour depending on style

  if (style == "present"){

    plt <- plt +

      # Manual legend
      scale_color_manual(name = bquote(.(I_label)~"Location "~theta~"="~.(Beta_tmp)~": "),
                         values = psk_colours_1,
                         guide = "legend",
                         labels = p_labels)+
      # Theme settings
      theme_minimal() +
      theme(legend.position = "bottom", # legend bottom-right
            legend.direction = "horizontal",
            legend.background = element_blank())+

      xlab(expression("Latent Dimension" ~theta))+
      ylab(expression(Pr(X[ij]~"="~1~"|"~theta)))


  }else if(style == "print"){


    plt <- plt +

      # Manual legend
      scale_colour_manual(name = bquote(.(I_label)~"Location "~theta~"="~.(Beta_tmp)~": "),
                          values = psk_colours_2,
                          guide = "legend",
                          labels = p_labels)+
      # Theme settings
      theme_minimal() +
      theme(legend.position = "bottom", # legend bottom-right
            legend.direction = "horizontal",
            legend.background = element_blank())+

      xlab(expression("Latent Dimension" ~theta))+
      ylab(expression(Pr(X[ij]~"="~1~"|"~theta)))

  }

  return(plt)

}
