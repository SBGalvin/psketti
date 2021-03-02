#' @title Plot a Infit and Outfit MSQ
#'
#' @name pskett_msq
#'
#' @description This function plots the Infit and Outfit MSQ values from a item
#'     of class ItemFit. see `item_fit_table()`.
#'
#' @param x input data, generated using `item_fit_table()`.
#' @param style a character string for plotting style options are present for
#'     coloured, or print for black and white. Defaults to "present".
#'     
#' @return MSQ plot.
#'
#' @importFrom ggplot2 ggplot aes geom_text geom_point geom_hline scale_colour_manual theme_minimal theme ylab xlab scale_x_continuous
#'
#' @export
#' 
#' @examples 
#' library(eRm)
#' library(psketti)
#' 
#' data("FakeData") # load data
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
#' fake_rm     <- RM(Fake_Data_scores)     # Estimate Rasch model
#' 
#' itemFit_psk <- item_fit_table(fake_rm)  # item fit stats
#' MSQplot     <- psketti_msq(itemFit_psk) # Plot infit and outfit
#' 
#' MSQplot

psketti_msq <- function(x, style = "present"){
  # initialize
  Fit <-Item <-MSQ <-Xpos <- NULL
  
  # Safety check
  if(!"ItemFit" %in% class(x)) 
    stop("Object is not of class ItemFit!!")
  
  # wide to long
  x2 <- reshape(x[, c("Item", "InfitMSQ", "OutfitMSQ")], 
                varying = c("InfitMSQ", "OutfitMSQ"), 
                v.names = "MSQ",
                timevar = "Fit", 
                times = c("InfitMSQ", "OutfitMSQ"), 
                direction = "long")
  
  # clean up df
  x2$Fit  <- gsub(pattern = "MSQ", replacement = "", x = x2$Fit)
  x2$Xpos <- ifelse(x2$Fit == "Outfit", -1, 1)
  rownames(x2) <- NULL
  
  # start plot
  plt <- ggplot(data = x2, aes(x = Xpos, y = MSQ))
  
  if(style == "present") {
    plt <- plt + 
      geom_hline(yintercept = 1.2, colour = "tomato4", lty = "dashed")+
      geom_hline(yintercept = 0.8, colour = "tomato4", lty = "dashed")
    
  } else if(style == "print"){
    plt <- plt + 
      geom_hline(yintercept = 1.2, colour = "grey50", lty = "dashed")+
      geom_hline(yintercept = 0.8, colour = "grey50", lty = "dashed")
    
  }
  
  
  plt <- plt +
    geom_point(aes(colour = Fit))+
    geom_text(aes(label = Item, hjust = ifelse(Xpos == 1, -1, 1.5)))+
    scale_x_continuous(breaks = c(-1, 1), limits = c(-2, 2), 
                       labels = c("Outfit", "Infit"))+
    xlab("")+
    ylab("MSQ")+
    theme_minimal()+
    theme(legend.position = "none")
  
  if(style == "present") {
    plt <- plt + scale_colour_manual(values = c("orange", "steelblue"))
    
  } else if(style == "print"){
    plt <- plt + scale_colour_manual(values = c("black", "grey50"))
    
  }
  
  return(plt)
  
}
