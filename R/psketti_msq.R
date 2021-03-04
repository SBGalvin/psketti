#' @title Plot a Infit and Outfit MSQ
#'
#' @name pskett_msq
#'
#' @description This function plots the Infit and Outfit MSQ values from a item
#'     of class ItemFit. see `item_fit_table()`. Based on Yu (2020).
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
  Xpos2 <- n <- Fit <-Item <-MSQ <-Xpos <- NULL
  
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
  
  # prepare text labels for jittering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n <- 2
  x3    <- x2[x2$Fit == "Infit",]
  x3    <- x3[order(x3$MSQ), ]
  xp3_p <- rep(1.5, nrow(x3))
  xp3_p <- jitter(xp3_p, 15)
  
  xp3_p[seq( n, length(xp3_p), n )] <- xp3_p[seq( n, length(xp3_p), n )]-1
  x3$Xpos2 <- xp3_p
  
  x4    <- x2[x2$Fit == "Outfit",]
  x4    <- x4[order(x4$MSQ), ]
  xp4_p <- rep( -1.5, nrow(x4) )
  xp4_p <- jitter(xp4_p, 15)
  
  xp4_p[seq( n, length(xp4_p), n )] <- xp4_p[seq( n, length(xp4_p), n )]+1
  x4$Xpos2 <- xp4_p
  
  x5 <- rbind( x3, x4 )
  
  
  # start plot
  plt <- ggplot(data = x5)
  
  if(style == "present") {
    plt <- plt + 
      geom_hline(yintercept = 1.2, colour = "#AB1128", lty = "dashed")+
      geom_hline(yintercept = 0.8, colour = "#AB1128", lty = "dashed")
    
  } else if(style == "print"){
    plt <- plt + 
      geom_hline(yintercept = 1.2, colour = "grey50", lty = "dashed")+
      geom_hline(yintercept = 0.8, colour = "grey50", lty = "dashed")
    
  }
  
  
  plt <- plt +
    geom_segment(aes(x = Xpos, xend = Xpos2, y = MSQ, yend = MSQ), colour = "grey")+
    geom_text(aes(label = Item, x = Xpos2, y = MSQ), colour = 'black')+
    geom_point(aes(colour = Fit, x = Xpos, y = MSQ))+
    scale_x_continuous(breaks = c(-1, 1), limits = c(-2, 2), 
                       labels = c("Outfit", "Infit"))+
    xlab("")+
    ylab("MSQ")+
    theme_minimal()+
    theme(legend.position = "none")
  
  if(style == "present") {
    plt <- plt + scale_colour_manual(values = c("#F7B900", "#440154"))
    
  } else if(style == "print"){
    plt <- plt + scale_colour_manual(values = c("black", "grey50"))
    
  }
  
  return(plt)
  
}
