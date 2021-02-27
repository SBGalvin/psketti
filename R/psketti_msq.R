

psketti_msq <- function(x, style = "present"){
  
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
      geom_hline(yintercept = 1.2, colour = "tomato", lty = "dotted")+
      geom_hline(yintercept = 0.8, colour = "tomato", lty = "dotted")
    
  } else if(style == "print"){
    plt <- plt + 
      geom_hline(yintercept = 1.2, colour = "grey50", lty = "dotted")+
      geom_hline(yintercept = 0.8, colour = "grey50", lty = "dotted")
    
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
