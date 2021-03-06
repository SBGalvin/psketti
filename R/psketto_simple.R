#' Unadorned Rasch IRF
#'
#' @param x an object of class pskettified
#' @param item If you want to plot a single IRF, use this argument to state the name of the item.
#' @param all.item Should all item IRF be plotted? Defaults to FALSE
#' @param item.label Should the item labels be plotted? Defaults to FALSE
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
#' fake_rm   <- RM(Fake_Data_scores) # Estimate Rasch model
#'
#' psk_data  <- pskettify(fake_rm)   # pskettify data
#' psketto_simple(psk_data, item = "i01")                       # single item IRF, no label
#' psketto_simple(psk_data, item = "i01", item.label = TRUE)    # single item IRF, labeled
#' psketto_simple(psk_data, item.label = TRUE, all.item = TRUE) # all item IRF labeled

psketto_simple <-  function(x, item, all.item = FALSE, item.label = FALSE){

  Theta<-Beta<- Item <- T_p<- plt <- Probs <- NULL

  # an unadorned Rasch ICC
  # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


  if (class(x)[[2]] == "RM"){
    if (all.item == FALSE & item.label == FALSE){
      # Data subset by item
      tmp1 <-x$emp_ICC[x$emp_ICC$Item == item,]
      tmp2 <-x$presp[x$presp$Item == item,]
      tmp3 <-x$ItemDF[x$ItemDF$Item == item,]
      # Item Difficulty/ Location
      Beta_tmp <- round(unique(tmp3$Beta),2)

      # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      plt <- ggplot(data = tmp2, aes(x = Theta, y = Probs, group = Item))+
        # Theoretical Rasch ICC
        geom_line(colour = "black", lty = "solid")

    } else if (all.item == FALSE & item.label == TRUE) {
      # Data subset by item
      tmp1 <-x$emp_ICC[x$emp_ICC$Item == item,]
      tmp2 <-x$presp[x$presp$Item == item,]
      tmp3 <-x$ItemDF[x$ItemDF$Item == item,]
      # Item Difficulty/ Location
      Beta_tmp <- round(unique(tmp3$Beta),2)

      # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      plt <- ggplot(data = tmp2, aes(x = Theta, y = Probs, group = Item))+
        # Theoretical Rasch ICC
        geom_line(colour = "black", lty = "solid")+
        geom_text(data = tmp3,
                  aes(x = Beta+1, y = .5, label = paste0(Item, " \U03B2: ", round(Beta_tmp, 2))))

    } else if (all.item == TRUE & item.label == TRUE) {
      # Data
      tmp1 <-x$emp_ICC
      tmp2 <-x$presp
      tmp3 <-x$ItemDF

      itm.x <- data.frame(
        Item =  unique(tmp3$Item),
        Beta = round(unique(tmp3$Beta), 2)
      )
      itm.x <- itm.x[order(itm.x$Beta),]
      itm.x$T_p <- seq(min(tmp2$Theta)+1, max(tmp2$Theta)-1, length.out = nrow(itm.x))
      itm.x$Probs <- Pr(theta = itm.x$T_p , b = itm.x$Beta)

      # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      plt <- ggplot(data = tmp2, aes(x = Theta, y = Probs, group = Item))+
        # Theoretical Rasch ICC
        geom_line(aes(colour = Item), lty = "solid", show.legend = FALSE)+
        geom_text(data = itm.x, aes(x = T_p, y = Probs, colour = Item, label = Item), show.legend = FALSE)

    }else if (all.item == TRUE & item.label == FALSE) {
      # Data
      tmp1 <-x$emp_ICC
      tmp2 <-x$presp
      tmp3 <-x$ItemDF


      # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      plt <- ggplot(data = tmp2, aes(x = Theta, y = Probs, group = Item))+
        # Theoretical Rasch ICC
        geom_line(aes(colour = Item), lty = "solid", show.legend = FALSE)

    }


  } else if (!class(x)[[2]] == "RM"){

    stop("Incorrect model pskettified: Please pskettify a dichotomous Rasch Model!")
  } else if(!class(x)[[1]] == "pskettified"){
    stop("Object is not pskettified: please pskettify a dichotomous Rasch Model!")
  }

  plt +
  return(plt)

}


