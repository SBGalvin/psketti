#' @title pskettify your data
#'
#' @name pskettify
#'
#' @description This function extracts data from an eRm object of class 'RM' and
#'     converts to a format for plotting data. This also computes the empirical
#'     response values and empirical confidence intervals.
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
#' @importFrom stats qbeta coef reshape
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

pskettify <- function(eRm.obj, conf.level = .95, Theta.lwr = -6, Theta.upr = 6){

 
  # warning messages for function
  if(!"eRm" %in% class(eRm.obj))
    stop("Object is not of class eRm: Please ensure your input object is an eRm object with model RM.")
  # Change (1) to (2) when PCM functionality is built in:
  # (1):
  if(!eRm.obj$model %in% c("RM", "PCM")){
    stop(
      paste0(
        "Error: Model not of type RM or PCM.",
        "\n",
        "Please ensure eRm object is either a",
        "\n", "\t",
        "estimated Dichotmous Rasch Model",
        "\n", "\t",
        "using eRm::RM()",
        "\n", "\t", "\t",
        "or",
        "\n", "\t",
        "estimated Partial Credit Rasch Model",
        "\n", "\t",
        "using eRm::PCM()"
      )
    )
  }
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Checks:
  obj.model       <- eRm.obj$model  # check ordering of this section
  ppar            <- eRm::person.parameter(eRm.obj) # USED IN DATA 3
  theta_score     <- ppar$pred.list[[1]]$y          # USED IN DATA 3
  
  if( obj.model == "RM"){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # DICHOTOMOUS RASCH MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~#
    X               <- data.frame(eRm.obj$X)
    difficulty      <- eRm.obj$betapar *-1   # this maybe a bit extra
    item.names      <- colnames(X)
    itm.df          <- data.frame(Item = names(eRm.obj$betapar),
                                  Beta = eRm.obj$betapar *-1,
                                  Se = eRm.obj$se.beta)
    
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
    
    # remove beta label from item names
    itm.df$Item    <- gsub(itm.df$Item,
                           pattern = "beta ",
                           replacement = "")
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # Data 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
      # ci calcultation
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
    
    # END DICHOTOMOUS RASCH MODEL ~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
  } else if (obj.model == "PCM"){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # PARTIAL CREDIT RASCH MODEL ~~~~~~~~~~~~~~~~~~~~~~#
    theta           <- seq(Theta.lwr, Theta.upr, .1)
    X               <- data.frame(eRm.obj$X)
    difficulty      <- eRm.obj$betapar *-1   # this maybe a bit extra
    item.names      <- colnames(X)
    category_names  <- names(eRm.obj$betapar)
    itm.df          <- data.frame(Item = sub("\\..*", "",
                                             x = sub(".+? ", "", category_names)),
                                  K = sub('.*\\.', '',category_names),
                                  tau = difficulty,
                                  Se = eRm.obj$se.beta)
    I_List <- unique(itm.df$Item)
    hh_out <- data.frame()
    for (ij in 1:length(I_List)) {
      
      h_item <- I_List[ij]
      h_dif  <- mean(itm.df[itm.df$Item == I_List[ij], ]$tau)
      hh <- data.frame(Item = h_item, Beta = h_dif)
      rep(hh, )
      hh_out <- rbind(hh_out, hh)
    }
    
    itm.df <-  merge(itm.df, hh_out)
    
    
    # Model probabilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    plist   <- plist.internal(object = eRm.obj, theta = theta) # eRm function
    
    presp <- data.frame()
    el <- 1
    for (el in 1: length(plist)) {
      m <- plist[el]
      m <- as.data.frame(m)
      c_names <-  paste0("c", 0:(ncol(m)-1))
      colnames(m) <- c_names
      
      m$Theta <- theta
      m$Item <- item.names[el]
      m <- m[, c("Theta", "Item", c_names)] # select column order
      # wide to long
      m <- reshape(m,
                   direction = "long", 
                   varying = c_names, 
                   timevar = "K",
                   v.names = "Probs",
                   times = c_names)
      rownames(m) <- NULL              # remove row names
      
      presp <- rbind(presp, m)
    }
    # clean up presp
    presp$id <- NULL
    presp    <- presp[, c("Item", "Theta", "K", "Probs")]
    #presp    <- merge(itm.df, presp) c0 is not estimate, this creates issues
    
    # Empirical ICC
    # Note, some K will have null values so CI 
    # for multinomial beta can't be estimated
    # Instead compute Empirical ICC with no standard error
    # Work on implementing the function, print message with output
    ppar          <- eRm::person.parameter(eRm.obj)
    X2 <- X
    X2$ID <- rownames(X2)
    X2 <- X2[, c("ID", colnames(X))]
    
    Theta   <- ppar$theta.table
    Theta$ID <- rownames(Theta)
    Theta <- Theta[, c("ID", "Person Parameter")]
    colnames(Theta) <- c("ID", "Theta")
    
    
    
    X3 <- merge(Theta, X2)
    
    X4 <- reshape(X3,
                  direction = "long",
                  varying = colnames(X),
                  timevar = "Item",
                  v.names = "K",
                  times = colnames(X))
    
    X4$K <- paste0("c", X4$K)         # paste label for consistency
    
    X4$K <- as.factor(X4$K)          # factor for counts
    X4$Theta <- as.factor(X4$Theta)  # factor for counts
    
    Th_Sum <- data.frame(Theta = names(summary(factor(Theta$Theta))),
                         Count = summary(factor(Theta$Theta)))
    # Frequencies table
    df.1 <- data.frame(xtabs(~Item+Theta+K, data = X4))
    df.2 <- merge(df.1, Th_Sum)
    df.2$Prop <- df.2$Freq/df.2$Count
    
    df.2$Theta <- as.numeric(as.character(df.2$Theta)) # return to number
    emp_ICC_df <- df.2
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  }
  
  # Final clean
  rownames(itm.df) <- NULL
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output_list <- list()
  output_list[[1]] <- itm.df
  output_list[[2]] <- presp
  output_list[[3]] <- emp_ICC_df
  names(output_list) <- c("ItemDF","presp", "emp_ICC")
  
  # apply class name
  class(output_list) <- c("pskettified", obj.model)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  return(invisible(output_list))
  print.pskettified(x = output_list)
}
