#' @title Distractor Analysis Plots
#'
#' @name psketti_distractor
#'
#' @description Implementation of a graphical (Asril and Marais, 2011) approach
#'     to assigning a partial credit scoring system to data previously estimated
#'     with a dichotomous Rasch model. The function console output prints object
#'     details, a list of items, and generic example of how to call the plot.
#'
#' @param x A long formatted dataframe
#' @param ID column name for ID column
#' @param Item column name for Item column
#' @param K column name for column containing multiple choice responses.
#' @param response_options An ordered factor object to arrange column order in
#'     the distractor table.
#' @param eRm.obj An object of class eRm and model RM. Use `eRm::RM(score_data)`
#'     to create this object. To plot empirical values for PCM see `pskettify`, 
#'     `psketti` and `psketto`.
#' @param p.style Plot output style, "print" for black and white, or "present"
#'     for color. Defaults to "present".
#' @param distractor_colours An optional vector of colours for distractor plot
#'     lines. Must be the same length as response_options. Defaults to `NULL`
#'     for viridis color palette.
#'
#' @return Plot.List is a list object containing plots of empirical distractor
#'     proportions plotted against the dichotomous Rasch IRF.
#'
#' @importFrom stats reshape xtabs setNames
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_line scale_colour_manual theme_bw theme ylab xlab
#' @importFrom grDevices gray
#'
#' @export
#' 
#' @examples
#' library(eRm)
#' library(psketti)
#' data("FakeData")
#' Fake_Data_scores <- reshape(FakeData[, c("ID", "Item", "X")],
#'                             timevar = "Item",
#'                             idvar = "ID",
#'                             direction = "wide")
#'
#' # set column names to be equal to original item names
#' names(Fake_Data_scores) <- c("ID",                                
#'                              paste0("i",
#'                                     sprintf(fmt  = "%02d", 1:23)))
#'
#' row.names(Fake_Data_scores) <- Fake_Data_scores$ID  # set ID as row names
#' Fake_Data_scores$ID         <- NULL                 # drop the ID column
#' fake_rm                     <- RM(Fake_Data_scores) # fit a Rasch Model
#'
#' # Prepare response options factor
#' r_o <- factor(sort(unique(FakeData$K)),          # input var
#'               levels = sort(unique(FakeData$K)), # factor levels
#'               ordered = TRUE)                    # ordered
#'
#' # multiple plots
#' spag_plot <- psketti_distractor(ID = "ID",              # set ID column
#'                                 Item = "Item",          # set Item column
#'                                 K= "K",                 # Set resp categories 
#'                                 x = FakeData,           # select data
#'                                 eRm.obj = fake_rm,      # select eRm object
#'                                 response_options = r_o, # set resp options
#'                                 p.style = "present")    # set plotting style
#' 
#' spag_plot                         # plot call instructions
#' spag_plot$Plot.List[['i01']][[1]] # plot item 1



psketti_distractor <- function(x, ID, Item, K, response_options,
                               eRm.obj, p.style = "present", 
                               distractor_colours = NULL){
  
  if (!eRm.obj$model == "RM") stop("eRm object not a dichotomous Rasch Model.")
  # set internal variables to NULL
  RO_len <- Theta <- K.prop <- NULL
  
  psketto_simple <-  function(pskettified_data, item = item){
    
    Probs <- NULL
    # an unadorned Rasch ICC
    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    z <- pskettified_data
    
    # Data subset by item
    tmp1 <- z$emp_ICC[z$emp_ICC$Item == item,]
    tmp2 <- z$presp[z$presp$Item == item,]
    tmp3 <- z$ItemDF[z$ItemDF$Item == item,]
    # Item Difficulty/ Location
    Beta_tmp <- round(unique(tmp3$Beta),2)
    
    # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    plt <- ggplot(data = tmp2, aes(x = Theta, y = Probs, group = Item))+
      # Theoretical Rasch ICC
      geom_line(colour = "black", lty = "solid")
    
    return(plt)
    
  }
  
  #x <- pskettified_data
  Model_type <- class(eRm.obj)[1]
  
  
  
  # Stage 1 Extract person abilities -----------------------------------------
  ppar <- eRm::person.parameter(eRm.obj)
  Ability <- data.frame(
    ID = rownames(ppar$theta.table),
    Theta = ppar$theta.table[,"Person Parameter"]
  )
  df.0 <- merge(Ability, x[, c(ID, Item, K)])
  
  
  # Plot
  # pskettified data with defaults
  psk_d <- pskettify(eRm.obj)
  
  # prepare counts for plotting using the n cuts
  k_counts <- data.frame(xtabs(~Item+Theta+K, data = df.0))  # Category counts
  
  T_counts <- data.frame(xtabs(~Item+Theta, data = df.0))   # Counts by Ability
  names(T_counts)[names(T_counts) == 'Freq']<- "T.Freq"     # Change column name
  
  df.kt <- merge(k_counts, T_counts)                      # merge the two tables
  df.kt["K.prop"] <- df.kt$Freq/df.kt$T.Freq              # create prop column
  df.kt$Theta <-  as.numeric(as.character(df.kt$Theta))   # convert to numeric 
  
  
  
  # for loop for plots and append to
  j.list <- unique(psk_d$presp$Item)
  
  distractorPlots <- list()
  j <- NULL
  
  #p.style <- "present"
  #distractor_colours <- NULL
  # Big selector for plot + type
  if(p.style == "present"){
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # checks for distractor colours
    RO_len <- length(response_options)  # this was mistyped and throwing errors
    
    if( !missing(distractor_colours) ){
      DC_len <- length(distractor_colours)
      
      # internal ifelse
      if( !RO_len == DC_len ){
        
        warning("distractor_colours is not the same length as response options!!!",
                "\nUsing default settings for distractor_colours.")
        
        dst_colours        <- viridis(RO_len)
        dst_colours[RO_len] <- "#F7B900" # ensure the last colour is darker than standard viridis yellow
        
      } else{
        dst_colours <- distractor_colours
      }
      
    } else if( missing(distractor_colours) ){
      dst_colours        <- viridis(RO_len)
      dst_colours[RO_len] <- "#F7B900" # ensure the last colour is darker than standard viridis yellow
      }
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (j in 1:length(j.list)) {
      x_j <- j.list[j]
      Beta_tmp <- round(unique(psk_d$ItemDF[psk_d$ItemDF$Item == j.list[j], ]$Beta),2)
      
      d_icc_plt <-  psketto_simple(pskettified_data = psk_d,
                                   item = j.list[j])+
        
        geom_line(data = df.kt[df.kt$Item == j.list[j], ],
                  aes(x = Theta, y = K.prop , colour = K, group = K), lty = 'dashed')+
        geom_point(data = df.kt[df.kt$Item == j.list[j], ],
                   aes(x = Theta, y = K.prop, colour = K), 
                   shape = 21,                              # NEW TEST!!!
                   fill = 'white')+
        scale_colour_manual(values = dst_colours,
                            name = bquote("Distractors"~.(x_j)~"Location "~theta~"="~.(Beta_tmp)~": "))+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.background = element_blank())+
        
        xlab(expression("Latent Dimension" ~theta))+
        ylab(expression(Pr(X[ij]~"="~1~"|"~theta)))
      
      distractorPlots[[j]] <-  list(d_icc_plt, x_j)
    }
    
    distractorPlots <- setNames(distractorPlots, j.list)
  }else if(p.style == "print"){
    distractor_grey <- gray(seq(0, .75, length.out = length(response_options))) # New
    # This colour is too light for lines and points
   
    for (j in 1:length(j.list)) {
     
      x_j <- j.list[j]
      Beta_tmp <- round(unique(psk_d$ItemDF[psk_d$ItemDF$Item == j.list[j], ]$Beta),2)
      
      d_icc_plt <-  psketto_simple(pskettified_data = psk_d,
                                   item = j.list[j])+
        geom_line(data = df.kt[df.kt$Item == j.list[j], ],
                  aes(x = Theta, y = K.prop , colour = K, group = K), lty = 'dashed')+
        geom_point(data = df.kt[df.kt$Item == j.list[j], ],
                   aes(x = Theta, y = K.prop, colour = K), 
                   shape = 21,                               # NEW TEST!!!
                   fill = 'white')+
        scale_colour_manual(values = distractor_grey,
                            name = bquote("Distractors"~.(x_j)~"Location "~theta~"="~.(Beta_tmp)~": "))+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.background = element_blank())+
        
        xlab(expression("Latent Dimension" ~theta))+
        ylab(expression(Pr(X[ij]~"="~1~"|"~theta)))
      
      distractorPlots[[j]] <-  list(d_icc_plt, x_j)
    }
    
    distractorPlots <- setNames(distractorPlots, j.list)
    
  }
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output_list <- list()
  output_list[[1]] <- paste0("Distractor plots for Rasch IRF")
  output_list[[2]] <- Model_type
  output_list[[3]] <- distractorPlots
  
  names(output_list) <- c("xlbl", "Model.Type", "Plot.List")
  class(output_list) <- "psketti"
  
  return(invisible(output_list))
  print.psketti(x = output_list) # load from helper
}
