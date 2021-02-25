#' @title Distractor Analysis Tables
#'
#' @name tabliatelle
#'
#' @description Implementation of the tabular (Styles and Andrich, 2009)
#'     approach to assigning a partial credit scoring system to data previously
#'     modeled with a dichotomous Rasch model.
#'
#' @param x A long formatted dataframe.
#' @param ID column name for ID column.
#' @param Item column name for Item column.
#' @param K column name for column containing multiple choice responses.
#' @param response_options An ordered factor object to arrange column order in
#'     the distractor table.
#' @param eRm.obj An object of class eRm and model RM. Use `eRm::RM(score_data)`
#'     to create this object.
#'
#' @return tabliatelle returns a list of class tabliatelle.
#'
#' @importFrom stats reshape xtabs
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_line scale_color_manual theme_bw theme ylab xlab
#'
#' @export
#'
#' @examples
#' library(eRm)
#' library(psketti)
#' data(FakeData)  # load dataset fake data
#'
#' # Set up dataframe for eRm, long to wide with dichotomous data
#' Fake_Data_scores <- reshape(FakeData[, c("ID", "Item", "X")],
#'                             timevar = "Item",
#'                             idvar = "ID",
#'                             direction = "wide")
#'
#' # Match original item names in score dataframe
#' names(Fake_Data_scores)     <- c("ID", paste0("i",sprintf(fmt  = "%02d", 1:23)) )
#' row.names(Fake_Data_scores) <- Fake_Data_scores$ID
#' Fake_Data_scores$ID         <- NULL
#'
#' fake_rm <- RM(Fake_Data_scores) # Fit Rasch Model
#'
#' # Prepare response options factor
#' r_o <- factor(sort(unique(FakeData$K)), levels = sort(unique(FakeData$K)), ordered = TRUE)
#'
#' # tabliatellify
#' tlt_data <- tabliatelle(x = FakeData, eRm.obj = fake_rm,
#'                         ID = "ID", Item = "Item", K = "K",
#'                         response_options = r_o)
#' # clean printout for console
#' tlt_data
#'
#' # access proportions table with un-rounded values
#' tlt_data$Proportions.table

tabliatelle <- function(x, ID, Item, K, response_options, eRm.obj){


  # Checks
  # warning messages for function
  if(!"eRm" %in% class(eRm.obj)) stop("Object is not of class eRm: Please ensure your input object is an eRm object with model RM.")

  if(!eRm.obj$model == "RM") stop("Model not of type RM. Please ensure eRm object is a fitted Dichotmous Rasch Model; use eRm::RM().")


  # Stage 1 Extract person abilities -----------------------------------------
  ppar <- eRm::person.parameter(eRm.obj)
  Ability <- data.frame(
    ID = rownames(ppar$theta.table),
    Theta = ppar$theta.table[,"Person Parameter"]
  )

  Item.Beta <- data.frame(Item = gsub(pattern = "beta ", replacement = "", names(coef(eRm.obj))),
                          Beta = -coef(eRm.obj))

  df.0 <- merge(Ability, x[, c(ID, Item, K)])

  # Stage 2 counts of item/categories response ------------------------------
  df.0$Class_Interval <- as.numeric(cut(df.0$Theta, 3))

  # Mean theta values for Class interval
  C_i_Theta <- by(df.0, list(df.0$Class_Interval), function(x)
  {
    c(
      Class_Interval = unique(x$Class_Interval),
      Theta_mean = mean(x$Theta)
    )
  })

  C_theta <- as.data.frame(do.call(rbind, C_i_Theta))
  C_theta$Class_Interval <- factor(C_theta$Class_Interval, ordered = TRUE)

  # Frequencies table
  df.1 <- data.frame(xtabs(~Item+Class_Interval+K, data = df.0))

  df.1 <- reshape(df.1,
                  direction = "wide",
                  timevar = K,
                  v.names = "Freq",
                  idvar = c(Item, "Class_Interval"))

  # rowsums less the first two columns
  df.1$Freq.Total <- rowSums(df.1[, -c(1, 2)])
  df.1$Class_Interval <- factor(df.1$Class_Interval, ordered = TRUE)

  # table
  df.2       <- merge(df.1, C_theta)
  Count_cols <- paste0("Freq.", as.character(response_options))
  i <- NULL
  for (i in 1:length(Count_cols)) {

    yy <- Count_cols[i]
    yy_name <- gsub(pattern = "Freq.", replacement = "Prop.", yy)
    df.2[yy_name] <- df.2[names(df.2) == yy]/df.2[names(df.2) == "Freq.Total"]
  }

  df.2["Prop.Total"] <- rowSums(df.2[,grepl("Prop.", colnames(df.2))])
  df.3 <- merge(df.2, Item.Beta)



  # output table
  Frequency.table <- df.3[, c("Item", "Beta", "Class_Interval", "Theta_mean",
                              paste0("Freq.", as.character(response_options)), "Freq.Total")]
  Proportions.table <-  df.3[, c("Item", "Beta", "Class_Interval", "Theta_mean",
                                 paste0("Prop.", as.character(response_options)), "Prop.Total")]
  # order by item and class interval levels
  Frequency.table   <- Frequency.table[order(Frequency.table[,1], Frequency.table[,2]), ]
  Proportions.table <- Proportions.table[order(Proportions.table[,1], Proportions.table[,2]), ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Class Size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  c_size <- unique(Frequency.table[,c("Class_Interval", "Freq.Total")])
  Class_N <- as.vector(c_size["Freq.Total"])
  names(Class_N) <- as.character(c_size["Class_Interval"])


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # prepare response option ~~~~~~~~~~~~~~~~~~~~~~~~~#
  r_o_x <- as.character(response_options)
  names(r_o_x) <- c("Key", rep("Distractor", length(response_options)-1))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output_list <- list()
  output_list[[1]] <- "dRM"   # if pcm was used return "PCM": ADD LATER
  output_list[[2]] <- r_o_x
  output_list[[3]] <- Class_N
  output_list[[4]] <- Frequency.table
  output_list[[5]] <- Proportions.table
  names(output_list) <- c("Model", "Response.Options", "Class.Size", "Frequency.table", "Proportions.table")

  # apply class name
  class(output_list) <- c("tabliatelle")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Output print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


  return(invisible(output_list))
  print.tabliatelle(output_list)

}
