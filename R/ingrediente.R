#' @title Score Report Tables
#'
#' @name ingrediente
#'
#' @description Generates score report for dataframe.
#'
#' @param x a dataframe.
#' @param ID column name for ID column
#' @param Item column name for Item column
#' @param Score a column name for response scores
#' @param K column name for column containing multiple choice responses
#' @param K_options An ordered factor object to arrange column order in the
#'     score table.
#' @param Index Column name for order of administration per participant. This
#'    can also be an ordered factor for the item names. Orders the Response
#'    string. Defaults to `NULL`, using the items to order the response string.
#'
#' @return Score reports for participants, with counts of category selection and
#'     a score string ordered by score string index
#'
#' @importFrom stats reshape xtabs
#'
#' @export
#'
#' @examples
#' # Example 1
#' # For dichotomous Rasch model
#' library(psketti)
#' data("FakeData")
#' 
#' K_opt <- factor(LETTERS[1:5], levels = LETTERS[1:5], ordered = TRUE)
#' score_report <- ingrediente(x = FakeData,
#'                             Item = "Item",
#'                             ID = "ID",
#'                             Score = "X",
#'                             K = "K",
#'                             K_options = K_opt,
#'                             Index = "Index")
#' 
#' # show score report for values with a total score <= 5
#' score_report[score_report$total_score <= 1, ]
#' # Score report ordering response string by item difficulty
#' data("FakeItems")
#' FI2 <- FakeItems[order(FakeItems$Beta),]
#' row.names(FI2)<- NULL
#' FI_factor <- factor(FI2$Item, levels = FI2$Item, ordered = TRUE)
#' 
#' 
#' 
#' score_report2 <- ingrediente(x = FakeData,
#'                              Item = "Item",
#'                              ID = "ID",
#'                              Score = "X",
#'                              K = "K",
#'                              K_options = K_opt,
#'                              Index = FI_factor)
#' 
#' # show score report for values with a total score <= 5
#' score_report2[score_report2$total_score == 21, ]
#' 
#' \dontrun{
#' # Example 2
#' # For Rasch partial credit model
#' library(dplyr)
#' library(tidyr)
#' data("FakePCMData")
#' data("FakePCMItems")
#' 
#' # Arrange Data, wide to long
#' fpcm <- FakePCMData %>% 
#'   pivot_longer(cols = -ID, values_to = "Response", names_to = "Item") %>% 
#'   mutate(X = Response) %>% 
#'   mutate(K = as.character(Response)) %>% 
#'   mutate(K = recode(K, "0" = "A", "1" = "B", "2" = "C", "3" = "D"))
#' 
#' # factor variable: Index for item order
#' F2            <- FakePCMItems[, c("Item", "Beta")] # extract relevant cols
#' F2            <- F2[order(F2$Beta),]               # order dataframe
#' row.names(F2) <-  NULL                             # drop rownames     
#' 
#' # create factor variable
#' F_factor <- factor(F2$Item,
#'                    levels = F2$Item,
#'                    ordered = TRUE)
#' #apply factor to data frame
#' fpcm$Index <- fpcm$Item                         # Item -> Index
#' fpcm$Index <- factor(fpcm$Index,
#'                      levels = levels(F_factor),
#'                      ordered = TRUE)
#' 
#' fpcm      <- as.data.frame(fpcm) # ensure this is a dataframe!!
#' 
#' # factor variable for K categories
#' K_opt     <- factor(LETTERS[1:4],
#'                     levels = LETTERS[1:4],
#'                     ordered = TRUE)
#' # produce score report
#' score_pcm <- ingrediente(x = fpcm,
#'                          Item = "Item",
#'                          ID = "ID",
#'                          Score = "X",
#'                          K = "K",
#'                          Index = "Index",
#'                          K_options = K_opt)
#' 
#' score_pcm[score_pcm$total_score < 2, ] # print out score report
#' }

ingrediente <- function(x, ID, Item, Score, K, K_options, Index=NULL){

  x <- as.data.frame(x) # to play well with reshape()

  #~~~~~~~~~~~~~~~~~~~~~~ Response String Ordering ~~~~~~~~~~~~~~~~~~~~~~~~~#
  # If index is NULL, then create Index from Item column
  if(is.null(Index)){
    # If index is null, use the Item column for ordering
    x["Index"] <- x[Item]
    Index      <- Item

  }else if(is.ordered(Index) & !"xx" %in% colnames(x)){
    # if the Index argument is an ordered factor and NOT already a column name:

    x[, "Index"]        <- as.factor(x[,"Item"])    # copy the item col to Index
    levels(x[,"Index"]) <- levels(Index)            # level the index col according to index argument
    x[, "Index"]        <- as.numeric(x[, "Index"]) # convert to numeric value
    x[, "Index"]        <- sprintf(fmt = "%03d",    # string pad
                                   x[, "Index"])

    Index               <- "Index"                  # overwrite "Index" to the Index arg
  }


  #~~~~~~~~~~~~~~~~~~~ Response String Ordered by Index Number ~~~~~~~~~~~~~~~~~~~#
  # long to wide format
  x_scores <- reshape(x[, c(ID, Item, Score)],
                      timevar = Item,
                      idvar = ID,
                      direction = "wide")

  #~~~~~~~~~~~~~~~~~~~ Total Score ~~~~~~~~~~~~~~~~~~~#
  x_scores$total_score <- rowSums(x_scores[, -1])

  x2 <- x[order(x[ID], x[Item]),]
  names(x2) <- gsub(pattern = Index, replacement = "T_item", names(x2))

  #~~~~~~~~~~~~~~~~~~~ category counts ~~~~~~~~~~~~~~~~~~~#
  df.1 <- data.frame(xtabs(~ID+K, data = x2)) # cross-tabs table
  df.1 <- reshape(df.1,                       # Reshape df.1 from long to wide
                  direction = "wide",
                  timevar =   K,              # Value column, use input arg
                  v.names =   "Freq",
                  idvar =     ID)             # ID column, use input arg

  names(df.1) <- gsub(pattern = "Freq.", replacement = "", names(df.1)) # clean column name
  df.1 <- df.1[, c(ID, levels(K_options))]                              # data frame order

  #~~~~~~~~~~~~~~~~~~~ Response Table ~~~~~~~~~~~~~~~~~~~#

  x3 <- x2[, c("ID", "T_item", "X", "K")]
  x3 <- reshape(x3,
                direction = "wide",
                timevar = "T_item",
                v.names =  c("X", "K"),
                idvar = "ID")

  # Set up the Score report (binary) and Response option report
  colNames <- colnames(x3)                     # column names
  X_cols   <- grep("^X.", colNames,value=TRUE) # column names containing X.
  K_cols   <- grep("^K.", colNames,value=TRUE) # column names containing K.

  Score_string    <- apply(x3[, X_cols, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = ""))
  Response_string <- apply(x3[, K_cols, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = ""))

  # responses df
  response.x <- rbind(
    data.frame(ID = x3[, 1], String_type = rep("Responses", nrow(x3)), Pattern = Response_string),
    data.frame(ID = x3[, 1], String_type = rep("Score",     nrow(x3)), Pattern = Score_string)
  )
  # order the response table
  response.x <- response.x[order(response.x$ID, response.x$String_type),]

  score_and_responses <- merge(response.x, x_scores[, c(1, ncol(x_scores))])
  score_and_responses <- score_and_responses[, c("ID", "total_score", "String_type", "Pattern")]

  return(score_and_responses)
}
