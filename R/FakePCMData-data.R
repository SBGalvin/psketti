#' Fake Data
#'
#' Simulated scored data for 10 items and 1200 participants, where items where presented in random order to each participant.
#' Contains responses for dichotomous (X), and polychotomous (K) data. Item delivery order is stored in the Index column
#' Data were simulated according to suggestions in Linacre (2007).
#' Data are stored in long format.
#'
#' @docType data
#'
#' @usage data("FakePCMData")
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Linacre. (2013)  Rasch Measurement Transactions 21:3 p. 1125
#' (\href{https://www.rasch.org/rmt/rmt213a.htm}{Rasch Measurement Transactions})
#'
#' @source \href{https://www.rasch.org/rmt/rmt213a.htm}{Rasch Measurement Transactions}
#'
#' @examples
#' # Show fake data
#' data("FakePCMData")
#'
#' library(tidyverse)
#' # Count of item responses by item and polychotomous category
#' count_item_by_reponse_K <- FakePCMData %>% 
#'                              pivot_longer(cols = -ID, 
#'                                           names_to = "Item",
#'                                           values_to = "Response") %>% 
#'                              group_by(Item, Response) %>% 
#'                              summarise(Count = n())
"FakePCMData"
