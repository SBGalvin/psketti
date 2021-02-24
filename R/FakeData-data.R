#' Fake Data
#'
#' Simulated scored data for 23 items and 1200 participants, where items where presented in random order to each participant.
#' Contains responses for dichotomous (X), and polychotomous (K) data. Item delivery order is stored in the Index column
#' Data were simulated according to suggestions in Linacre (2007).
#' Data are stored in long format.
#'
#' @docType data
#'
#' @usage data(FakeData)
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
#' # Show fake item parameters
#' data(FakeItems)
#'
#' # Show fake data
#' data(FakeData)
#'
#' library(tidyverse)
#' # Count of item responses by item and polychotomous category
#' count_by_reponse_K <- FakeData %>% group_by(Item, K) %>%  summarise(Count = n())
"FakeData"
