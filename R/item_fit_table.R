#' @title Create Item Fit Table
#'
#' @name item_fit_table
#'
#' @description This function extracts Item fit statistics from eRm `itemfit()`
#'
#' @param eRm.obj input data, generated using a Rasch model estimation function
#'     from eRm.
#'     
#' @return A data.frame of class ItemFit.
#' 
#' @importFrom stats pchisq
#'
#' @export

item_fit_table <- function(eRm.obj){
  
  # functions from eRm
  pparj <- eRm::person.parameter(eRm.obj) # person params
  iparj <- eRm::itemfit(pparj) 
  
  itm.parj <- data.frame(Item = names(eRm.obj$betapar),
                         Beta = eRm.obj$betapar *-1,
                         Se = eRm.obj$se.beta)
  # remove extra text
  itm.parj$Item <- gsub(pattern = "beta ", replacement = "", x = itm.parj$Item)
  
  # item fit table: include stats from eRm::itemfit()
  ifit.tbl <- data.frame(Item = names(iparj$i.fit),
                         Chisq     =  iparj$i.fit,
                         df        =  iparj$i.df -1,   # follow up with this
                         OutfitMSQ =  iparj$i.outfitMSQ,
                         InfitMSQ  =  iparj$i.infitMSQ,
                         OutFitt   =  iparj$i.outfitZ,
                         InFitt    =  iparj$i.infitZ,
                         Disc      =  iparj$i.disc) 
     
    
    # Calculate chisq p value
  ifit.tbl$pvalue <- pchisq(ifit.tbl$Chisq,
                            df = ifit.tbl$df,
                            lower.tail = FALSE)
  
  ifit.tbl <- merge(ifit.tbl, itm.parj)
  ifit.tbl <- ifit.tbl[ , c("Item", "Beta", "Se", 
                            "Chisq", "df", "OutfitMSQ",
                            "InfitMSQ", "OutFitt", "InFitt", "Disc")]
  
  # give this a class name
  class(ifit.tbl) <- "ItemFit"
 
  return(ifit.tbl)
}
