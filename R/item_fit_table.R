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
#' @importFrom stats pchisq na.exclude
#' @importFrom eRm person.parameter
#'
#' @export

item_fit_table <- function(eRm.obj){
  # warning messages for function
  if(!"eRm" %in% class(eRm.obj))
    stop("Object is not of class eRm: Please ensure your input object is an eRm object with model RM.")
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
  
  
  # functions from eRm
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Checks:
  obj.model <- eRm.obj$model                  # check ordering of this section
  ppar      <- eRm::person.parameter(eRm.obj) # person params
  ipar      <- eRm::itemfit(ppar)             # item params
  
  if (obj.model == "RM"){
    # item parameter table
    itm.par <- data.frame(Item = names(eRm.obj$betapar),
                          Beta = eRm.obj$betapar *-1,
                          Se = eRm.obj$se.beta)
    # remove extra text from item label
    itm.par$Item <- gsub(pattern = "beta ", replacement = "", x = itm.par$Item)
    
   
    XXX <- ppar$X[-ppar$pers.ex,]
   
    # degrees of freedom
    df <- apply(XXX, 2, function(x){length(na.exclude(x))-1})
    
    # item fit table: include stats from eRm::itemfit()
    ifit.tbl <- data.frame(Item = names(ipar$i.fit),
                           Chisq     =  ipar$i.fit,
                           df        =  df,   
                           OutfitMSQ =  ipar$i.outfitMSQ,
                           InfitMSQ  =  ipar$i.infitMSQ,
                           OutFitt   =  ipar$i.outfitZ,
                           InFitt    =  ipar$i.infitZ,
                           Disc      =  ipar$i.disc) 
    # Calculate chisq p value
    ifit.tbl$pvalue <- pchisq(ifit.tbl$Chisq,
                              df = ifit.tbl$df,
                              lower.tail = FALSE)
    
    ifit.tbl <- merge(ifit.tbl, itm.par)
    ifit.tbl <- ifit.tbl[ , c("Item", "Beta", "Se", 
                              "Chisq", "df", "OutfitMSQ",
                              "InfitMSQ", "OutFitt", "InFitt", "Disc")]
    
  } else if(obj.model == "PCM"){
    # get the threshold names
    category_names  <- names(eRm.obj$betapar)
    
    itm.par <- data.frame(Item = sub("\\..*", "",
                                     x = sub(".+? ", "", category_names)),
                          K = sub('.*\\.', '',category_names),
                          tau = eRm.obj$betapar *-1,
                          Se = eRm.obj$se.beta)
    
    # remove extra text from item label
    rownames(itm.par) <- NULL
    
    # Calculate DF
    XXX <- ppar$X[-ppar$pers.ex,] # for value than can be calculated
    df <- apply(XXX, 2, function(x){length(na.exclude(x))-1})
    
    # item fit table: include stats from eRm::itemfit()
    ifit.tbl <- data.frame(Item = names(ipar$i.fit),
                           Chisq     =  ipar$i.fit,
                           df        =  df,   
                           OutfitMSQ =  ipar$i.outfitMSQ,
                           InfitMSQ  =  ipar$i.infitMSQ,
                           OutFitt   =  ipar$i.outfitZ,
                           InFitt    =  ipar$i.infitZ,
                           Disc      =  ipar$i.disc) 
    # Calculate chisq p value
    ifit.tbl$pvalue <- pchisq(ifit.tbl$Chisq,
                              df = ifit.tbl$df,
                              lower.tail = FALSE)
    
    ifit.tbl <- merge(ifit.tbl, itm.par)
    ifit.tbl <- ifit.tbl[ , c("Item", "K", "tau", "Se", 
                              "Chisq", "df", "OutfitMSQ",
                              "InfitMSQ", "OutFitt", "InFitt", "Disc")]
  }
  
  ifit.tbl <- as.data.frame(ifit.tbl)
  
  class(ifit.tbl) <- c("data.frame", "ItemFit")
 
  return(ifit.tbl)
}
