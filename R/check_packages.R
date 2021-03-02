#' @title Check packages
#' 
#' Checks environment for loaded packages and installed packages.
#'
#' @importFrom utils installed.packages packageVersion
#' 
#' @noRd

check_packages <- function(){
  
  # find which of packages are installed or loaded ~~~~~~~~~~~~~~~~~~~~~
  pkg_names <- c("eRm", "dplyr", "ggplot2", "viridis", "Lumpy")
  env_items   <- search()
  env_items   <- gsub(pattern = "package:", replacement = "", env_items)
  pkg_machine <- data.frame(Package = names(installed.packages()[,3]),
                            Version = unname(installed.packages()[,3]))
  
  pkg <- NULL
  pkg_out <- data.frame()
  
  for (pkg in 1:length(pkg_names)) {
    
    if(pkg_names[pkg] %in% env_items){
      # temporary data frame
      tmp_df <- data.frame(Package = pkg_names[pkg],
                           Version = paste0("(",
                                            packageVersion(pkg_names[pkg]), 
                                            ")"),
                           Loaded = "already loaded")
      
    }else if(!pkg_names[pkg] %in% pkg_machine$Package){
      tmp_df <- data.frame(Package = pkg_names[pkg],
                           Version = "NULL",
                           Loaded = "NOT INSTALLED!")
    }else if(pkg_names[pkg] %in% pkg_machine$Package & !pkg_names[pkg] %in% env_items){
      # library(pkg_names[pkg]) # load package
      tmp_df <- data.frame(Package = pkg_names[pkg],
                           Version = paste0("(",packageVersion(pkg_names[pkg]), ")"),
                           Loaded = "loaded by psketti")
      
    }
    
    pkg_out <- rbind(pkg_out, tmp_df)
  }
  
  # list of not installed dependencies
  pkg_to_install <- pkg_out[pkg_out["Loaded"] == "NOT INSTALLED!",]$Package
  
  # Package laoding messages ------------------------------------------------
  pkg <- NULL
  
  for (pkg in 1:nrow(pkg_out)) {
    # if package is not installed:
    if (pkg_out[pkg, 3] == "NOT INSTALLED!") {
      pkg_out[pkg, 3] <- paste0("\033[0;", 31, "m", pkg_out[pkg, 3], 
                                "\033[0m","\n")
      
    } else if( grepl(x = pkg_out[pkg, 3], pattern = "psketti")) {
      # if package is already loaded:
      pkg_out[pkg, 3] <- gsub(pattern = "psketti", 
                              replacement = paste0("\033[0;", 33, "m",
                                                   "psketti", "\033[0m"),
                              pkg_out[pkg, 3])
      
    } else {
      pkg_out[pkg, 3] <- pkg_out[pkg, 3]
    }
  }
  
  # output ~~~~~~~~~~~~~~~~~~~~~~
  return(pkg_out)
}