#' @title Package Loading Message
#'
#' psketti loading message.
#'     
#' @param libname library
#' @param pkgname package name
#' 
#' @noRd

.onAttach <- function(libname, pkgname){
  
  # psketti info
  version <- read.dcf(file=system.file("DESCRIPTION", package = pkgname),
                      fields = "Version")
  #version <- "0.1.0"
  #ppkgname <- "psketti"

  
  sep_lines <- paste0( rep("-", options("width")$width), collapse = "")
  
  # are required packages loaded?
  pkg_out <- check_packages()
  
  
  # output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  packageStartupMessage(paste0("\033[0;", 33, "m", sep_lines, "\033[0m", "\n"),
                        # psketti version and beta warning
                        paste0("\033[0;", 33, "m", "Loading ", pkgname, " v ",
                               version, "\033[0m","\n"),
                        paste0("\033[0;", 41, "m", 
                               "Beta Software !!!!", "\033[0m","\n", "\n"))
  
  # dependencies table
  packageStartupMessage(
    paste0("\033[0;", 33, "m", "Dependencies:", "\033[0m","\n"),
    "\t", pkg_out[1, 1], "\t\t", " ", pkg_out[1, 2], " ", pkg_out[1, 3], "\n",
    "\t", pkg_out[2, 1], "\t\t", " ", pkg_out[2, 2], " ", pkg_out[2, 3], "\n",
    "\t", pkg_out[3, 1], "\t\t", " ", pkg_out[3, 2], " ", pkg_out[3, 3], "\n",
    "\t", pkg_out[4, 1], "\t\t", " ", pkg_out[4, 2], " ", pkg_out[4, 3], "\n")
  
  packageStartupMessage(
    "\n",
    paste0("please install any NOT INSTALLED dependencies using:",
           "\n",
           "install.packages('dependency')"),
    "\n",
    paste0("\033[0;", 33, "m", sep_lines, "\033[0m", "\n")
  )

  
  
  
}

