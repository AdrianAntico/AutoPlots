#' @title BuildBinary
#'
#' @description Build package binary
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @noRd
BuildBinary <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "AutoQuant", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::build(pkg = "AutoQuant")
  }
  setwd(x)
}

#' @title Install
#'
#' @description To install the package
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @noRd
Install <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "AutoPlots", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::install(pkg = "AutoPlots", dependencies = FALSE)
  }
  setwd(x)
}

#' @title UpdateDocs
#'
#' @description Update helf files and reference manual
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @noRd
UpdateDocs <- function(BuildVignette = FALSE, Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub/AutoPlots")
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  }
  setwd(x)
}
