# AutoPlots is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title BuildBinary
#'
#' @description Build package binary
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#' @return nothing
#' @noRd
BuildBinary <- function(Root = NULL) {
  x <- getwd()
  on.exit(expr = setwd(x))
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "AutoPlots", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::build(pkg = "AutoPlots")
  }
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
#' @return nothing
#' @noRd
Install <- function(Root = NULL) {
  x <- getwd()
  on.exit(expr = setwd(x))
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "AutoPlots", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::install(pkg = "AutoPlots", dependencies = FALSE)
  }
}

#' @title UpdateDocs
#'
#' @description Update helf files and reference manual
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param BuildVignette logical
#' @param Root character
#' @return nothing
#' @noRd
UpdateDocs <- function(BuildVignette = FALSE, Root = NULL) {
  x <- getwd()
  on.exit(expr = setwd(x))
  if(!is.null(Root)) {
    setwd(Root)
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub/AutoPlots")
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  }
}
