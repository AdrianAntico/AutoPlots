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

#' @import data.table
#' @importFrom data.table data.table %chin% .I .N .SD := as.data.table fwrite is.data.table rbindlist set setcolorder setnames setorderv as.IDate as.ITime %like%
#' @importFrom lubridate %m+%
#' @importFrom utils installed.packages
#' @importFrom stats as.formula cor cor.test dgeom lm median na.omit optimize pnorm qnorm quantile runif sd setNames var
#' @importFrom utils head
NULL
.datatable.aware = TRUE
