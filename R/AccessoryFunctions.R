# AutoPlots is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WAfppRRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# :: Helper Functions ::                                                      ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @noRd
SummaryFunction <- function(AggMethod) {
  if(AggMethod == "count") {
    aggFunc <- function(x) .N
  } else if(AggMethod == "mean") {
    aggFunc <- function(x) mean(x, na.rm = TRUE)
  } else if(AggMethod == "log(mean(x))") {
    aggFunc <- function(x) log(mean(x, na.rm = TRUE))
  } else if(AggMethod == "mean(abs(x))") {
    aggFunc <- function(x) mean(abs(x), na.rm = TRUE)
  } else if(AggMethod == "sum") {
    aggFunc <- function(x) sum(x, na.rm = TRUE)
  } else if(AggMethod == "log(sum(x))") {
    aggFunc <- function(x) log(sum(x, na.rm = TRUE))
  } else if(AggMethod == "sum(abs(x))") {
    aggFunc <- function(x) sum(abs(x), na.rm = TRUE)
  } else if(AggMethod == "median") {
    aggFunc <- function(x) median(x, na.rm = TRUE)
  } else if(AggMethod == "log(median(x))") {
    aggFunc <- function(x) log(median(x, na.rm = TRUE))
  } else if(AggMethod == "median(abs(x))") {
    aggFunc <- function(x) median(abs(x), na.rm = TRUE)
  } else if(AggMethod == "sd") {
    aggFunc <- function(x) sd(x, na.rm = TRUE)
  } else if(AggMethod == "log(sd(x))") {
    aggFunc <- function(x) log(sd(x, na.rm = TRUE))
  } else if(AggMethod == "sd(abs(x))") {
    aggFunc <- function(x) sd(abs(x), na.rm = TRUE)
  } else if(AggMethod == "skewness") {
    aggFunc <- function(x) e1071::skewness(x, na.rm = TRUE)
  } else if(AggMethod == "skewness(abs(x))") {
    aggFunc <- function(x) e1071::skewness(abs(x), na.rm = TRUE)
  } else if(AggMethod == "kurtosis") {
    aggFunc <- function(x) e1071::kurtosis(x, na.rm = TRUE)
  } else if(AggMethod == "kurtosis(abs(x))") {
    aggFunc <- function(x) e1071::kurtosis(abs(x), na.rm = TRUE)
  } else if(AggMethod == "CoeffVar") {
    aggFunc <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  } else if(AggMethod == "CoeffVar(abs(x))") {
    aggFunc <- function(x) sd(abs(x), na.rm = TRUE) / mean(abs(x), na.rm = TRUE)
  }
  return(aggFunc)
}

#' @noRd
ColTypes <- function(data) {
  CT <- c()
  for(Col in names(data)) CT <- c(CT, class(data[[Col]])[1L])
  CT
}

#' @noRd
bold_ <- function(x) paste0('<b>',x,'</b>')

#' @noRd
font_ <- function(family = "Segoe UI Symbol", size = 12, color = 'white') list(family = family, size = size, color = color)

#' @noRd
ColNameFilter <- function(data, Types = 'all') {
  if(Types == 'all') return(names(data))
  nam <- c()
  for(t in Types) {
    if(tolower(t) == 'numeric') {
      nam <- NumericColNames(data)
    } else if(tolower(t) == 'character') {
      nam <- CharacterColNames(data)
    } else if(tolower(t) == 'factor') {
      nam <- FactorColNames(data)
    } else if(tolower(t) == 'logical') {
      nam <- LogicalColNames(data)
    } else if(tolower(t) %chin% c("date","idate","idatetime","posixct","posix")) {
      nam <- DateColNames(data)
    }
  }
  return(nam)
}

#' @noRd
NumericColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.numeric))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
CharacterColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.character))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
FactorColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.factor))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
LogicalColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.logical))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
DateColNames <- function(data) {
  x <- list()
  counter <- 0L
  for(i in names(data)) {
    if(class(data[[i]])[1L] %in% c("IDate","Date","date","POSIXct","POSIX")) {
      counter <- counter + 1L
      x[[counter]] <- i
    }
  }
  if(length(x) > 0L) return(x) else return(NULL)
}

#' # text & logical with NULL default
#' @noRd
CEP <- function(x) if(any(missing(x))) 'NULL' else if(!exists('x')) 'NULL' else if(is.null(x)) "NULL" else if(identical(x, character(0))) "NULL" else if(identical(x, numeric(0))) "NULL" else if(identical(x, integer(0))) "NULL" else if(identical(x, logical(0))) "NULL" else if(any(x == "")) "NULL" else if(any(is.na(x))) "NULL" else if(any(x == 'None')) "NULL" else if(is.numeric(x)) x else if(length(x) > 1) paste0("c(", noquote(paste0("'", x, "'", collapse = ',')), ")") else paste0("'", x, "'")

#' # number and logical with FALSE / TRUE default
#' @noRd
CEPP <- function(x, Default = NULL, Type = 'character') if(missing(x)) 'NULL' else if(!exists('x')) 'NULL' else if(length(x) == 0) 'NULL' else if(any(is.na(x))) 'NULL' else if(all(x == "")) 'NULL' else if(Type == 'numeric') NumNull(x) else if(Type == 'character') CharNull(x)

#' @title ExpandText
#'
#' @description This function is for pasting character vector arguments into their respective parameter slots for code printing (and command line vector argument passing)
#'
#'
#' @noRd
ExpandText <- function(x) {
  if(length(x) > 0L) {
    if(is.character(x) || is.factor(x) || lubridate::is.Date(x) || lubridate::is.POSIXct(x)) {
      return(paste0("c('", paste0(x, collapse = "','"), "')"))
    } else if(is.numeric(x) || is.logical(x)) {
      return(paste0("c(", paste0(x, collapse = ","), ")"))
    }
  } else {
    return('NULL')
  }
}

#' @title CharNull
#'
#' @param x Value
#'
#' @noRd
CharNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    return(NULL)
  }

  if(!exists('x')) {
    return(NULL)
  }

  if(length(x) == 0) {
    return(NULL)
  }

  if(all(is.na(suppressWarnings(as.character(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.character(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.character(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.character(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.character(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
  }
}

#' @title FakeDataGenerator
#'
#' @description Create fake data for examples
#'
#' @author Adrian Antico
#' @family Data Wrangling
#'
#' @param Correlation Set the correlation value for simulated data
#' @param N Number of records
#' @param ID Number of IDcols to include
#' @param ZIP Zero Inflation Model target variable creation. Select from 0 to 5 to create that number of distinctly distributed data, stratifed from small to large
#' @param FactorCount Number of factor type columns to create
#' @param AddDate Set to TRUE to include a date column
#' @param AddComment Set to TRUE to add a comment column
#' @param AddWeightsColumn Add a weights column for ML
#' @param ChainLadderData Set to TRUE to return Chain Ladder Data for using AutoMLChainLadderTrainer
#' @param Classification Set to TRUE to build classification data
#' @param MultiClass Set to TRUE to build MultiClass data
#'
#' @return data.table of data
#' @export
FakeDataGenerator <- function(Correlation = 0.70,
                              N = 1000L,
                              ID = 5L,
                              FactorCount = 2L,
                              AddDate = TRUE,
                              AddComment = FALSE,
                              AddWeightsColumn = FALSE,
                              ZIP = 5L,
                              ChainLadderData = FALSE,
                              Classification = FALSE,
                              MultiClass = FALSE) {

  # Error checking
  if(sum(Classification, MultiClass) > 1) stop("Only one of the following can be set to TRUE: Classifcation, and MultiClass")

  # Create ChainLadderData
  if(ChainLadderData) {

    # Overwrite N
    N <- 1000

    # Define constants
    MaxCohortDays <- 15L

    # Start date
    CalendarDateData <- data.table::data.table(CalendarDateColumn = rep(as.Date("2018-01-01"), N), key = "CalendarDateColumn")

    # Increment date column so it is sequential
    CalendarDateData[, temp := seq_len(N)]
    CalendarDateData[, CalendarDateColumn := CalendarDateColumn + lubridate::days(temp) - 1L]
    CohortDate_temp <- data.table::copy(CalendarDateData)
    data.table::setnames(x = CohortDate_temp, old = c("CalendarDateColumn"), new = c("CohortDate_temp"))

    # Cross join the two data sets
    ChainLadderData <- data.table::setkeyv(data.table::CJ(
      CalendarDateColumn = CalendarDateData$CalendarDateColumn,
      CohortDateColumn = CohortDate_temp$CohortDate_temp,
      sorted = TRUE,
      unique = TRUE),
      cols = c("CalendarDateColumn", "CohortDateColumn"))

    # Remove starter data sets and N
    rm(CalendarDateData, CohortDate_temp, N)

    # Remove impossible dates
    ChainLadderData <- ChainLadderData[CohortDateColumn >= CalendarDateColumn]

    # Add CohortPeriods
    ChainLadderData[, CohortDays := as.numeric(difftime(CohortDateColumn, CalendarDateColumn, tz = "MST", units = "day"))]

    # Limit the number of CohortTime
    ChainLadderData <- ChainLadderData[CohortDays < MaxCohortDays]

    # Add measure columns placeholder values
    ChainLadderData[, ":=" (Leads = 0, Appointments = 0, Rates = 0)]

    # Sort decending both date columns
    data.table::setorderv(x = ChainLadderData, cols = c("CalendarDateColumn","CohortDateColumn"), order = c(-1L, 1L))

    # Add columns for BaselineMeasure and ConversionMeasure
    UniqueCalendarDates <- unique(ChainLadderData$CalendarDateColumn)
    NN <- length(UniqueCalendarDates)
    LoopSeq <- c(1:15)
    LoopSeq <- cumsum(LoopSeq)
    LoopSeq <- c(1, LoopSeq)
    LoopSeq <- c(LoopSeq, seq(135, 15*993, 15))
    for(cal in seq(NN)) {

      # Generate first element of decay data
      DecayCurveData <- dgeom(x = 0, prob = runif(n = 1L, min = 0.45, max = 0.55), log = FALSE)

      # Fill in remain elements in vector
      if(cal > 1L) {
        zz <- seq_len(min(15L, cal))
        for(i in zz[1:min(cal-1L,15)]) {
          DecayCurveData <- c(DecayCurveData, c(dgeom(x = i, prob = runif(n = 1L, min = 0.45, max = 0.55), log = FALSE)))
        }
      }

      # Fill ChainLadderData
      data.table::set(ChainLadderData, i = (LoopSeq[cal]+1L):LoopSeq[cal + 1L], j = "Rates", value = DecayCurveData[seq_len(min(15L, cal))])
    }

    # Fill in Leads and Conversions----
    x <- unique(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","Leads")])
    x[, Leads := runif(n = x[, .N], min = 100, max = 500)]
    ChainLadderData <- merge(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","CohortDateColumn","CohortDays","Appointments","Rates")], x, by = "CalendarDateColumn", all = FALSE)
    ChainLadderData[, Appointments := Leads * Rates]
    ChainLadderData[, Sales := Appointments * Rates * (runif(.N))]
    ChainLadderData[, Rates := NULL]
    data.table::setcolorder(ChainLadderData, c(1,2,3,5,4))
    return(ChainLadderData)
  }

  # Modify----
  if(MultiClass && FactorCount == 0L) {
    FactorCount <- 1L
    temp <- 1L
  }

  # Create data----
  Correl <- Correlation
  data <- data.table::data.table(Adrian = runif(N))
  data[, x1 := qnorm(Adrian)]
  data[, x2 := runif(N)]
  data[, Independent_Variable1 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable2 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
  data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
  data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
  data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
  data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
  data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
  if(ID > 0L) for(i in seq_len(ID)) data[, paste0("IDcol_", i) := runif(N)]
  data[, ":=" (x2 = NULL)]

  # FactorCount----
  for(i in seq_len(FactorCount)) {
    RandomValues <- sort(c(runif(n = 4L, min = 0.01, max = 0.99)))
    RandomLetters <- sort(c(sample(x = LETTERS, size = 5L, replace = FALSE)))
    data[, paste0("Factor_", i) := as.factor(
      data.table::fifelse(Independent_Variable1 < RandomValues[1L], RandomLetters[1L],
                          data.table::fifelse(Independent_Variable1 < RandomValues[2L], RandomLetters[2L],
                                              data.table::fifelse(Independent_Variable1 < RandomValues[3L],  RandomLetters[3L],
                                                                  data.table::fifelse(Independent_Variable1 < RandomValues[4L],  RandomLetters[4L], RandomLetters[5L])))))]
  }

  # Add date----
  if(AddDate) {
    if(FactorCount == 0) {
      data <- data[, DateTime := as.Date(Sys.time())]
      data[, temp := seq_len(.N)][, DateTime := DateTime - temp][, temp := NULL]
      data <- data[order(DateTime)]
    } else {
      data <- data[, DateTime := as.Date(Sys.time())]
      CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
      data[, temp := seq_len(.N), by = c(names(data)[c(CatFeatures)])][, DateTime := DateTime - temp][, temp := NULL]
      data.table::setorderv(x = data, cols = c("DateTime", c(names(data)[c(CatFeatures)])), order = rep(1, length(c(names(data)[c(CatFeatures)]))+1))
    }
  }

  # Zero Inflation Setup
  if(!Classification && !MultiClass) {
    if(ZIP == 1L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.5, 0, Independent_Variable8)][, Independent_Variable8 := NULL]
    } else if(ZIP == 2L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.33, 0, data.table::fifelse(Adrian < 0.66, log(Adrian * 10), log(Adrian*20)))]
    } else if(ZIP == 3L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.25, 0, data.table::fifelse(Adrian < 0.50, log(Adrian * 10), data.table::fifelse(Adrian < 0.75, log(Adrian * 50), log(Adrian * 150))))]
    } else if(ZIP == 4L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.20, 0, data.table::fifelse(Adrian < 0.40, log(Adrian * 10), data.table::fifelse(Adrian < 0.60, log(Adrian * 50), data.table::fifelse(Adrian < 0.80, log(Adrian * 150), log(Adrian * 250)))))]
    } else if(ZIP == 5L) {
      data[, Adrian := data.table::fifelse(Adrian < 1/6, 0, data.table::fifelse(Adrian < 2/6, log(Adrian * 10), data.table::fifelse(Adrian < 3/6, log(Adrian * 50), data.table::fifelse(Adrian < 4/6, log(Adrian * 250), data.table::fifelse(Adrian < 5/6, log(Adrian * 500), log(Adrian * 1000))))))]
    }
  }

  # Classification
  if(Classification) data[, Adrian := data.table::fifelse(jitter(x = Adrian, factor = 100) > 0.63, 1, 0)]

  # Remove----
  data[, ":=" (x1 = NULL)]

  # MultiClass
  if(MultiClass) {
    data[, Adrian := NULL]
    data.table::setnames(data, "Factor_1", "Adrian")
  }

  # Comment data
  if(AddComment) {
    a <- c('Hello', 'Hi', 'Howdy', 'House', 'Someone', 'Watching', 'You')
    b <- c('really like', 'absolutely adore', 'mediocre', 'great', 'stochastic')
    c <- c('noload', 'download', 'upload', 'Burn Notice', 'The Office')
    N1 <- 1/length(a)
    N2 <- 1/length(b)
    N3 <- 1/length(c)
    N11 <- 1/N1
    N22 <- 1/N2
    N33 <- 1/N3
    RandomText <- function(N1,N11,N2,N22,N3,N33,a,b,c) {
      paste(sample(x = a, size = 1, replace = TRUE, prob = rep(N1, N11)),
            sample(x = b, size = 1, replace = TRUE, prob = rep(N2, N22)),
            sample(x = c, size = 1, replace = TRUE, prob = rep(N3, N33)))
    }
    data[, Comment := "a"]
    for(i in seq_len(data[, .N])) {
      data.table::set(data, i = i, j = "Comment", value = RandomText(N1,N11,N2,N22,N3,N33,a,b,c))
    }
  }

  # Add weights column
  if(AddWeightsColumn) {
    data[, Weights := runif(.N)]
  }

  # Return data
  return(data)
}

#' @title Standardize
#'
#' @description Generate standardized values for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Center TRUE
#' @param Scale TRUE
#' @param ScoreTable FALSE. Set to TRUE to return a data.table that can be used to apply or backtransform via StandardizeScoring
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' x <- Standardize(data = data, ColNames = c('Weekly_Sales', 'XREG3'), GroupVars = c('Region','Store','Dept'), Center = TRUE, Scale = TRUE, ScoreTable = TRUE)
#' }
#'
#' @noRd
Standardize <- function(data, ColNames, GroupVars = NULL, Center = TRUE, Scale = TRUE, ScoreTable = FALSE) {

  # Standardize
  if(length(GroupVars) == 0L) {
    data[, paste0(ColNames, '_Standardize') := lapply(.SD, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = c(ColNames)]
  } else {
    data[, paste0(ColNames, '_Standardize') := lapply(.SD, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = c(ColNames), by = c(eval(GroupVars))]
  }

  # ScoreTable creation
  if(ScoreTable) {
    x <- data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ColNames), by = c(GroupVars)]
    data.table::setnames(x = x, old = ColNames, new = paste0(ColNames, "_mean"))
    y <- data[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ColNames), by = c(GroupVars)]
    data.table::setnames(x = y, old = ColNames, new = paste0(ColNames, "_sd"))
    xy <- cbind(x,y[, (GroupVars) := NULL])
  }

  # Return
  if(!ScoreTable) {
    return(data)
  } else {
    return(list(
      data = data,
      ScoreTable = xy
    ))
  }
}

#' @title StandardizeScoring
#'
#' @description Generate standardized values for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param Apply 'apply' or 'backtransform'
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Center TRUE
#' @param Scale TRUE
#'
#' @examples
#' \dontrun{
#' x <- Standardize(data = data, ColNames = c('Weekly_Sales', 'XREG1'), GroupVars = c('Region','Store','Dept'), Center = TRUE, Scale = TRUE)
#' }
#'
#' @noRd
StandardizeScoring <- function(data, ScoreTable, Apply = 'apply', GroupVars = NULL) {

  # Facts
  nam <- names(ScoreTable)[which(!names(ScoreTable) %in% GroupVars)]

  # Apply will apply standardization to new data
  # Backtransform will undo standardization
  if(Apply == 'apply') {
    data.table::setkeyv(x = data, cols = GroupVars)
    data.table::setkeyv(x = ScoreTable, cols = GroupVars)
    data[ScoreTable, paste0(nam) := mget(paste0('i.', nam))]
    nams <- nam[seq_len(length(nam) / 2)]
    ColNames <- gsub(pattern = "_mean", replacement = "", x = nams)
    for(i in ColNames) data[, paste0(i, "_Standardize") := (get(i) - get(paste0(i, "_mean"))) / get(paste0(i, "_sd"))]
    data.table::set(data, j = c(nam), value = NULL)
  } else {
    data.table::setkeyv(x = data, cols = GroupVars)
    data.table::setkeyv(x = ScoreTable, cols = GroupVars)
    data[ScoreTable, paste0(nam) := mget(paste0('i.', nam))]
    nams <- nam[seq_len(length(nam) / 2)]
    ColNames <- gsub(pattern = "_mean", replacement = "", x = nams)
    for(i in ColNames) data[, eval(i) := get(paste0(i, "_Standardize")) * get(paste0(i, "_sd")) + get(paste0(i, "_mean"))]
    data.table::set(data, j = c(nam), value = NULL)
  }

  # Return
  return(data)
}

#' @title PercRank
#'
#' @description Generate percent ranks for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Granularity Provide a value such that data.table::frank(Variable) * (1 / Granularity) / .N * Granularity. Default is 0.001
#' @param ScoreTable = FALSE. Set to TRUE to get the reference values for applying to new data. Pass to scoring version of this function
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' x <- PercRank(data, ColNames = c('Weekly_Sales', 'XREG1'), GroupVars = c('Region','Store','Dept'), Granularity = 0.001, ScoreTable = TRUE)
#' }
#'
#' @noRd
PercRank <- function(data, ColNames, GroupVars = NULL, Granularity = 0.001, ScoreTable = FALSE) {
  if(length(GroupVars) == 0L) {
    data[, paste0(ColNames, '_PercRank') := lapply(.SD, FUN = function(x) data.table::frank(x) * (1 / Granularity) / .N * Granularity), .SDcols = c(ColNames)]
  } else {
    data[, paste0(ColNames, '_PercRank') := lapply(.SD, FUN = function(x) data.table::frank(x) * (1 / Granularity) / .N * Granularity), .SDcols = c(ColNames), by = c(eval(GroupVars))]
  }
  if(!ScoreTable) {
    return(data)
  } else {
    return(list(
      data = data,
      ScoreTable = unique(data[, .SD, .SDcols = c(ColNames, paste0(ColNames, '_PercRank'))])
    ))
  }
}

#' Test YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param eps erorr tolerance
#' @param ... Arguments to pass along
#' @return YeoJohnson results
Test_YeoJohnson <- function(x,
                            eps = 0.001,
                            ...) {
  stopifnot(is.numeric(x))
  lambda <- Estimate_YeoJohnson_Lambda(x, eps = eps, ...)
  trans_data <- x
  na_idx <- is.na(x)
  trans_data[!na_idx] <- Apply_YeoJohnson(x[!na_idx], lambda, eps)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "YeoJohnson", Data = trans_data, Lambda = lambda, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Estimate YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lower the lower bound for search
#' @param upper the upper bound for search
#' @param eps erorr tolerance
#' @return YeoJohnson results
Estimate_YeoJohnson_Lambda <- function(x,
                                       lower = -5,
                                       upper = 5,
                                       eps = 0.001) {

  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]

  # See references, Yeo & Johnson Biometrika (2000)
  yj_loglik <- function(lambda) {
    x_t <- Apply_YeoJohnson(x, lambda, eps)
    x_t_bar <- mean(x_t)
    x_t_var <- var(x_t) * (n - 1) / n
    constant <- sum(sign(x) * log(abs(x) + 1))
    - 0.5 * n * log(x_t_var) + (lambda - 1) * constant
  }

  results <- optimize(
    yj_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  return(results$maximum)
}

#' Apply YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return YeoJohnson results
Apply_YeoJohnson <- function(x,
                             lambda,
                             eps = 0.001) {
  pos_idx <- x >= 0
  neg_idx <- x < 0

  # Transform negative values
  if(any(pos_idx)) {
    if(abs(lambda) < eps) {
      x[pos_idx] <- log(x[pos_idx] + 1)
    } else {
      x[pos_idx] <- ((x[pos_idx] + 1) ^ lambda - 1) / lambda
    }
  }

  # Transform nonnegative values
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      x[neg_idx] <- -log(-x[neg_idx] + 1)
    } else {
      x[neg_idx] <- -((-x[neg_idx] + 1) ^ (2 - lambda) - 1) / (2 - lambda)
    }
  }
  return(x)
}

#' Inverse YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return YeoJohnson results
InvApply_YeoJohnson <- function(x,
                                lambda,
                                eps = 0.001) {
  val <- x
  neg_idx <- x < 0
  if(any(!neg_idx)) {
    if(abs(lambda) < eps) {
      val[!neg_idx] <- exp(x[!neg_idx]) - 1
    } else {
      val[!neg_idx] <- (x[!neg_idx] * lambda + 1) ^ (1 / lambda) - 1
    }
  }
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      val[neg_idx] <- -expm1(-x[neg_idx])
    } else {
      val[neg_idx] <- 1 - (-(2 - lambda) * x[neg_idx] + 1) ^ (1 / (2 - lambda))
    }
  }
  return(val)
}

#' Test BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param ... Arguments to pass along
#' @return BoxCox results
Test_BoxCox <- function(x, ...) {
  stopifnot(is.numeric(x))
  lambda <- Estimate_BoxCox_Lambda(x, ...)
  trans_data <- Apply_BoxCox(x, lambda)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "BoxCox", Data = trans_data, Lambda = lambda, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Estimate BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lower the lower bound for search
#' @param upper the upper bound for search
#' @param eps erorr tolerance
#' @return BoxCox results
Estimate_BoxCox_Lambda <- function(x,
                                   lower = -1,
                                   upper = 2,
                                   eps = 0.001) {
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]
  if (any(x <= 0)) stop("x must be positive")
  log_x <- log(x)
  xbar <- exp(mean(log_x))
  fit <- lm(x ~ 1, data = data.frame(x = x))
  xqr <- fit$qr
  boxcox_loglik <- function(lambda) {
    if (abs(lambda) > eps)
      xt <- (x ^ lambda - 1) / lambda
    else
      xt <- log_x * (1 + (lambda * log_x) / 2 *
                       (1 + (lambda * log_x) / 3 *
                          (1 + (lambda * log_x) / 4)))
    - n / 2 * log(sum(qr.resid(xqr, xt / xbar ^ (lambda - 1)) ^ 2))
  }

  results <- optimize(
    boxcox_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  return(results$maximum)
}

#' Apply BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return BoxCox results
Apply_BoxCox <- function(x,
                         lambda,
                         eps = 0.001) {
  if(lambda < 0) x[x < 0] <- NA
  if(abs(lambda) < eps) {
    val <- log(x)
  } else {
    val <- (sign(x) * abs(x) ^ lambda - 1) / lambda
  }
  return(val)
}

#' Inverse BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return BoxCox results
InvApply_BoxCox <- function(x,
                            lambda,
                            eps = 0.001) {
  if(lambda < 0) x[x > -1 / lambda] <- NA
  if(abs(lambda) < eps) {
    val <- exp(x)
  } else {
    x <- x * lambda + 1
    val <- sign(x) * abs(x) ^ (1 / lambda)
  }
  return(val)
}

#' Test Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Test_Asinh <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- asinh(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Asinh", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Apply_Asinh <- function(x) {
  return(asinh(x))
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
InvApply_Asinh <- function(x) {
  return(sinh(x))
}

#' Test Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Test_Asin <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- asin(sqrt(x))
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Asin", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Apply_Asin <- function(x) {
  return(asin(sqrt(x)))
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
InvApply_Asin <- function(x) {
  return(sin(x) ^ 2)
}

#' Test Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Test_Logit <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x / (1 - x))
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Logit", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Apply_Logit <- function(x) {
  return(log(x / (1 - x)))
}

#' Inverse Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
InvApply_Logit <- function(x) {
  return(1 / (1 + exp(-x)))
}

#' Test Identity Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Identity results
Test_Identity <- function(x) {
  stopifnot(is.numeric(x))
  x.t <- x
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  x.t <- (x.t - mu) / sigma
  ptest <- nortest::pearson.test(x.t)
  val <- list(Name = "Identity", Data = x, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Test Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Test_Log <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Log", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Log <- function(x) {
  return(log(x))
}

#' Inverse Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Log <- function(x) {
  return(exp(x))
}

#' Test LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return LogPlus1 results
Test_LogPlus1 <- function(x) {
  stopifnot(is.numeric(x))
  xx <- min(x, na.rm = TRUE)
  if(xx <= 0) trans_data <- log(x+abs(xx)+1) else trans_data <- log(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "LogPlus1", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_LogPlus1 <- function(x) {
  return(log(x+1))
}

#' Inverse LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_LogPlus1 <- function(x) {
  return(exp(x)-1)
}

#' Test Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Sqrt results
Test_Sqrt <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- sqrt(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Sqrt", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Sqrt <- function(x) {
  return(sqrt(x))
}

#' Inverse Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Sqrt <- function(x) {
  return(x^2)
}

#' @title AutoTransformationCreate
#'
#' @description AutoTransformationCreate is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified. This function will loop through your selected transformation options (YeoJohnson, BoxCox, Asinh, Asin, and Logit) and find the one that produces data that is the closest to normally distributed data. It then makes the transformation and collects the metadata information for use in the AutoTransformationScore() function, either by returning the objects (always) or saving them to file (optional).
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data
#' @param ColumnNames List your columns names in a vector, for example, c("Target", "IV1")
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Asin", "Logit", and "Identity". Note, LogPlus1 runs
#' @param Path Set to the directly where you want to save all of your modeling files
#' @param TransID Set to a character value that corresponds with your modeling project
#' @param SaveOutput Set to TRUE to save necessary file to run AutoTransformationScore()
#' @return data with transformed columns and the transformation object for back-transforming later
#' @examples
#' \dontrun{
#' # Create Fake Data
#' data <- FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 2L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Columns to transform
#' Cols <- names(data)[1L:11L]
#' print(Cols)
#'
#' # Run function
#' data <- AutoTransformationCreate(
#'   data,
#'   ColumnNames = Cols,
#'   Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "Identity"),
#'   Path = getwd(),
#'   TransID = "Trans",
#'   SaveOutput = TRUE)
#' }
#' @noRd
AutoTransformationCreate <- function(data,
                                     ColumnNames = NULL,
                                     Methods = c("BoxCox","YeoJohnson","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","Identity"),
                                     Path = NULL,
                                     TransID = "ModelID",
                                     SaveOutput = FALSE) {

  # Check arguments
  Methods <- unique(tolower(Methods))
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!any(tolower(Methods) %chin% c("boxcox", "yeojohnson", "asinh", "sqrt", "log", "logplus1", "asin", "logit"))) stop("Methods not supported")
  # if(!"identity" %chin% Methods) Methods <- c(Methods, "identity")
  if(is.numeric(ColumnNames) || is.integer(ColumnNames)) ColumnNames <- names(data)[ColumnNames]
  for(i in ColumnNames) if(!(any(class(data[[eval(i)]]) %chin% c("numeric", "integer")))) stop("ColumnNames must be for numeric or integer columns")

  # Loop through ColumnNames
  # colNames = 1
  for(colNames in seq_along(ColumnNames)) {# colNames = 1L

    # Collection Object
    if(length(Methods) < 5) {
      EvaluationTable <- data.table::data.table(
        ColumnName = rep("BLABLA", length(ColumnNames) * (length(Methods)+1)),
        MethodName = rep("BLABLA", length(ColumnNames) * (length(Methods)+1)),
        Lambda = rep(1.0, length(ColumnNames) * (length(Methods)+1)),
        NormalizedStatistics = rep(1.0, length(ColumnNames) * (length(Methods)+1)))
    } else {
      EvaluationTable <- data.table::data.table(
        ColumnName = rep("BLABLA", length(ColumnNames) * (length(Methods) + 1)),
        MethodName = rep("BLABLA", length(ColumnNames) * (length(Methods) + 1)),
        Lambda = rep(1.0, length(ColumnNames) * (length(Methods) + 1)),
        NormalizedStatistics = rep(1.0, length(ColumnNames) * (length(Methods) + 1)))
    }
    DataCollection <- list()
    Counter <- 0L

    # Check range of data
    MinVal <- min(data[[eval(ColumnNames[colNames])]], na.rm = TRUE)
    MaxVal <- max(data[[eval(ColumnNames[colNames])]], na.rm = TRUE)

    # Create Final Methods Object
    FinalMethods <- Methods

    # Update Methods
    if(MinVal <= 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("boxcox","log","logit"))]
    if(MinVal < 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("sqrt","asin"))]
    if(MaxVal > 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("asin"))]
    if(MaxVal >= 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("logit"))]

    # Store column data as vector
    x <- data[[eval(ColumnNames[colNames])]]

    # YeoJohnson
    if(any(tolower(FinalMethods) %chin% "yeojohnson")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_YeoJohnson(x)
      DataCollection[["yeojohnson"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Log
    if(any(tolower(FinalMethods) %chin% "log")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Log(x)
      DataCollection[["log"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # LogPlus1
    if(any(tolower(FinalMethods) %chin% "logplus1")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_LogPlus1(x)
      DataCollection[["logplus1"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Sqrt
    if(any(tolower(FinalMethods) %chin% "sqrt")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Sqrt(x)
      DataCollection[["sqrt"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # BoxCox
    if(any(tolower(FinalMethods) %chin% "boxcox")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_BoxCox(x)
      DataCollection[["boxcox"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Asinh
    if(any(tolower(FinalMethods) %chin% "asinh")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Asinh(x)
      DataCollection[["asinh"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Asin
    if(any(tolower(FinalMethods) %chin% "asin")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Asin(x)
      DataCollection[["asin"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Logit
    if(any(tolower(FinalMethods) %chin% "logit")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Logit(x)
      DataCollection[["logit"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Identity
    if(any(tolower(FinalMethods) %chin% "identity")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Identity(x)
      DataCollection[["identity"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Pick winner
    EvaluationTable <- EvaluationTable[MethodName != "BLABLA"]
    if(colNames == 1L) {
      Results <- EvaluationTable[order(NormalizedStatistics)][1L]
    } else {
      Results <- data.table::rbindlist(list(Results, EvaluationTable[order(NormalizedStatistics)][1L]))
    }

    # Apply to data----
    data <- tryCatch({data[, ColumnNames[colNames] := DataCollection[[tolower(Results[eval(colNames), MethodName])]]]}, error = function(x) data)
  }

  # Save output----
  if(SaveOutput && !is.null(Path)) data.table::fwrite(Results, file = file.path(normalizePath(Path), paste0(TransID, "_transformation.csv")))

  # Return data----
  return(list(Data = data, FinalResults = Results))
}

#' @title ClassificationMetrics
#'
#' @description ClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param TestData Test data from your modeling
#' @param Thresholds Value
#' @param Target Name of your target variable
#' @param PredictColumnName Name of your predicted value variable
#' @param PositiveOutcome The value of the positive outcome level
#' @param NegativeOutcome The value of the negative outcome level
#' @param CostMatrix c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost)
#' @noRd
ClassificationMetrics <- function(TestData,
                                  Thresholds,
                                  Target,
                                  PredictColumnName,
                                  PositiveOutcome,
                                  NegativeOutcome,
                                  CostMatrix = c(0,1,1,0)) {

  if("Target" %chin% names(TestData)) data.table::set(TestData, j = "Target", value = NULL)
  ThreshLength <- rep(1, length(Thresholds))
  ThresholdOutput <- data.table::data.table(
    Threshold   = ThreshLength,
    TN          = ThreshLength,
    TP          = ThreshLength,
    FN          = ThreshLength,
    FP          = ThreshLength,
    N           = ThreshLength,
    P           = ThreshLength,
    MCC         = ThreshLength,
    Accuracy    = ThreshLength,
    TPR         = ThreshLength,
    TNR         = ThreshLength,
    FNR         = ThreshLength,
    FPR         = ThreshLength,
    FDR         = ThreshLength,
    FOR         = ThreshLength,
    F1_Score    = ThreshLength,
    F2_Score    = ThreshLength,
    F0.5_Score  = ThreshLength,
    NPV         = ThreshLength,
    PPV         = ThreshLength,
    ThreatScore = ThreshLength,
    Utility     = ThreshLength)
  counter <- 0L
  for(Thresh in Thresholds) {
    counter <- counter + 1L
    TN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
    TP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
    FN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
    FP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
    N1  <- TestData[, .N]
    N  <- TestData[get(PredictColumnName) < eval(Thresh), .N]
    P1  <- TestData[get(Target) == 1, .N]
    P  <- TestData[get(Target) == 1 & get(PredictColumnName) > Thresh, .N]

    # Calculate metrics ----
    MCC         <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    Accuracy    <- (TP+TN)/N1
    TPR         <- TP/P1
    TNR         <- TN/(N1-P1)
    FNR         <- FN / P1
    FPR         <- FP / N1
    FDR         <- FP / (FP + TP)
    FOR         <- FN / (FN + TN)
    F1_Score    <- 2 * TP / (2 * TP + FP + FN)
    F2_Score    <- 3 * TP / (2 * TP + FP + FN)
    F0.5_Score  <- 1.5 * TP / (0.5 * TP + FP + FN)
    NPV         <- TN / (TN + FN)
    PPV         <- TP / (TP + FP)
    ThreatScore <- TP / (TP + FN + FP)
    Utility     <- P1/N1 * (CostMatrix[1L] * TPR + CostMatrix[2L] * (1 - TPR)) + (1 - P1/N1) * (CostMatrix[3L] * FPR + CostMatrix[4L] * (1 - FPR))

    # Fill in values ----
    data.table::set(ThresholdOutput, i = counter, j = "Threshold",   value = Thresh)
    data.table::set(ThresholdOutput, i = counter, j = "P",           value = P)
    data.table::set(ThresholdOutput, i = counter, j = "N",           value = N)
    data.table::set(ThresholdOutput, i = counter, j = "TN",          value = TN)
    data.table::set(ThresholdOutput, i = counter, j = "TP",          value = TP)
    data.table::set(ThresholdOutput, i = counter, j = "FP",          value = FP)
    data.table::set(ThresholdOutput, i = counter, j = "FN",          value = FN)
    data.table::set(ThresholdOutput, i = counter, j = "Utility",     value = Utility)
    data.table::set(ThresholdOutput, i = counter, j = "MCC",         value = MCC)
    data.table::set(ThresholdOutput, i = counter, j = "Accuracy",    value = Accuracy)
    data.table::set(ThresholdOutput, i = counter, j = "F1_Score",    value = F1_Score)
    data.table::set(ThresholdOutput, i = counter, j = "F0.5_Score",  value = F0.5_Score)
    data.table::set(ThresholdOutput, i = counter, j = "F2_Score",    value = F2_Score)
    data.table::set(ThresholdOutput, i = counter, j = "NPV",         value = NPV)
    data.table::set(ThresholdOutput, i = counter, j = "TPR",         value = TPR)
    data.table::set(ThresholdOutput, i = counter, j = "TNR",         value = TNR)
    data.table::set(ThresholdOutput, i = counter, j = "FNR",         value = FNR)
    data.table::set(ThresholdOutput, i = counter, j = "FPR",         value = FPR)
    data.table::set(ThresholdOutput, i = counter, j = "FDR",         value = FDR)
    data.table::set(ThresholdOutput, i = counter, j = "FOR",         value = FOR)
    data.table::set(ThresholdOutput, i = counter, j = "PPV",         value = PPV)
    data.table::set(ThresholdOutput, i = counter, j = "ThreatScore", value = ThreatScore)
  }

  # Remove NA's
  ThresholdOutput <- ThresholdOutput[, RowSum := rowSums(x = as.matrix(ThresholdOutput))][!is.na(RowSum)][, RowSum := NULL]
  return(ThresholdOutput)
}

#' @title RemixClassificationMetrics
#'
#' @description RemixClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param TargetVariable Name of your target variable
#' @param Thresholds seq(0.01,0.99,0.01),
#' @param CostMatrix c(1,0,0,1) c(TP utility, FN utility, FP utility, TN utility)
#' @param ClassLabels c(1,0),
#' @param ValidationData. Test data
#' @examples
#' \dontrun{
#' RemixClassificationMetrics <- function(
    #'   TargetVariable = "Adrian",
#'   Thresholds = seq(0.01,0.99,0.01),
#'   CostMatrix = c(1,0,0,1),
#'   ClassLabels = c(1,0),
#'   ValidationData. = ValidationData)
#' }
#' @noRd
RemixClassificationMetrics <- function(TargetVariable = NULL,
                                       Thresholds = seq(0.01,0.99,0.01),
                                       CostMatrix = c(1,0,0,1),
                                       ClassLabels = c(1,0),
                                       ValidationData. = NULL) {

  # Create metrics
  if(!"p1" %chin% names(ValidationData.)) data.table::setnames(ValidationData., "Predict", "p1")
  temp <- ClassificationMetrics(
    TestData = ValidationData.,
    Target = eval(TargetVariable),
    PredictColumnName = "p1",
    Thresholds = Thresholds,
    PositiveOutcome = ClassLabels[1L],
    NegativeOutcome = ClassLabels[2L],
    CostMatrix = CostMatrix)
  if(temp[,.N] > 95) data.table::setorderv(temp, cols = "MCC", order = -1L, na.last = TRUE)

  # Return values----
  return(temp)
}

#' @title BinaryMetrics
#'
#' @description Compute binary metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param ClassWeights. = ClassWeights
#' @param CostMatrixWeights. = CostMatrixWeights
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#' @param Method 'threshold' for 0.01 to 0.99 by 0.01 thresholds or 'bins' for 20 equally sized bins
#'
#' @noRd
BinaryMetrics <- function(ClassWeights. = ClassWeights,
                          CostMatrixWeights. = CostMatrixWeights,
                          SaveModelObjects. = SaveModelObjects,
                          ValidationData. = ValidationData,
                          TrainOnFull. = TrainOnFull,
                          TargetColumnName. = TargetColumnName,
                          ModelID. = ModelID,
                          model_path. = model_path,
                          metadata_path. = metadata_path,
                          Method = "threshold") {
  if(is.null(CostMatrixWeights.)) CostMatrixWeights. <- c(ClassWeights.[1L], 0, 0, ClassWeights.[2L])
  if(Method == "threshold") {
    vals <- seq(0.01,0.99,0.01)
  } else if(Method == "bins") {
    temp <- ValidationData.$p1
    vals <- quantile(temp, probs = seq(0.05,1,0.05), type = 7)
  }
  if(SaveModelObjects. && !TrainOnFull.) {
    EvalMetrics <- RemixClassificationMetrics(TargetVariable = eval(TargetColumnName.), Thresholds = unique(vals), CostMatrix = CostMatrixWeights., ClassLabels = c(1,0), ValidationData. = ValidationData.)
  } else {
    EvalMetrics <- RemixClassificationMetrics(TargetVariable = eval(TargetColumnName.), Thresholds = unique(vals), CostMatrix = CostMatrixWeights., ClassLabels = c(1,0), ValidationData. = ValidationData.)
  }
  EvalMetrics[, P_Predicted := TP + FP]
  data.table::setcolorder(EvalMetrics, c(1,ncol(EvalMetrics),2:(ncol(EvalMetrics)-1)))
  data.table::setcolorder(EvalMetrics, c(1:8, ncol(EvalMetrics), 9:10, 17:19, 11:16, 20:(ncol(EvalMetrics)-1)))
  data.table::setcolorder(EvalMetrics, c(1:14, ncol(EvalMetrics), 15:(ncol(EvalMetrics)-1)))
  data.table::setorderv(EvalMetrics, "Utility", -1)
  return(EvalMetrics)
}

#' @title DummifyDT
#'
#' @description DummifyDT creates dummy variables for the selected columns. Either one-hot encoding, N+1 columns for N levels, or N columns for N levels.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data The data set to run the micro auc on
#' @param cols A vector with the names of the columns you wish to dichotomize
#' @param TopN Default is NULL. Scalar to apply to all categorical columns or a vector to apply to each categorical variable. Only create dummy variables for the TopN number of levels. Will be either TopN or max(levels)
#' @param OneHot Set to TRUE to run one hot encoding, FALSE to generate N columns for N levels
#' @param KeepFactorCols Set to TRUE to keep the original columns used in the dichotomization process
#' @param SaveFactorLevels Set to TRUE to save unique levels of each factor column to file as a csv
#' @param SavePath Provide a file path to save your factor levels. Use this for models that you have to create dummy variables for.
#' @param ImportFactorLevels Instead of using the data you provide, import the factor levels csv to ensure you build out all of the columns you trained with in modeling.
#' @param FactorLevelsList Supply a list of factor variable levels
#' @param ClustScore This is for scoring AutoKMeans. It converts the added dummy column names to conform with H2O dummy variable naming convention
#' @param ReturnFactorLevels If you want a named list of all the factor levels returned, set this to TRUE. Doing so will cause the function to return a list with the source data.table and the list of factor variables' levels
#' @param GroupVar Ignore this
#' @examples
#' \dontrun{
#  # Create fake data with 10 categorical columns
#' data <- FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 10L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Create dummy variables
#' data <- DummifyDT(
#'   data = data,
#'   cols = c("Factor_1",
#'            "Factor_2",
#'            "Factor_3",
#'            "Factor_4",
#'            "Factor_5",
#'            "Factor_6",
#'            "Factor_8",
#'            "Factor_9",
#'            "Factor_10"),
#'   TopN = c(rep(3,9)),
#'   KeepFactorCols = TRUE,
#'   OneHot = FALSE,
#'   SaveFactorLevels = TRUE,
#'   SavePath = getwd(),
#'   ImportFactorLevels = FALSE,
#'   FactorLevelsList = NULL,
#'   ClustScore = FALSE,
#'   ReturnFactorLevels = FALSE)
#'
#' # Create Fake Data for Scoring Replication
#' data <- FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 10L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Scoring Version
#' data <- DummifyDT(
#'   data = data,
#'   cols = c("Factor_1",
#'            "Factor_2",
#'            "Factor_3",
#'            "Factor_4",
#'            "Factor_5",
#'            "Factor_6",
#'            "Factor_8",
#'            "Factor_9",
#'            "Factor_10"),
#'   TopN = c(rep(3,9)),
#'   KeepFactorCols = TRUE,
#'   OneHot = FALSE,
#'   SaveFactorLevels = TRUE,
#'   SavePath = getwd(),
#'   ImportFactorLevels = TRUE,
#'   FactorLevelsList = NULL,
#'   ClustScore = FALSE,
#'   ReturnFactorLevels = FALSE)
#' }
#' @return Either a data table with new dummy variables columns and optionally removes base columns (if ReturnFactorLevels is FALSE), otherwise a list with the data.table and a list of the factor levels.
#' @export
DummifyDT <- function(data,
                      cols,
                      TopN               = NULL,
                      KeepFactorCols     = FALSE,
                      OneHot             = FALSE,
                      SaveFactorLevels   = FALSE,
                      SavePath           = NULL,
                      ImportFactorLevels = FALSE,
                      FactorLevelsList   = NULL,
                      ClustScore         = FALSE,
                      ReturnFactorLevels = FALSE,
                      GroupVar           = FALSE) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check arguments ----
  if(!is.null(TopN)) if(length(TopN) > 1L && length(TopN) != length(cols)) stop("TopN must match the length of cols")
  if(!is.null(TopN)) if(length(TopN) > 1L) TopN <- rev(TopN)
  if(!is.character(cols)) stop("cols needs to be a character vector of names")
  if(!is.logical(KeepFactorCols)) stop("KeepFactorCols needs to be either TRUE or FALSE")
  if(!is.logical(KeepFactorCols)) stop("KeepFactorCols needs to be either TRUE or FALSE")
  if(!is.logical(OneHot)) stop("OneHot needs to be either TRUE or FALSE")
  if(!is.logical(SaveFactorLevels)) stop("SaveFactorLevels needs to be either TRUE or FALSE")
  if(!is.logical(ImportFactorLevels)) stop("ImportFactorLevels needs to be either TRUE or FALSE")
  if(!is.logical(ClustScore)) stop("ClustScore needs to be either TRUE or FALSE")
  if(!is.null(SavePath)) if(!is.character(SavePath)) stop("SavePath needs to be a character value of a folder location")

  # Ensure correct argument settings ----
  if(OneHot && ClustScore) {
    OneHot <- FALSE
    KeepFactorCols <- FALSE
  }

  # Build dummies start ----
  FactorsLevelsList <- list()
  if(!GroupVar) if(length(cols) > 1L && "GroupVar" %chin% cols) cols <- cols[!cols %chin% "GroupVar"]
  if(length(TopN) > 1L) Counter <- 1L
  for(col in cols) {
    size <- ncol(data)
    Names <- setdiff(names(data), col)
    if(ImportFactorLevels) {
      temp <- data.table::fread(file.path(SavePath, paste0(col, ".csv")), sep = ",")
      inds <- sort(unique(temp[[eval(col)]]))
    } else if(!is.null(FactorLevelsList)) {
      temp <- FactorLevelsList[[eval(col)]]
      inds <- sort(unique(temp[[eval(col)]]))
    } else if(!is.null(TopN)) {
      if(length(TopN) > 1L) {
        indss <- data[, .N, by = eval(col)][order(-N)]
        inds <- sort(indss[seq_len(min(TopN[Counter], .N)), get(col)])
        if(length(TopN) > 1L) Counter <- Counter + 1L
      } else {
        indss <- data[, .N, by = eval(col)][order(-N)]
        inds <- sort(indss[seq_len(min(TopN, .N)), get(col)])
      }
    } else {
      indss <- data[, .N, by = eval(col)][order(-N)]
      inds <- sort(unique(data[[eval(col)]]))
    }

    # Allocate columns ----
    data.table::alloc.col(data, n = ncol(data) + length(inds))

    # Save factor levels for scoring later ----
    if(SaveFactorLevels) {
      if(!is.null(TopN)) {
        if(length(TopN) > 1L) {
          temp <- indss[seq_len(min(TopN[Counter-1L], .N))][, N := NULL]
          data.table::fwrite(x = temp, file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
        } else {
          temp <- indss[seq_len(min(TopN, .N))][, N := NULL]
          data.table::fwrite(x = temp, file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
        }
      } else {
        temp <- indss[, N := NULL]
        data.table::fwrite(x = temp, file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
      }
    }

    # Collect Factor Levels ----
    if(ReturnFactorLevels && SaveFactorLevels) {
      FactorsLevelsList[[eval(col)]] <- temp
    } else if(ReturnFactorLevels) {
      FactorsLevelsList[[eval(col)]] <- data[, get(col), by = eval(col)][, V1 := NULL]
    }

    # Convert to character if col is factor ----
    if(is.factor(data[[eval(col)]])) data.table::set(data, j = eval(col), value = as.character(data[[eval(col)]]))

    # If for clustering set up old school way ----
    if(!ClustScore) {
      data.table::set(data, j = paste0(col, "_", inds), value = 0L)
    } else {
      data.table::set(data, j = paste0(col, inds), value = 0L)
    }

    # Build dummies ----
    for(ind in inds) {
      if(!ClustScore) {
        data.table::set(data, i = which(data[[col]] %in% ind), j = paste0(col, "_", ind), value = 1L)
      } else {
        data.table::set(data, i = which(data[[col]] %in% ind), j = paste0(col, ind),value = 1L)
      }
    }

    # Remove original factor columns ----
    if(!KeepFactorCols) data.table::set(data, j = eval(col), value = NULL)
    if(ClustScore) setcolorder(data, c(setdiff(names(data), Names), Names))
    if(OneHot) data.table::set(data, j = paste0(col, "_Base"), value = 0L)
  }

  # Clustering section ----
  if(ClustScore) data.table::setnames(data, names(data), tolower(gsub('[[:punct:] ]+', replacement = "", names(data))))

  # Return data ----
  if(ReturnFactorLevels) {
    return(list(data = data, FactorLevelsList = FactorsLevelsList))
  } else {
    return(data)
  }
}

#' @title AutoLagRollStats
#'
#' @description AutoLagRollStats Builds lags and a large variety of rolling statistics with options to generate them for hierarchical categorical interactions.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param Targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param DateColumn The column name of your date column used to sort events over time
#' @param IndependentGroups A vector of categorical column names that you want to have run independently of each other. This will mean that no interaction will be done.
#' @param HierarchyGroups A vector of categorical column names that you want to have generate all lags and rolling stats done for the individual columns and their full set of interactions.
#' @param TimeGroups A vector of TimeUnits indicators to specify any time-aggregated GDL features you want to have returned. E.g. c("raw" (no aggregation is done),"hour", "day","week","month","quarter","year")
#' @param TimeBetween Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param TimeUnit List the time aggregation level for the time between events features, such as "hour", "day", "weeks", "months", "quarter", or "year"
#' @param TimeUnitAgg List the time aggregation of your data that you want to use as a base time unit for your features. E.g. "raw" or "day"
#' @param Lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param MA_RollWindows A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SD_RollWindows  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param Skew_RollWindows  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param Kurt_RollWindows  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantile_RollWindows A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantiles_Selected Select from the following c("q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60"," q65", "q70", "q75", "q80", "q85", "q90", "q95")
#' @param RollOnLag1 Set to FALSE to build rolling stats off of target columns directly or set to TRUE to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param ShortName Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.
#' @param Debug Set to TRUE to get a print of which steps are running
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @export
AutoLagRollStats <- function(data,
                             Targets              = NULL,
                             HierarchyGroups      = NULL,
                             IndependentGroups    = NULL,
                             DateColumn           = NULL,
                             TimeUnit             = NULL,
                             TimeUnitAgg          = NULL,
                             TimeGroups           = NULL,
                             TimeBetween          = NULL,
                             RollOnLag1           = TRUE,
                             Type                 = "Lag",
                             SimpleImpute         = TRUE,
                             Lags                 = NULL,
                             MA_RollWindows       = NULL,
                             SD_RollWindows       = NULL,
                             Skew_RollWindows     = NULL,
                             Kurt_RollWindows     = NULL,
                             Quantile_RollWindows = NULL,
                             Quantiles_Selected   = NULL,
                             ShortName            = TRUE,
                             Debug                = FALSE) {

  # Define args ----
  RollFunctions <- c()
  if(!is.null(MA_RollWindows)) RollFunctions <- c(RollFunctions,"mean")
  if(!is.null(SD_RollWindows)) RollFunctions <- c(RollFunctions,"sd")
  if(!is.null(Skew_RollWindows)) RollFunctions <- c(RollFunctions,"skew")
  if(!is.null(Kurt_RollWindows)) RollFunctions <- c(RollFunctions,"kurt")
  if(!is.null(Quantiles_Selected)) RollFunctions <- c(RollFunctions,Quantiles_Selected)
  if(is.null(TimeBetween)) TimeBetween <- NULL else TimeBetween <- "TimeBetweenRecords"
  if(RollOnLag1) RollOnLag1 <- 1L else RollOnLag1 <- 0L
  TimeGroupPlaceHolder <- c()
  if("raw" %chin% tolower(TimeGroups)) TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "raw")
  if(any(c("hours","hour","hr","hrs","hourly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "hour")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
  }
  if(any(c("days","day","dy","dd","d") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "day")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
  }
  if(any(c("weeks","week","weaks","weak","wk","wkly","wks") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "weeks")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
  }
  if(any(c("months","month","mth","mnth","monthly","mnthly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "months")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
  }
  if(any(c("quarter","quarters","qarter","quarterly","q","qtly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "quarter")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
  }
  if(any(c("year","years","annual","yearly","annually","ann","yr","yrly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "year")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
  }
  TimeGroups <- TimeGroupPlaceHolder
  if(is.null(TimeUnitAgg)) TimeUnitAgg <- TimeGroups[1L]
  #The correct TimeGroups are: c("hour", "day", "weeks", "months", "quarter", "year", "1min", "5min", "10min", "15min", "30min", "45min")

  # Ensure date column is proper ----
  if(Debug) print("Data Wrangling: Convert DateColumnName to Date or POSIXct----")
  if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
    if(is.character(data[[eval(DateColumn)]])) {
      x <- data[1,get(DateColumn)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data.table::set(data, j = eval(DateColumn), value = as.Date(data[[eval(DateColumn)]], tryFormats = x1))
    }
  } else {
    data.table::set(data, j = eval(DateColumn), value = as.POSIXct(data[[eval(DateColumn)]]))
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: No Categoricals")

  # No Categoricals----
  if(is.null(IndependentGroups) && is.null(HierarchyGroups)) {

    # Initialize Counter----
    Counter <- 0L

    # Loop through various time aggs----
    for(timeaggs in TimeGroups) {

      # Increment Counter----
      Counter <- Counter + 1L

      # Copy data----
      tempData <- data.table::copy(data)

      # Check time scale----
      if(Counter > 1) {

        # Floor Date column to timeagg level----
        data.table::set(tempData, j = eval(DateColumn), value = lubridate::floor_date(x = tempData[[eval(DateColumn)]], unit = timeaggs))

        # Agg by date column----
        tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), by = c(eval(DateColumn))]

        # Build features----
        tempData <- DT_GDL_Feature_Engineering(
          tempData,
          lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
          periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
          SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
          Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
          Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
          Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
          statsFUNs       = RollFunctions,
          targets         = Targets,
          groupingVars    = NULL,
          sortDateName    = DateColumn,
          timeDiffTarget  = NULL,
          timeAgg         = timeaggs,
          WindowingLag    = RollOnLag1,
          ShortName       = ShortName,
          Type            = Type,
          SimpleImpute    = SimpleImpute)

      } else {

        # Build features----
        data.table::setkeyv(data <- DT_GDL_Feature_Engineering(
          data,
          lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
          periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
          SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
          Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
          Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
          Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
          statsFUNs       = RollFunctions,
          targets         = Targets,
          groupingVars    = NULL,
          sortDateName    = DateColumn,
          timeDiffTarget  = NULL,
          timeAgg         = timeaggs,
          WindowingLag    = RollOnLag1,
          ShortName       = ShortName,
          Type            = Type,
          SimpleImpute    = SimpleImpute), DateColumn)
      }

      # Check if timeaggs is same of TimeUnit----
      if(Counter > 1L) {
        data.table::setkeyv(data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], "TEMPDATE")
        data[tempData, (setdiff(names(tempData), names(data))) := mget(paste0("i.", setdiff(names(tempData), names(data))))]
        data.table::set(data, j = "TEMPDATE", value = NULL)
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: Indep + Hierach")

  # Hierarchy Categoricals----
  if(!is.null(HierarchyGroups)) {

    # Categorical Names Fully Interacted----
    Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchyGroups, BottomsUp = TRUE)

    # Categorical Names Fully Interacted (Check if there already)----
    for(cat in seq_len(length(Categoricals)-length(HierarchyGroups))) {
      if(!any(names(data) %chin% Categoricals[cat])) data[, eval(Categoricals[cat]) := do.call(paste, c(.SD, sep = " ")), .SDcols = c(unlist(data.table::tstrsplit(Categoricals[cat], "_")))]
    }

    # Loop through each feature interaction
    Counter <- 0L
    for(Fact in Categoricals) {

      # Loop through all TimeGroups----
      for(timeaggs in TimeGroups) {

        # Counter incrementing
        Counter <- Counter + 1L

        # Check if timeaggs is same of TimeUnitAgg ----
        if(Counter > 1L) {

          # Aggregate tempData and tempRegs to correct dimensional level----
          tempData <- data[, .SD, .SDcols = c(eval(Targets), eval(DateColumn), eval(Fact))]

          # Agg by date column ----
          if(timeaggs != "raw") {
            tempData[, eval(DateColumn) := lubridate::floor_date(x = get(DateColumn), unit = timeaggs)]
            tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), by = c(eval(DateColumn), eval(Fact))]
          }

          # Build GDL Features----
          data.table::setkeyv(tempData <- DT_GDL_Feature_Engineering(
            tempData,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            ShortName       = ShortName,
            Type            = Type,
            SimpleImpute    = SimpleImpute), c(Fact, DateColumn))

        } else {

          # Build GDL Features----
          data <- DT_GDL_Feature_Engineering(
            data,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            ShortName       = ShortName,
            Type            = Type,
            SimpleImpute    = SimpleImpute)
        }

        # Check if timeaggs is same of TimeUnit----
        if(Counter > 1L) {
          data.table::setkeyv(data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], c(Fact,"TEMPDATE"))
          data[tempData, (setdiff(names(tempData), names(data))) := mget(paste0("i.", setdiff(names(tempData), names(data))))]
          data.table::set(data, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: Indep")

  # Single categoricals at a time AND no hierarchical: if there are hierarchical the single cats will be handled above----
  if(!is.null(IndependentGroups) && is.null(HierarchyGroups)) {

    # Loop through IndependentGroups----
    Counter <- 0L
    # Fact = IndependentGroups[1]
    # timeaggs = TimeGroups[1]
    for(Fact in IndependentGroups) {

      # Loop through all TimeGroups----
      for(timeaggs in TimeGroups) {

        # Counter incrementing
        Counter <- Counter + 1L

        # Copy data----
        tempData <- data.table::copy(data)

        # Check if timeaggs is same of TimeUnit ----
        if(Counter > 1L) {

          # Floor Date column to timeagg level ----
          tempData[, eval(DateColumn) := lubridate::floor_date(x = get(DateColumn), unit = timeaggs)]

          # Agg by date column----
          tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), by = c(eval(DateColumn),eval(Fact))]

          # Build GDL Features----
          data.table::setkeyv(tempData <- DT_GDL_Feature_Engineering(
            tempData,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            ShortName       = ShortName,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            SimpleImpute    = SimpleImpute), c(Fact, DateColumn))

        } else {

          # Set up for binary search instead of vector scan
          data.table::setkeyv(x = data, cols = c(eval(Fact),eval(DateColumn)))

          # Build GDL Features
          data <- DT_GDL_Feature_Engineering(
            data,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            ShortName       = ShortName,
            SimpleImpute    = SimpleImpute)
        }

        # Check if timeaggs is same of TimeUnit ----
        if(Counter > 1L) {
          data.table::setkeyv(data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], c(Fact, "TEMPDATE"))
          data[tempData, (setdiff(names(tempData), names(data))) := mget(paste0("i.", setdiff(names(tempData), names(data))))]
          data.table::set(data, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Simple impute missed ----
  if(SimpleImpute) {
    for(miss in seq_along(data)) {
      data.table::set(data, i = which(is.na(data[[miss]])), j = miss, value = -1)
    }
  }

  # Return data ----
  if("TEMPDATE" %chin% names(data)) data.table::set(data, j = "TEMPDATE", value = NULL)
  return(data)
}

#' @title DT_GDL_Feature_Engineering
#'
#' @description Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SDperiods  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param Skewperiods  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param Kurtperiods  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantileperiods A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Select from the following c("mean","sd","skew","kurt","q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95")
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param ShortName Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @export
DT_GDL_Feature_Engineering <- function(data,
                                       lags            = 1,
                                       periods         = 0,
                                       SDperiods       = 0,
                                       Skewperiods     = 0,
                                       Kurtperiods     = 0,
                                       Quantileperiods = 0,
                                       statsFUNs       = c("mean"),
                                       targets         = NULL,
                                       groupingVars    = NULL,
                                       sortDateName    = NULL,
                                       timeDiffTarget  = NULL,
                                       timeAgg         = c("days"),
                                       WindowingLag    = 0,
                                       ShortName       = TRUE,
                                       Type            = c("Lag"),
                                       SimpleImpute    = TRUE) {

  # timeAgg
  if(is.null(timeAgg)) {
    timeAgg <- "TimeUnitNULL"
  } else if(tolower(timeAgg) == "raw") {
    timeAggss <- "transactional"
    timeAgg <- "day"
  } else {
    timeAggss <- timeAgg
  }

  # Number of targets
  tarNum <- length(targets)

  # Argument Checks
  if(is.null(lags) && WindowingLag == 1) lags <- 1
  if(!(1 %in% lags) && WindowingLag == 1) lags <- c(1, lags)
  if(any(lags < 0)) stop("lags need to be positive integers")
  if(!is.null(groupingVars)) if(!is.character(groupingVars)) stop("groupingVars needs to be a character scalar or vector")
  if(!is.character(targets)) stop("targets needs to be a character scalar or vector")
  if(!is.character(sortDateName)) stop("sortDateName needs to be a character scalar or vector")
  if(!is.null(timeAgg)) if(!is.character(timeAgg)) stop("timeAgg needs to be a character scalar or vector")
  if(!(WindowingLag %in% c(0, 1))) stop("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) stop("Type needs to be either Lag or Lead")
  if(!is.logical(SimpleImpute)) stop("SimpleImpute needs to be TRUE or FALSE")

  # Ensure enough columns are allocated beforehand
  if(!is.null(groupingVars)) {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
    }
  } else {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs))
    }
  }

  # Begin feature engineering----
  if(!is.null(groupingVars)) {
    for(i in seq_along(groupingVars)) {# i = 1
      Targets <- targets

      # Sort data----
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1L])
        data.table::setorderv(data, colVar, order = 1L)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1L])
        data.table::setorderv(data, colVar, order = -1L)
      }

      # Lags ----
      LAG_Names <- c()
      for(t in Targets) {
        if(ShortName) {
          LAG_Names <- c(LAG_Names, paste0(timeAggss, "_LAG_", lags, "_", t))
        } else {
          LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", groupingVars[i], "_LAG_", lags, "_", t))
        }
      }
      data[, paste0(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), by = c(groupingVars[i]), .SDcols = c(Targets)]

      # Define targets----
      if(WindowingLag != 0L) {
        if(ShortName) {
          Targets <- paste0(timeAggss, "_LAG_", WindowingLag, "_", Targets)
        } else {
          Targets <- paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", Targets)
        }
      }

      # MA stats ----
      if(any(tolower(statsFUNs) %chin% "mean") && !all(periods %in% c(0L, 1L))) {
        periods <- periods[periods > 1L]
        MA_Names <- c()
        for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names, paste0("Mean_", periods[j],"_", t))
        data[, paste0(MA_Names) := data.table::frollmean(
          x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE),
          by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # SD stats ----
      if(any(tolower(statsFUNs) %chin% c("sd")) && !all(SDperiods %in% c(0L,1L))) {
        tempperiods <- SDperiods[SDperiods > 1L]
        SD_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names, paste0("SD_", tempperiods[j], "_", t))
        data[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Skewness stats ----
      if(any(tolower(statsFUNs) %chin% c("skew")) && !all(Skewperiods %in% c(0L,1L,2L))) {
        tempperiods <- Skewperiods[Skewperiods > 2L]
        Skew_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names, paste0("Skew_", tempperiods[j], "_", t))
        data[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = Targets]
      }

      # Kurtosis stats ----
      if(any(tolower(statsFUNs) %chin% c("kurt")) && !all(Kurtperiods %in% c(0L,1L,2L,3L,4L))) {
        tempperiods <- Kurtperiods[Kurtperiods > 3L]
        Kurt_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names, paste0("Kurt_", tempperiods[j], "_", t))
        data[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Quantiles ----
      if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
        tempperiods <- Quantileperiods[Quantileperiods > 4L]
        for(z in c(seq(5L,95L,5L))) {
          if(any(paste0("q",z) %chin% statsFUNs)) {
            Names <- c()
            for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names, paste0("Q_", z, "_", tempperiods[j], "_", t))
            data[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
          }
        }
      }
    }

    # Impute missing values ----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!! ----
    return(data)

  } else {

    # Sort data
    if(tolower(Type) == "lag") {
      data.table::setorderv(data, c(sortDateName[1L]), order = 1L)
    } else {
      data.table::setorderv(data, c(sortDateName[1L]), order = -1L)
    }
    Targets <- targets

    # Lags ----
    LAG_Names <- c()
    for(t in Targets) LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", "LAG_", lags, "_", t))

    # Build features ----
    data[, eval(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), .SDcols = c(Targets)]

    # Define targets ----
    if(WindowingLag != 0L) {
      Targets <- paste0(timeAggss, "_", "LAG_", WindowingLag, "_", Targets)
    } else {
      Targets <- Targets
    }

    # MA stats ----
    if(any(tolower(statsFUNs) %chin% "mean") && !all(periods %in% c(0L, 1L))) {
      periods <- periods[periods > 1L]
      MA_Names <- c()
      for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names, paste0("Mean_", periods[j], "_", t))
      data[, paste0(MA_Names) := data.table::frollmean(x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE), .SDcols = c(Targets)]
    }

    # SD stats ----
    if(any(tolower(statsFUNs) %chin% c("sd")) && !all(SDperiods %in% c(0L,1L))) {
      tempperiods <- SDperiods[SDperiods > 1L]
      SD_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names, paste0("SD_", tempperiods[j], "_", t))
      data[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Skewness stats ----
    if(any(tolower(statsFUNs) %chin% c("skew")) && !all(Skewperiods %in% c(0L,1L,2L))) {
      tempperiods <- Skewperiods[Skewperiods > 2L]
      Skew_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names, paste0("Skew_", tempperiods[j], "_", t))
      data[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Kurtosis stats ----
    if(any(tolower(statsFUNs) %chin% c("kurt")) && !all(Kurtperiods %in% c(0L,1L,2L,3L))) {
      tempperiods <- Kurtperiods[Kurtperiods > 3L]
      Kurt_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names, paste0("Kurt_", tempperiods[j], "_", t))
      data[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Quantiles ----
    if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
      tempperiods <- Quantileperiods[Quantileperiods > 4L]
      for(z in c(seq(5L,95L,5L))) {
        if(any(paste0("q",z) %chin% statsFUNs)) {
          Names <- c()
          for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names, paste0("Q_", z, "_", tempperiods[j], "_", t))
          data[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), .SDcols = c(Targets)]
        }
      }
    }

    # Impute missing values ----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!! ----
    return(data)
  }
}

#' @title FullFactorialCatFeatures
#'
#' @description FullFactorialCatFeatures reverses the difference
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param GroupVars Character vector of categorical columns to fully interact
#' @param MaxCombin The max K in N choose K. If NULL, K will loop through 1 to length(GroupVars)
#' @param BottomsUp TRUE or FALSE. TRUE starts with the most comlex interaction to the main effects
#'
#' @noRd
FullFactorialCatFeatures <- function(GroupVars = NULL,
                                     MaxCombin = NULL,
                                     BottomsUp = TRUE) {

  if(is.null(MaxCombin)) {
    MaxCombin <- N <- length(GroupVars)
  } else {
    N <- MaxCombin
  }
  Categoricals <- c()

  # N choose 1 case
  for(j in seq_along(GroupVars)) Categoricals <- c(Categoricals,GroupVars[j])

  # N choose i for 2 <= i < N
  for(i in seq_len(N)[-1L]) {

    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    if(MaxCombin == length(GroupVars)) {
      if(i < N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = '_')
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = '_')
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    } else {
      if(i <= N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = '_')
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = '_')
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    }
  }

  # Order of output
  if(BottomsUp) return(rev(Categoricals)) else return(Categoricals)
}

# ----

# ----
