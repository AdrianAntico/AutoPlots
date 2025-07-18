% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotFunctions.R
\name{Plot.VariableImportance}
\alias{Plot.VariableImportance}
\title{Plot.VariableImportance}
\usage{
Plot.VariableImportance(
  dt = NULL,
  XVar = NULL,
  YVar = NULL,
  GroupVar = NULL,
  YVarTrans = "Identity",
  XVarTrans = "Identity",
  FacetRows = 1,
  FacetCols = 1,
  FacetLevels = NULL,
  AggMethod = "mean",
  Height = NULL,
  Width = NULL,
  Title = "Variable Importance Plot",
  ShowLabels = FALSE,
  Title.YAxis = NULL,
  Title.XAxis = NULL,
  EchartsTheme = "macarons",
  TimeLine = TRUE,
  TextColor = "white",
  title.fontSize = 22,
  title.fontWeight = "bold",
  title.textShadowColor = "#63aeff",
  title.textShadowBlur = 3,
  title.textShadowOffsetY = 1,
  title.textShadowOffsetX = -1,
  xaxis.fontSize = 14,
  yaxis.fontSize = 14,
  Debug = FALSE
)
}
\arguments{
\item{dt}{source data.table}

\item{XVar}{Column name of X-Axis variable. If NULL then ignored}

\item{YVar}{Column name of Y-Axis variable. If NULL then ignored}

\item{GroupVar}{Column name of Group Variable for distinct colored histograms by group levels}

\item{YVarTrans}{"Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"}

\item{XVarTrans}{"Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"}

\item{FacetRows}{Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows}

\item{FacetCols}{Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns}

\item{FacetLevels}{Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.}

\item{AggMethod}{Choose from 'mean', 'sum', 'sd', and 'median'}

\item{Height}{"400px"}

\item{Width}{"200px"}

\item{Title}{title}

\item{ShowLabels}{character}

\item{Title.YAxis}{character}

\item{Title.XAxis}{character}

\item{EchartsTheme}{"auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"}

\item{TimeLine}{logical}

\item{TextColor}{'darkblue'}

\item{title.fontSize}{22}

\item{title.fontWeight}{"bold"}

\item{title.textShadowColor}{'#63aeff'}

\item{title.textShadowBlur}{3}

\item{title.textShadowOffsetY}{1}

\item{title.textShadowOffsetX}{-1}

\item{xaxis.fontSize}{14}

\item{yaxis.fontSize}{14}

\item{Debug}{Debugging purposes}
}
\value{
plot
}
\description{
Generate variable importance plots
}
\seealso{
Other Model Evaluation: 
\code{\link{BinaryMetricsPlot}()},
\code{\link{Calibration.Box}()},
\code{\link{Calibration.Line}()},
\code{\link{ConfusionMatrix}()},
\code{\link{Gains}()},
\code{\link{Lift}()},
\code{\link{PartialDependence.Box}()},
\code{\link{PartialDependence.HeatMap}()},
\code{\link{PartialDependence.Line}()},
\code{\link{Plot.BinaryMetrics}()},
\code{\link{Plot.Calibration.Box}()},
\code{\link{Plot.Calibration.Line}()},
\code{\link{Plot.ConfusionMatrix}()},
\code{\link{Plot.Gains}()},
\code{\link{Plot.Lift}()},
\code{\link{Plot.PartialDependence.Box}()},
\code{\link{Plot.PartialDependence.HeatMap}()},
\code{\link{Plot.PartialDependence.Line}()},
\code{\link{Plot.ROC}()},
\code{\link{Plot.Residuals.Histogram}()},
\code{\link{Plot.Residuals.Scatter}()},
\code{\link{Plot.ShapImportance}()},
\code{\link{ROC}()},
\code{\link{Residuals.Histogram}()},
\code{\link{Residuals.Scatter}()},
\code{\link{ShapImportance}()},
\code{\link{VariableImportance}()}
}
\author{
Adrian Antico
}
\concept{Model Evaluation}
