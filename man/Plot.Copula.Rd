% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotFunctions.R
\name{Plot.Copula}
\alias{Plot.Copula}
\title{Plot.Copula}
\usage{
Plot.Copula(
  dt = NULL,
  SampleSize = 30000L,
  XVar = NULL,
  YVar = NULL,
  GroupVar = NULL,
  YVarTrans = "Identity",
  XVarTrans = "Identity",
  FacetRows = 1,
  FacetCols = 1,
  FacetLevels = NULL,
  Height = NULL,
  Width = NULL,
  Title = "Copula Plot",
  ShowLabels = FALSE,
  AddGLM = FALSE,
  Title.YAxis = NULL,
  Title.XAxis = NULL,
  EchartsTheme = "macarons",
  MouseScroll = TRUE,
  TimeLine = FALSE,
  TextColor = "white",
  yaxis.fontSize = 14,
  xaxis.fontSize = 14,
  title.fontSize = 22,
  title.fontWeight = "bold",
  title.textShadowColor = "#63aeff",
  title.textShadowBlur = 3,
  title.textShadowOffsetY = 1,
  title.textShadowOffsetX = -1,
  xaxis.rotate = 0,
  yaxis.rotate = 0,
  ContainLabel = TRUE,
  Debug = FALSE
)
}
\arguments{
\item{dt}{source data.table}

\item{SampleSize}{An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored}

\item{XVar}{X-Axis variable name}

\item{YVar}{Y-Axis variable name}

\item{GroupVar}{Requires an XVar and YVar already be defined}

\item{YVarTrans}{"Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"}

\item{XVarTrans}{"Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"}

\item{FacetRows}{Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows}

\item{FacetCols}{Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns}

\item{FacetLevels}{Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.}

\item{Height}{"400px"}

\item{Width}{"200px"}

\item{Title}{'Copula Plot'}

\item{ShowLabels}{character}

\item{AddGLM}{logical}

\item{Title.YAxis}{character}

\item{Title.XAxis}{character}

\item{EchartsTheme}{= "dark-blue",}

\item{MouseScroll}{logical, zoom via mouse scroll}

\item{TimeLine}{Logical}

\item{TextColor}{'darkblue'}

\item{yaxis.fontSize}{14}

\item{xaxis.fontSize}{14}

\item{title.fontSize}{22}

\item{title.fontWeight}{"bold"}

\item{title.textShadowColor}{'#63aeff'}

\item{title.textShadowBlur}{3}

\item{title.textShadowOffsetY}{1}

\item{title.textShadowOffsetX}{-1}

\item{xaxis.rotate}{0}

\item{yaxis.rotate}{0}

\item{ContainLabel}{TRUE}

\item{Debug}{Debugging purposes}
}
\value{
plot
}
\description{
Build a copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
}
\examples{
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Copula Plot Chart
AutoPlots::Plot.Copula(
  dt = data,
  SampleSize = 10000,
  XVar = "Independent_Variable8",
  YVar = "Adrian",
  GroupVar = NULL,
  YVarTrans = "Identity",
  XVarTrans = "Identity",
  FacetRows = 1,
  FacetCols = 1,
  FacetLevels = NULL,
  Height = NULL,
  Width = NULL,
  Title = 'Copula Plot',
  ShowLabels = FALSE,
  AddGLM = FALSE,
  Title.YAxis = NULL,
  Title.XAxis = NULL,
  EchartsTheme = "macarons",
  MouseScroll = TRUE,
  TimeLine = FALSE,
  TextColor = "black",
  yaxis.fontSize = 14,
  xaxis.fontSize = 14,
  title.fontSize = 22,
  title.fontWeight = "bold",
  title.textShadowColor = '#63aeff',
  title.textShadowBlur = 3,
  title.textShadowOffsetY = 1,
  title.textShadowOffsetX = -1,
  xaxis.rotate = 0,
  yaxis.rotate = 0,
  ContainLabel = TRUE,
  Debug = FALSE)

}
\seealso{
Other Standard Plots: 
\code{\link{ACF}()},
\code{\link{Area}()},
\code{\link{BarPlot3D}()},
\code{\link{Bar}()},
\code{\link{Box}()},
\code{\link{Copula3D}()},
\code{\link{Copula}()},
\code{\link{CorrMatrix}()},
\code{\link{Density}()},
\code{\link{Donut}()},
\code{\link{HeatMap}()},
\code{\link{Histogram}()},
\code{\link{Line}()},
\code{\link{PACF}()},
\code{\link{Parallel}()},
\code{\link{Pie}()},
\code{\link{Plot.ACF}()},
\code{\link{Plot.Area}()},
\code{\link{Plot.BarPlot3D}()},
\code{\link{Plot.Bar}()},
\code{\link{Plot.Box}()},
\code{\link{Plot.Copula3D}()},
\code{\link{Plot.CorrMatrix}()},
\code{\link{Plot.Density}()},
\code{\link{Plot.Donut}()},
\code{\link{Plot.HeatMap}()},
\code{\link{Plot.Histogram}()},
\code{\link{Plot.Line}()},
\code{\link{Plot.PACF}()},
\code{\link{Plot.Parallel}()},
\code{\link{Plot.Pie}()},
\code{\link{Plot.ProbabilityPlot}()},
\code{\link{Plot.Radar}()},
\code{\link{Plot.River}()},
\code{\link{Plot.Rosetype}()},
\code{\link{Plot.Scatter3D}()},
\code{\link{Plot.Scatter}()},
\code{\link{Plot.StackedBar}()},
\code{\link{Plot.Step}()},
\code{\link{Plot.WordCloud}()},
\code{\link{ProbabilityPlot}()},
\code{\link{Radar}()},
\code{\link{River}()},
\code{\link{Rosetype}()},
\code{\link{Scatter3D}()},
\code{\link{Scatter}()},
\code{\link{StackedBar}()},
\code{\link{Step}()},
\code{\link{WordCloud}()}
}
\author{
Adrian Antico
}
\concept{Standard Plots}
