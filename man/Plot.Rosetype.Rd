% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotFunctions.R
\name{Plot.Rosetype}
\alias{Plot.Rosetype}
\title{Plot.Rosetype}
\usage{
Plot.Rosetype(
  dt = NULL,
  PreAgg = FALSE,
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
  Title = "Donut Plot",
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

\item{PreAgg}{logical}

\item{XVar}{X-Axis variable name}

\item{YVar}{Y-Axis variable name}

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

\item{EchartsTheme}{"auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo","essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired","jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal","sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"}

\item{TimeLine}{logical}

\item{TextColor}{'darkblue'}

\item{title.fontSize}{Defaults to size 22. Numeric. This changes the size of the title.}

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
Build a donut plot by simply passing arguments to a single function
}
\examples{
# Create fake data
dt <- data.table::data.table(Y = qnorm(p = stats::runif(10000)), GV = sample(LETTERS, 1000, TRUE))

# Create plot
AutoPlots::Plot.Rosetype(
  dt = dt,
  PreAgg = FALSE,
  XVar = "GV",
  YVar = "Y",
  GroupVar = NULL,
  YVarTrans = "Identity",
  XVarTrans = "Identity",
  FacetRows = 1,
  FacetCols = 1,
  FacetLevels = NULL,
  AggMethod = 'mean',
  Height = NULL,
  Width = NULL,
  Title = 'Pie Chart',
  ShowLabels = FALSE,
  Title.YAxis = NULL,
  Title.XAxis = NULL,
  EchartsTheme = "macarons",
  TimeLine = TRUE,
  TextColor = "black",
  title.fontSize = 22,
  title.fontWeight = "bold",
  title.textShadowColor = '#63aeff',
  title.textShadowBlur = 3,
  title.textShadowOffsetY = 1,
  title.textShadowOffsetX = -1,
  xaxis.fontSize = 14,
  yaxis.fontSize = 14,
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
\code{\link{Plot.Copula}()},
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
