% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotFunctions_NEW.R
\name{ProbabilityPlot}
\alias{ProbabilityPlot}
\title{ProbabilityPlot}
\usage{
ProbabilityPlot(
  dt = NULL,
  SampleSize = 1000L,
  YVar = NULL,
  YVarTrans = "Identity",
  Height = NULL,
  Width = NULL,
  ShowLabels = FALSE,
  Theme = "dark",
  TextColor = "white",
  ContainLabel = TRUE,
  title.text = "Probability Plot",
  title.subtext = NULL,
  title.link = NULL,
  title.sublink = NULL,
  title.Align = NULL,
  title.top = NULL,
  title.left = NULL,
  title.right = NULL,
  title.bottom = NULL,
  title.padding = NULL,
  title.itemGap = NULL,
  title.backgroundColor = NULL,
  title.borderColor = NULL,
  title.borderWidth = NULL,
  title.borderRadius = NULL,
  title.shadowColor = NULL,
  title.shadowBlur = NULL,
  title.shadowOffsetX = NULL,
  title.shadowOffsetY = NULL,
  title.textStyle.color = NULL,
  title.textStyle.fontStyle = NULL,
  title.textStyle.fontWeight = NULL,
  title.textStyle.fontFamily = "Segoe UI",
  title.textStyle.fontSize = NULL,
  title.textStyle.lineHeight = NULL,
  title.textStyle.width = NULL,
  title.textStyle.height = NULL,
  title.textStyle.textBorderColor = NULL,
  title.textStyle.textBorderWidth = NULL,
  title.textStyle.textBorderType = NULL,
  title.textStyle.textBorderDashOffset = NULL,
  title.textStyle.textShadowColor = NULL,
  title.textStyle.textShadowBlur = NULL,
  title.textStyle.textShadowOffsetX = NULL,
  title.textStyle.textShadowOffsetY = NULL,
  title.subtextStyle.color = NULL,
  title.subtextStyle.align = NULL,
  title.subtextStyle.fontStyle = NULL,
  title.subtextStyle.fontWeight = NULL,
  title.subtextStyle.fontFamily = "Segoe UI",
  title.subtextStyle.fontSize = NULL,
  title.subtextStyle.lineHeight = NULL,
  title.subtextStyle.width = NULL,
  title.subtextStyle.height = NULL,
  title.subtextStyle.textBorderColor = NULL,
  title.subtextStyle.textBorderWidth = NULL,
  title.subtextStyle.textBorderType = NULL,
  title.subtextStyle.textBorderDashOffset = NULL,
  title.subtextStyle.textShadowColor = NULL,
  title.subtextStyle.textShadowBlur = NULL,
  title.subtextStyle.textShadowOffsetX = NULL,
  title.subtextStyle.textShadowOffsetY = NULL,
  Debug = FALSE
)
}
\arguments{
\item{dt}{source data.table}

\item{SampleSize}{An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored}

\item{YVar}{Y-Axis variable name}

\item{YVarTrans}{"Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"}

\item{Height}{"400px"}

\item{Width}{"200px"}

\item{ShowLabels}{character}

\item{Theme}{"macaron"}

\item{TextColor}{'darkblue'}

\item{ContainLabel}{Default TRUE}

\item{title.text}{Title name}

\item{title.subtext}{Subtitle name}

\item{title.link}{Title as a link}

\item{title.sublink}{Subtitle as a link}

\item{title.Align}{'auto' 'left' 'right' 'center'}

\item{title.top}{'auto' '20' 'top' 'middle' 'bottom'}

\item{title.left}{distance between title and left side of container}

\item{title.right}{distance between title and right side of container}

\item{title.bottom}{'auto' '20' 'top' 'middle' 'bottom'}

\item{title.padding}{numeric}

\item{title.itemGap}{space between title and subtitle}

\item{title.backgroundColor}{hex or name}

\item{title.borderColor}{hex or name}

\item{title.borderWidth}{numeric}

\item{title.borderRadius}{numeric}

\item{title.shadowColor}{hex or name}

\item{title.shadowBlur}{numeric}

\item{title.shadowOffsetX}{numeric}

\item{title.shadowOffsetY}{numeric}

\item{title.textStyle.color}{hex or name}

\item{title.textStyle.fontStyle}{'normal' 'italic' 'oblique'}

\item{title.textStyle.fontWeight}{'normal' 'bold' 'bolder' 'lighter'}

\item{title.textStyle.fontFamily}{'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'}

\item{title.textStyle.fontSize}{numeric}

\item{title.textStyle.lineHeight}{numeric}

\item{title.textStyle.width}{numeric}

\item{title.textStyle.height}{numeric}

\item{title.textStyle.textBorderColor}{hex or name}

\item{title.textStyle.textBorderWidth}{numeric}

\item{title.textStyle.textBorderType}{'solid' 'dashed' 'dotted'}

\item{title.textStyle.textBorderDashOffset}{numeric}

\item{title.textStyle.textShadowColor}{hex or name}

\item{title.textStyle.textShadowBlur}{numeric}

\item{title.textStyle.textShadowOffsetX}{numeric}

\item{title.textStyle.textShadowOffsetY}{numeric}

\item{title.subtextStyle.color}{hex or name}

\item{title.subtextStyle.align}{'auto' 'left' 'right' 'center'}

\item{title.subtextStyle.fontStyle}{'normal' 'italic' 'oblique'}

\item{title.subtextStyle.fontWeight}{'normal' 'bold' 'bolder' 'lighter'}

\item{title.subtextStyle.fontFamily}{'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'}

\item{title.subtextStyle.fontSize}{numeric}

\item{title.subtextStyle.lineHeight}{numeric}

\item{title.subtextStyle.width}{numeric}

\item{title.subtextStyle.height}{numeric}

\item{title.subtextStyle.textBorderColor}{hex or name}

\item{title.subtextStyle.textBorderWidth}{numeric}

\item{title.subtextStyle.textBorderType}{'solid' 'dashed' 'dotted'}

\item{title.subtextStyle.textBorderDashOffset}{numeric}

\item{title.subtextStyle.textShadowColor}{numeric}

\item{title.subtextStyle.textShadowBlur}{numeric}

\item{title.subtextStyle.textShadowOffsetX}{numeric}

\item{title.subtextStyle.textShadowOffsetY}{numeric}

\item{Debug}{Debugging purposes}
}
\value{
plot
}
\description{
Build a normal probability plot
}
\examples{
# Create fake data
dt <- data.table::data.table(Y = qnorm(p = runif(10000)))

# Create plot
AutoPlots::ProbabilityPlot(
  dt = dt,
  SampleSize = 1000L,
  YVar = "Y",
  YVarTrans = "Identity",
  Height = NULL,
  Width = NULL,
  ShowLabels = FALSE,
  Theme = "blue",
  TextColor = "black",
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
\code{\link{Plot.Rosetype}()},
\code{\link{Plot.Scatter3D}()},
\code{\link{Plot.Scatter}()},
\code{\link{Plot.StackedBar}()},
\code{\link{Plot.Step}()},
\code{\link{Plot.WordCloud}()},
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
