% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revised_echarts4r_functions.R
\name{display_plots_grid}
\alias{display_plots_grid}
\title{Display a Series of Plots in a Styled HTML Grid with Columns}
\usage{
display_plots_grid(
  plots,
  cols = NULL,
  container_class = "plot-card",
  grid_class = "plot-grid"
)
}
\arguments{
\item{plots}{A list of echarts4r plots (or htmlwidgets).}

\item{cols}{Number of columns.}

\item{container_class}{CSS class for each plot container.}

\item{grid_class}{CSS class for the grid layout container.}
}
\value{
A browsable HTML grid for use in Rmarkdown, Shiny, or viewer pane.
}
\description{
Display a Series of Plots in a Styled HTML Grid with Columns
}
