% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revised_echarts4r_functions.R
\name{e_bar_full}
\alias{e_bar_full}
\title{Enhanced area Setter for echarts4r}
\usage{
e_bar_full(
  e = NULL,
  serie = NULL,
  label = FALSE,
  backgroundStyle.color = NULL,
  backgroundStyle.opacity = NULL
)
}
\arguments{
\item{e}{plot object}

\item{serie}{Variable}

\item{label}{Logical}

\item{backgroundStyle.color}{Fill color. Can be a single color or vector of multiple colors for gradient.}

\item{backgroundStyle.opacity}{transparency}
}
\value{
The modified echarts4r object
}
\description{
Exposes every area* option so you don't have to hand-craft the JSON.
}
