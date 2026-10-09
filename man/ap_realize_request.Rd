\name{ap_realize_request}
\alias{ap_realize_request}
\title{Draw an authenticated realization request}
\usage{
ap_realize_request(request, task, file = NULL, encoding = NULL,
  width = 800L, height = 500L, res = 96L)
}
\arguments{
\item{request}{A CommunicationIntelligence \code{RealizationRequest}. A hash on a modified record is not accepted.}

\item{task}{The \code{TaskBinding} named by the request.}

\item{file}{PNG path. The default is a temporary file.}

\item{encoding}{Optional rendering instruction. \code{NULL} is an identity column from zero, one panel per quantity. Fields that change the estimand are refused, except an authorized log10 point.}

\item{width, height}{Device pixels, from 200 to 1200.}

\item{res}{One of 72, 96, 120, or 144.}
}
\value{
A list. \code{status} is \code{RENDERED}, \code{REFUSED}, or \code{UNRESOLVED_AUTHORITY}. \code{release_authorized} is always \code{FALSE}. A rendered file does not authorize publication. \code{geometry} is a \code{data.table} of admitted values, axis limits, and device coordinates for this PNG. Those coordinates are not a cross-machine pixel certificate.
}
\description{
Verify a realization request by replaying CommunicationIntelligence, then draw the admitted numbers. The PNG subtitle shows the unit, the source revision, and that the figure is not an approval. AutoPlots does not estimate, refit, approve, or authorize publication.
}
\details{
The supported geoms are \code{column}, \code{point}, and \code{interval}. Interval ids are \code{<stem>_lower}, \code{<stem>_upper}, and an optional \code{<stem>} point. Without the point, the title is the admitted endpoint range, not either endpoint alone. Quantities that are not part of the interval are refused, as are endpoints that do not share a unit. A requested interval kind must already occur in the admitted uncertainty wording.

A log10 point requires a receipt with \code{status = "MET"} and \code{subject_binding} equal to the CommunicationIntelligence binding of \code{list(scale = "log10", quantity_ids = <ids in plot order>, values = <admitted numbers in plot order>)}. Without that receipt the status is \code{UNRESOLVED_AUTHORITY}. The title keeps the admitted number. The axis label and subtitle say that the scale is log10.
}
\examples{
refused <- ap_realize_request(NULL, NULL)
stopifnot(identical(refused$status, "REFUSED"))
stopifnot(isFALSE(refused$release_authorized))
}
