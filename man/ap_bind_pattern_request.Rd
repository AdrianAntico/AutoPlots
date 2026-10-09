\name{ap_bind_pattern_request}
\alias{ap_bind_pattern_request}
\title{Ask CommunicationIntelligence for a plot request}
\usage{
ap_bind_pattern_request(inputs, task_id = "autoplots-pattern")
}
\arguments{
\item{inputs}{An admitted CommunicationIntelligence input set. This function does not invent owner values.}

\item{task_id}{Task id used for the host request.}
}
\value{
A list. \code{status = "REQUEST_READY"} means \code{request} and \code{task} can be passed to \code{\link{ap_realize_request}}. That status is not a drawing status. \code{release_authorized} is \code{FALSE}.
}
\description{
Build a pattern-inspection realization request through CommunicationIntelligence's public API. CommunicationIntelligence's text helpers do not call this function.
}
\examples{
\dontrun{
bound <- ap_bind_pattern_request(inputs, "figure-1")
figure <- ap_realize_request(bound$request, bound$task)
}
}
