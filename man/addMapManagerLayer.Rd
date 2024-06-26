% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widget_mapManager_server.R
\name{addMapManagerLayer}
\alias{addMapManagerLayer}
\title{Add a layer in a map manager widget}
\usage{
addMapManagerLayer(session = shiny::getDefaultReactiveDomain(), inputId, value)
}
\arguments{
\item{session}{The \code{session} object passed to function given to
\code{shinyServer} Default is \code{\link[shiny:domains]{shiny::getDefaultReactiveDomain()}}.}

\item{inputId}{\code{character} The identifier of the input object.}

\item{value}{\link{Theme}, \link{Weight}, or \link{Solution} object.}
}
\description{
Add a new layer to a map manager widget on the client.
}
\seealso{
\code{\link[=mapManager]{mapManager()}}.
}
