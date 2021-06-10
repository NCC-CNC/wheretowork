% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solutionResultsSidebarPane.R
\name{solutionResultsSidebarPane}
\alias{solutionResultsSidebarPane}
\title{Solution results sidebar pane}
\usage{
solutionResultsSidebarPane(id, solutionResultsId = paste0(id, "_results"))
}
\arguments{
\item{id}{\code{character} identifier for the sidebar pane.}

\item{solutionResultsId}{\code{character} identifier for the
\code{\link[=solutionResults]{solutionResults()}} widget to create within the sidebar pane.
This widget is used to  display results for solutions.
Defaults to \code{paste0(id, "_results")}.}
}
\value{
A \code{shiny.tag} object with the sidebar pane.
}
\description{
Constructs a sidebar pane for displaying solution results.
This is designed to
be used as an argument to \link[leaflet.extras2:sidebar_tabs]{leaflet.extras2::sidebar_tabs} when specifying
the user interface for a Shiny web application.
}
\examples{
\dontrun{
# run Shiny app to demo the sidebar pane
if (interactive()) {
  runExample("solutionResultsSidebarPane")
}
}

}