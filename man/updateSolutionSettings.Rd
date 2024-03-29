% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widget_solutionSettings_server.R
\name{updateSolutionSettings}
\alias{updateSolutionSettings}
\title{Update solution settings widget}
\usage{
updateSolutionSettings(
  session = shiny::getDefaultReactiveDomain(),
  inputId,
  value
)
}
\arguments{
\item{session}{The \code{session} object passed to function given to
\code{shinyServer} Default is \code{\link[shiny:domains]{shiny::getDefaultReactiveDomain()}}.}

\item{inputId}{\code{character} The identifier of the input object.}

\item{value}{\code{list} Object containing the new settings (see Details).}
}
\description{
Change the settings for solution settings widget on the client.
Specifically, change the settings for a theme or weight depicted
in a solution settings widget.
}
\details{
\subsection{Overview}{

The argument to \code{value} should be \code{list} object containing
the information necessary to update a theme or weight.
Broadly speaking, it should contain the following elements:

\describe{
\item{id}{\code{character} value with the identifier for the theme or weight.}
\item{setting}{\code{character} value with the name of the setting to update.}
\item{value}{\code{ANY} new value for the setting.}
\item{type}{\code{character} value indicating if the setting is a \code{theme}
or \code{weight}.}
}

Note that the \code{value} element in the \code{list} object should have a
class (i.e. \code{numeric}, \verb{logical, or }character\verb{) that is relevant to the setting that should be updated. For example, if the }setting\verb{element is equal to}name\verb{, then the }value\verb{element should contain a}character` value.
For reference, we provide examples detailing all the various settings that
can be updated below.
}

\subsection{Themes}{

Here we detail all possible settings that can be updated for
themes.

\strong{Update the name.}
This controls the bold text shown in the header of the theme.

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"name"}}
\item{value}{\code{"SPECIES"}}
\item{type}{\code{"theme"}}
}

\strong{Update the status.}
This controls the large switch shown in the header of theme.
Note that the \code{value} element must be a \code{logical} (\code{TRUE} or \code{FALSE}) value.

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"status"}}
\item{value}{\code{TRUE}}
\item{type}{\code{"theme"}}
}

\strong{Update the view.}
This controls whether the "group" or "single" tab is active for themes with
multiple features.
It does not have any effect for themes with a single feature.
Note that the \code{value} element must be a \code{character} value equal to
\code{"group"} or \code{"single"}.

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"view"}}
\item{value}{\code{"group"}}
\item{type}{\code{"theme"}}
}

\strong{Update the group goal.}
This controls the slider present under the "group" tab.
It does not have any effect for themes with a single features.

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"group_goal"}}
\item{value}{\code{0.7}}
\item{type}{\code{"theme"}}
}

\strong{Update the feature status.}
This controls the switches present under the "single" tab.
To ensure compatibility between themes with a single feature
and themes with multiple features, this will update the status
for a theme with a single feature.
Note that the \code{value} element must have a value for each feature
within the theme (the example below assumes the theme has three features).

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"feature_status"}}
\item{value}{\code{c(TRUE, FALSE, TRUE)}}
\item{type}{\code{"theme"}}
}

\strong{Update the feature current values.}
This controls the bars indicating the amount currently held for each
feature under both the "single" and "group" tabs.
Note that the \code{value} element must have a value for each feature
within the theme (the example below assumes the theme has three features).

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"feature_current"}}
\item{value}{\code{c(0.3, 0.1, 0.7)}}
\item{type}{\code{"theme"}}
}

\strong{Update the feature goals.}
This controls the sliders present under the "single" tab.
To ensure compatibility between themes with a single feature
and themes with multiple features, this will update the status
for a theme with a single feature.
Note that the \code{value} element must have a value for each feature
within the theme (the example below assumes the theme has three features).

\describe{
\item{id}{\code{"THEMEID"}}
\item{setting}{\code{"feature_goal"}}
\item{value}{\code{c(0.3, 0.1, 0.7)}}
\item{type}{\code{"theme"}}
}
}

\subsection{Weights}{

Here we detail all possible settings that can be updated for
weights.

\strong{Update the name.}
This controls the bold text shown in the header of the weight.

\describe{
\item{id}{\code{"WEIGHTID"}}
\item{setting}{\code{"name"}}
\item{value}{\code{"SPECIES"}}
\item{type}{\code{"weight"}}
}

\strong{Update the status.}
This controls the large switch shown in the header of weight.
Note that the \code{value} element must be a \code{logical} (\code{TRUE} or \code{FALSE}) value.

\describe{
\item{id}{\code{"WEIGHTID"}}
\item{setting}{\code{"status"}}
\item{value}{\code{TRUE}}
\item{type}{\code{"weight"}}
}

\strong{Update the factor.}
This controls the slider shown for the weight.

\describe{
\item{id}{\code{"WEIGHTID"}}
\item{setting}{\code{"factor"}}
\item{value}{\code{0.1}}
\item{type}{\code{"weight"}}
}
}

\subsection{Includes}{

Here we detail all possible settings that can be updated for
includes.

\strong{Update the name.}
This controls the bold text shown in the header of the include.

\describe{
\item{id}{\code{"INCLUDEID"}}
\item{setting}{\code{"name"}}
\item{value}{\code{"PA"}}
\item{type}{\code{"include"}}
}

\strong{Update the status.}
This controls the large switch shown in the header of include.
Note that the \code{value} element must be a \code{logical} (\code{TRUE} or \code{FALSE}) value.

\describe{
\item{id}{\code{"INCLUDEID"}}
\item{setting}{\code{"status"}}
\item{value}{\code{TRUE}}
\item{type}{\code{"include"}}
}
}

\subsection{Excludes}{

Here we detail all possible settings that can be updated for
excludes.

\strong{Update the name.}
This controls the bold text shown in the header of the exclude.

\describe{
\item{id}{\code{"EXCLUDEID"}}
\item{setting}{\code{"name"}}
\item{value}{\code{"XX"}}
\item{type}{\code{"exclude"}}
}

\strong{Update the status.}
This controls the large switch shown in the header of exclude.
Note that the \code{value} element must be a \code{logical} (\code{TRUE} or \code{FALSE}) value.

\describe{
\item{id}{\code{"EXCLUDEID"}}
\item{setting}{\code{"status"}}
\item{value}{\code{TRUE}}
\item{type}{\code{"exclude"}}
}
}
}
\seealso{
\code{\link[=solutionSettings]{solutionSettings()}}.
}
