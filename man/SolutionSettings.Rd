% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_SolutionSettings.R
\name{SolutionSettings}
\alias{SolutionSettings}
\title{Solution settings class}
\description{
Solution settings class

Solution settings class
}
\details{
Definition for the \code{SolutionSettings} class.

\code{user_settings} stores user uploaded .yaml file used to repopulate solution
settings from a previous optimization run. The user uploaded .yaml file
must completely match the current project.

The argument to \code{value} should be a \code{list} with the following elements:
\describe{
\item{id}{\code{character} identifier for theme, weight, include, or exclude.}
\item{setting}{\code{character} name of parameter.
Available options are: \code{"status"}, \code{"factor"}, \code{"value"}, or \code{"goal"}.}
\item{type}{\code{character} indicating the type of setting.
Available options are: \code{"theme"}, \code{"weight"}, \code{"include"}, \code{"exclude"}
\code{"parameter"}.}
}

The argument to \code{value} should be a \code{list} with the following elements:
\describe{
\item{id}{\code{character} identifier for theme, weight, include, exclude or parameter.}
\item{setting}{\code{character} name of parameter.
Available options are: \code{"status"}, \code{"factor"}, \code{"value"}, \code{"goal"}, or \code{"fileinput"}}
\item{value}{\code{numeric}, \code{logical}, or \code{character} value for new setting.}
\item{type}{\code{character} indicating the type of setting.
Available options are: \code{"theme"}, \code{"weight"}, \code{"include"}, \code{"exclude"},
\code{"parameter"}.}
}

The argument to \code{value} should be a \code{list} of \link{Theme}, \link{Weight},
\link{Include}, \link{Exclude} and \link{Parameter} objects.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{theme_ids}}{\code{character} vector of identifiers for the themes.}

\item{\code{weight_ids}}{\code{character} vector of identifiers for the weights.}

\item{\code{include_ids}}{\code{character} vector of identifiers for the includes.}

\item{\code{exclude_ids}}{\code{character} vector of identifiers for the excludes.}

\item{\code{parameter_ids}}{\code{character} vector of identifiers for the
parameters.}

\item{\code{themes}}{\code{list} of \link{Theme} objects.}

\item{\code{weights}}{\code{list} of \link{Weight} objects.}

\item{\code{includes}}{\code{list} of \link{Include} objects.}

\item{\code{excludes}}{\code{list} of \link{Exclude} objects.}

\item{\code{parameters}}{\code{list} of \link{Parameter} objects.}

\item{\code{user_settings}}{\code{list} of \link{Theme}, \link{Weight}, \link{Include}, \link{Exclude}
and \link{Parameter} objects.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SolutionSettings-new}{\code{SolutionSettings$new()}}
\item \href{#method-SolutionSettings-print}{\code{SolutionSettings$print()}}
\item \href{#method-SolutionSettings-repr}{\code{SolutionSettings$repr()}}
\item \href{#method-SolutionSettings-get_theme}{\code{SolutionSettings$get_theme()}}
\item \href{#method-SolutionSettings-get_weight}{\code{SolutionSettings$get_weight()}}
\item \href{#method-SolutionSettings-get_include}{\code{SolutionSettings$get_include()}}
\item \href{#method-SolutionSettings-get_exclude}{\code{SolutionSettings$get_exclude()}}
\item \href{#method-SolutionSettings-get_parameter}{\code{SolutionSettings$get_parameter()}}
\item \href{#method-SolutionSettings-get_setting}{\code{SolutionSettings$get_setting()}}
\item \href{#method-SolutionSettings-set_setting}{\code{SolutionSettings$set_setting()}}
\item \href{#method-SolutionSettings-get_feature_names}{\code{SolutionSettings$get_feature_names()}}
\item \href{#method-SolutionSettings-update_ss}{\code{SolutionSettings$update_ss()}}
\item \href{#method-SolutionSettings-get_widget_data}{\code{SolutionSettings$get_widget_data()}}
\item \href{#method-SolutionSettings-get_theme_settings}{\code{SolutionSettings$get_theme_settings()}}
\item \href{#method-SolutionSettings-get_weight_settings}{\code{SolutionSettings$get_weight_settings()}}
\item \href{#method-SolutionSettings-get_include_settings}{\code{SolutionSettings$get_include_settings()}}
\item \href{#method-SolutionSettings-get_exclude_settings}{\code{SolutionSettings$get_exclude_settings()}}
\item \href{#method-SolutionSettings-get_parameter_settings}{\code{SolutionSettings$get_parameter_settings()}}
\item \href{#method-SolutionSettings-get_theme_data}{\code{SolutionSettings$get_theme_data()}}
\item \href{#method-SolutionSettings-get_weight_data}{\code{SolutionSettings$get_weight_data()}}
\item \href{#method-SolutionSettings-get_include_data}{\code{SolutionSettings$get_include_data()}}
\item \href{#method-SolutionSettings-get_exclude_data}{\code{SolutionSettings$get_exclude_data()}}
\item \href{#method-SolutionSettings-get_overlap}{\code{SolutionSettings$get_overlap()}}
\item \href{#method-SolutionSettings-set_overlap}{\code{SolutionSettings$set_overlap()}}
\item \href{#method-SolutionSettings-update_current_held}{\code{SolutionSettings$update_current_held()}}
\item \href{#method-SolutionSettings-clone}{\code{SolutionSettings$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-new"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-new}{}}}
\subsection{Method \code{new()}}{
Create a \code{SolutionSettings} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$new(themes, weights, includes, excludes, parameters)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{themes}}{\code{list} of \link{Theme} objects.}

\item{\code{weights}}{\code{list} of \link{Weight} objects.}

\item{\code{includes}}{\code{list} of \link{Include} objects.}

\item{\code{excludes}}{\code{list} of \link{Exclude} objects.}

\item{\code{parameters}}{\code{list} of \link{Parameter} objects.}

\item{\code{user_settings}}{\code{list} of \code{list} of \link{Theme}, \link{Weight}, \link{Include},
\link{Exclude} and \link{Parameter} objects (see Details section).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{SolutionSettings} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-print"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-print}{}}}
\subsection{Method \code{print()}}{
Print the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{not used.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-repr"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-repr}{}}}
\subsection{Method \code{repr()}}{
Generate a \code{character} summarizing the representation of the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$repr()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{character} value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_theme"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_theme}{}}}
\subsection{Method \code{get_theme()}}{
Get a theme.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_theme(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{character} theme identifier.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\link{Theme} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_weight"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_weight}{}}}
\subsection{Method \code{get_weight()}}{
Get a weight.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_weight(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{character} weight identifier.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\link{Weight} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_include"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_include}{}}}
\subsection{Method \code{get_include()}}{
Get an include.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_include(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{character} include identifier.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\link{Include} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_exclude"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_exclude}{}}}
\subsection{Method \code{get_exclude()}}{
Get an exclude.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_exclude(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{character} exclude identifier.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\link{Exclude} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_parameter"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_parameter}{}}}
\subsection{Method \code{get_parameter()}}{
Get an parameter.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_parameter(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{character} weight identifier.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\link{Parameter} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_setting"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_setting}{}}}
\subsection{Method \code{get_setting()}}{
Get a setting for a weight, theme, include, exclude, or parameter.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_setting(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{list} with new parameter information (see Details section)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-set_setting"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-set_setting}{}}}
\subsection{Method \code{set_setting()}}{
Set a setting for theme, weight, include, exclude and parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$set_setting(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{list} with new setting information (see Details section)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_feature_names"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_feature_names}{}}}
\subsection{Method \code{get_feature_names()}}{
get character vector of all feature names from a list of themes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_feature_names(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{\code{list} of \link{Theme} objects.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{vector} of feature names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-update_ss"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-update_ss}{}}}
\subsection{Method \code{update_ss()}}{
update settings for theme, weight, include, exclude and parameters from
user uploaded configuration file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$update_ss(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{\code{list} with new setting information (see Details section)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_widget_data"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_widget_data}{}}}
\subsection{Method \code{get_widget_data()}}{
Get data for displaying the theme in a \code{\link[=solutionSettings]{solutionSettings()}} widget.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_widget_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{list} with widget data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_theme_settings"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_theme_settings}{}}}
\subsection{Method \code{get_theme_settings()}}{
Get theme settings for generating a prioritization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_theme_settings()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[tibble:tibble]{tibble::tibble()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_weight_settings"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_weight_settings}{}}}
\subsection{Method \code{get_weight_settings()}}{
Get weight settings for generating a prioritization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_weight_settings()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[tibble:tibble]{tibble::tibble()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_include_settings"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_include_settings}{}}}
\subsection{Method \code{get_include_settings()}}{
Get include settings for generating a prioritization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_include_settings()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[tibble:tibble]{tibble::tibble()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_exclude_settings"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_exclude_settings}{}}}
\subsection{Method \code{get_exclude_settings()}}{
Get exclude settings for generating a prioritization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_exclude_settings()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[tibble:tibble]{tibble::tibble()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_parameter_settings"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_parameter_settings}{}}}
\subsection{Method \code{get_parameter_settings()}}{
Get parameter settings for generating a prioritization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_parameter_settings()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[tibble:tibble]{tibble::tibble()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_theme_data"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_theme_data}{}}}
\subsection{Method \code{get_theme_data()}}{
Get theme matrix data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_theme_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[Matrix:sparseMatrix]{Matrix::sparseMatrix()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_weight_data"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_weight_data}{}}}
\subsection{Method \code{get_weight_data()}}{
Get weight matrix data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_weight_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[Matrix:sparseMatrix]{Matrix::sparseMatrix()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_include_data"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_include_data}{}}}
\subsection{Method \code{get_include_data()}}{
Get includes matrix data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_include_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[Matrix:sparseMatrix]{Matrix::sparseMatrix()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_exclude_data"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_exclude_data}{}}}
\subsection{Method \code{get_exclude_data()}}{
Get exclude matrix data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_exclude_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{\link[Matrix:sparseMatrix]{Matrix::sparseMatrix()}} with data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-get_overlap"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-get_overlap}{}}}
\subsection{Method \code{get_overlap()}}{
Get list of include and exclude names that overlap.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$get_overlap()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{list} with exclude and include names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-set_overlap"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-set_overlap}{}}}
\subsection{Method \code{set_overlap()}}{
Get list of include and exclude names that overlap.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$set_overlap()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{list} with exclude and include names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-update_current_held"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-update_current_held}{}}}
\subsection{Method \code{update_current_held()}}{
Update the current amount held for each themes and weights automatically
based on the include and exclude statuses.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$update_current_held(
  theme_data = self$get_theme_data(),
  weight_data = self$get_weight_data(),
  include_data = self$get_include_data(),
  exclude_data = self$get_exclude_data()
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{theme_data}}{\verb{[Matrix::sparseMatrix()] with theme data. Defaults to }self$get_theme_data()`.}

\item{\code{weight_data}}{\verb{[Matrix::sparseMatrix()] with weight data. Defaults to }self$get_weight_data()`.}

\item{\code{include_data}}{\verb{[Matrix::sparseMatrix()] with include data. Defaults to }self$get_include_data()`.}

\item{\code{exclude_data}}{\verb{[Matrix::sparseMatrix()] with exclude data. Defaults to }self$get_exclude_data()`.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolutionSettings-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SolutionSettings-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolutionSettings$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
