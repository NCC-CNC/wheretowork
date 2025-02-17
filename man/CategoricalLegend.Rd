% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_CategoricalLegend.R
\name{CategoricalLegend}
\alias{CategoricalLegend}
\title{Categorical legend class}
\description{
Categorical legend class

Categorical legend class
}
\details{
Definition for the \code{CategoricalLegend} class.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{values}}{\code{numeric} vector.}

\item{\code{colors}}{\code{character} vector.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CategoricalLegend-new}{\code{CategoricalLegend$new()}}
\item \href{#method-CategoricalLegend-get_widget_data}{\code{CategoricalLegend$get_widget_data()}}
\item \href{#method-CategoricalLegend-get_resample_method}{\code{CategoricalLegend$get_resample_method()}}
\item \href{#method-CategoricalLegend-get_color_map}{\code{CategoricalLegend$get_color_map()}}
\item \href{#method-CategoricalLegend-export}{\code{CategoricalLegend$export()}}
\item \href{#method-CategoricalLegend-clone}{\code{CategoricalLegend$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CategoricalLegend-new"></a>}}
\if{latex}{\out{\hypertarget{method-CategoricalLegend-new}{}}}
\subsection{Method \code{new()}}{
Create a \code{CategoricalLegend} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CategoricalLegend$new(values, colors)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{values}}{\code{numeric} value.}

\item{\code{colors}}{\code{character} vector of colors.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{CategoricalLegend} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CategoricalLegend-get_widget_data"></a>}}
\if{latex}{\out{\hypertarget{method-CategoricalLegend-get_widget_data}{}}}
\subsection{Method \code{get_widget_data()}}{
Get data for creating a widget.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CategoricalLegend$get_widget_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A new \code{CategoricalLegend} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CategoricalLegend-get_resample_method"></a>}}
\if{latex}{\out{\hypertarget{method-CategoricalLegend-get_resample_method}{}}}
\subsection{Method \code{get_resample_method()}}{
Get resample method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CategoricalLegend$get_resample_method()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{character} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CategoricalLegend-get_color_map"></a>}}
\if{latex}{\out{\hypertarget{method-CategoricalLegend-get_color_map}{}}}
\subsection{Method \code{get_color_map()}}{
Get a function for mapping values to colors.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CategoricalLegend$get_color_map()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \code{function} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CategoricalLegend-export"></a>}}
\if{latex}{\out{\hypertarget{method-CategoricalLegend-export}{}}}
\subsection{Method \code{export()}}{
Export settings
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CategoricalLegend$export()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{list} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CategoricalLegend-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CategoricalLegend-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CategoricalLegend$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
