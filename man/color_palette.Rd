% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_palette.R
\name{color_palette}
\alias{color_palette}
\title{Color palette}
\usage{
color_palette(x, n = NULL)
}
\arguments{
\item{x}{\code{character} name of the color palette
(see Details for available options).}

\item{n}{\code{integer} number of colors required.
A \code{NULL} argument can also be used to obtain a default number of colors.
If a \code{NULL} argument is supplied and the palette has a limited number of
colors, then the maximum number of colors is returned.
Otherwise, if a \code{NULL} argument is supplied and the palette does not have
a limited number of colors, then a 20 colors are returned.
The default argument is \code{NULL}.}
}
\description{
Generate colors using the name of a color palette.
}
\details{
This function can be used to generate colors using palettes provided by
the \pkg{RColorBrewer} and \pkg{viridisLite} packages. If colors are
from multiple palettes, they can be supplied as a single \code{character}
object delimited by semicolon (\code{";"}) characters.

Specifically, available palettes include:
\itemize{
\item \code{"viridis"} (see \link[viridisLite:viridis]{viridisLite::viridis})
\item \code{"magma"} (see \link[viridisLite:viridis]{viridisLite::magma})
\item \code{"plasma"} (see \link[viridisLite:viridis]{viridisLite::plasma})
\item \code{"inferno"} (see \link[viridisLite:viridis]{viridisLite::inferno})
\item \code{"cividis"} (see \link[viridisLite:viridis]{viridisLite::cividis})
\item \code{"mako"} (see \link[viridisLite:viridis]{viridisLite::mako})
\item \code{"rocket"} (see \link[viridisLite:viridis]{viridisLite::rocket})
\item \code{"turbo"} (see \link[viridisLite:viridis]{viridisLite::turbo})
\item \verb{"BrBG}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"PiYG}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"PRGn}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"PuOr}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"RdBu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"RdGy}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"RdYlBu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"RdYlGn}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Spectral}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Accent}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Dark2}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Paired}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Pastel1}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Pastel2}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Set1}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Set2}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Set3}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Blues}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"BuGn}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"BuPu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"GnBu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Greens}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Greys}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Oranges}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"OrRd}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"PuBu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"PuBuGn}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"PuRd}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Purples}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"RdPu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"Reds}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"YlGn}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"YlGnBu}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"YlOrBr}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \verb{"YlOrRd}" (see \link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal})
\item \code{"random"} (the above palettes are randomly used to generate colors)
}
}
\examples{
# obtain 5 colors from the Greens palette
color_palette("Greens", 5)

# obtain all colors from the Greens palette
color_palette("Greens", NULL)

# obtain 15 colors using the Set1 and Pastel1 palettes
color_palette("Set1;Pastel1", 15)

# obtain colors from the plasma palette
color_palette("plasma", NULL)

# obtain colors from a random palette
color_palette("random", NULL)
}