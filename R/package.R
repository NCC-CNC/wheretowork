#' @include internal.R
NULL

#' @import htmlwidgets
#' @import R6
#' @import shiny
#' @import leaflet
#' @import leaflet.extras2
#' @import shinyBS
#' @import raster
#' @import sf
NULL

#' locationmisc: Classes, Widgets, and Functions for the Location App
#'
#' This package provides classes, widgets, and functions for the
#' [Location App](https://github.com/NCC-CNC/location-app).
#'
#' @name locationmisc
#' @docType package
NULL

# define global variables used in lazy evaluation
utils::globalVariables(
  c(
    "absolute_area",
    "absolute_perimeter",
    "statistics"
  )
)

# define functions for internally used packages to pass checks
tmp1 <- rgdal::readOGR
tmp2 <- R.utils::gzip
