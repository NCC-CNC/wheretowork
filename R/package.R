#' @include internal.R
NULL

#' @import shinyBS
#' @import promises
#' @import R6
#' @import raster
#' @import sf
NULL

#' wheretowork: Systematic conservation planning application.
#'
#' TODO.
#'
#' @name wheretowork
#'
#' @docType package
#'
#' @examples
#' #TODO
NULL

# define global variables to pass package checks
## these variables are used in lazy evaluation or the shiny application
utils::globalVariables(
  c(
    "absolute_area",
    "absolute_perimeter",
    "statistics",
    "area_budget_parameter",
    "boundary_gap_parameter",
    "session",
    "app_data",
    "project_data",
    "import_data"
  )
)

# define functions for internally used packages to pass checks
tmp1 <- rgdal::readOGR
tmp2 <- R.utils::gzip
