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
#' The application is a decision support tool to help prioritize conservation
#' efforts for the Nature Conservancy of Canada. It provides an interactive
#' interface for conducting systematic conservation planning exercises, and
#' uses mathematical optimization algorithms to generate solutions.
#'
#' @name wheretowork
#'
#' @docType _PACKAGE
#'
#' @examples
#' \donttest{
#  # launch application
#' if (interactive()) {
#' run_app()
#' }
#' }
NULL

# define global variables to pass package checks
## these variables are used in lazy evaluation or the shiny application
utils::globalVariables(
  c(
    "absolute_area",
    "absolute_perimeter",
    "statistics",
    "session",
    "app_data",
    "project_data",
    "import_data"
  )
)

# define functions for internally used packages to pass checks
tmp1 <- R.utils::gzip
tmp1 <- rcbc::cbc_solve
