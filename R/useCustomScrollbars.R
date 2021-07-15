#' @include internal.R
NULL

#' Use custom scrollbars
#'
#' This function is used to add custom scrollbars to a Shiny application.
#'
# @return `shiny.tag` object.
#'
#' @export
useCustomScrollbars <- function() {
  shiny::includeCSS(
    system.file(
      "htmlwidgets", "lib",  "scrollbar-1.0.0", "style.css",
      package = "locationmisc"
    )
  )
}
