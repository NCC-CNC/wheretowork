#' Run examples
#'
#' Run a 'Shiny' application containing showcasing the widgets implemented
#' in the \pkg{locationmisc} package.
#'
#' @examples
#' if (interactive()) {
#'   runExample()
#' }
#'
#' @export
runExample <- function() {
  appDir <- system.file("example", package = "locationmisc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `locationmisc`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
