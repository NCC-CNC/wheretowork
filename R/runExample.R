#' Run examples
#'
#' Run a 'Shiny' application containing showcasing widgets implemented
#' in the \pkg{locationmisc} package.
#'
#' @param name `character` name of widget.
#'   Available options include `"solutionSettings"` and `"mapManager"`.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   runExample("solutionSettings")
#' }
#' }
#' @export
runExample <- function(name) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(name),
    assertthat::noNA(name))
  # find source code for example
  appDir <- system.file("example", name, package = "locationmisc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `locationmisc`.",
         call. = FALSE)
  }
  # run app
  shiny::runApp(appDir, display.mode = "normal")
}
