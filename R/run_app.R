#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
run_app <- function(onStart = purrr::partial(
                      eval, expr = app_global, envir = globalenv()
                    ),
                    options = list(),
                    enableBookmarking = "disable",
                    uiPattern = "/",
                    ...) {
  # launch app
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
