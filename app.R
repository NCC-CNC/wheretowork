# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# load package if possible
if (file.exists("DESCRIPTION")) {
  pkgload::load_all(
    export_all = FALSE, helpers = FALSE, attach_testthat = FALSE
  )
}

# set application production mode
if (is.null(getOption("golem.app.prod"))) {
  options("golem.app.prod" = TRUE)
}

# set ports if needed
if (!is.null(Sys.getenv("R_SHINY_PORT"))) {
  options(shiny.port = as.numeric(Sys.getenv("R_SHINY_PORT")))
}
if (!is.null(Sys.getenv("R_SHINY_HOST"))) {
  options(shiny.host = Sys.getenv("R_SHINY_HOST"))
}

# launch application
wheretowork::run_app()
