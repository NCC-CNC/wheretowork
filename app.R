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
if (
  is.null(getOption("golem.app.prod")) &&
  !identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
  options("golem.app.prod" = TRUE)
}

# launch application
wheretowork::run_app()
