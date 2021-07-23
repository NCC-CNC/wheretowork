#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {

  # initialization
  ## spawn new process so that users in different process
  try(system("touch restart.txt"), silent = TRUE)
  ## initialize app
  eval(server_initialize_app)

  ## prevent timeout on Heroku if needed
  if (identical(Sys.getenv("HEROKU"), "TRUE")) {
    shiny::observe({
      shiny::invalidateLater(10000)
      cat(".")
    })
  }

  # import data
  ## import data using builtin import option
  eval(server_import_builtin_data)

  ## import data using manual import option
  eval(server_verify_manual_uploads)
  eval(server_import_manual_data)

  ## import data using spatial import option
  eval(server_verify_spatial_uploads)
  eval(server_import_spatial_data)

  # update map
  eval(server_update_map)

  # update server_solution settings
  eval(server_update_solution_settings)

  # generate new solution using settings
  eval(server_generate_new_solution)

  # update solution results
  eval(server_update_solution_results)

  # export data
  eval(server_export_data)
  eval(server_export_spreadsheets)
}
