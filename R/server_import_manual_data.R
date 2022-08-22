#' Sever function: import manual data
#'
#' Set behavior for importing projects using manual upload option.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_manual_data)
#' ```
#'
#' @noRd
server_import_manual_data <- quote({

  # set behavior for importing data using the manual option
  shiny::observeEvent(input$importModal_manual_button, {
    ## validation
    if (
      !is.character(app_data$configuration_path) ||
      !is.character(app_data$spatial_path) ||
      !is.character(app_data$attribute_path) ||
      !is.character(app_data$boundary_path)) {
      return()
    }

    ## update import button
    disable_html_element("importModal_manual_button")

    ## import configuration
    x <- try(
      read_project(
        path = app_data$configuration_path,
        spatial_path = app_data$spatial_path,
        attribute_path = app_data$attribute_path,
        boundary_path = app_data$boundary_path,
        mode = get_golem_config("mode"),
        force_hidden = app_data$manual_hidden
      ),
      silent = TRUE
    )

    ## throw error if needed
    if (inherits(x, c("try-error", "error"))) {

      ## try to parse project author details
      project_config <- try(
        yaml::read_yaml(app_data$configuration_path), silent = TRUE
      )
      if (inherits(project_config, "try-error")) {
          project_author_name <- get_golem_config("default_project_name")
          project_author_email <- get_golem_config("default_project_email")
      } else {
        if (assertthat::is.string(project_config$author_name) &&
            assertthat::is.string(project_config$author_email)) {
          project_author_name <- project_config$author_name
          project_author_email <- project_config$author_email
        } else {
          project_author_name <- get_golem_config("default_project_name")
          project_author_email <- get_golem_config("default_project_email")
        }
      }

      ## prepare download link
      output$importModal_log_link <- shiny::downloadHandler(
        filename = function() {
          paste0(
            tools::file_path_sans_ext(basename(app_data$configuration_path)),
            "_log.zip"
          )
        },
        content = function(con) {
          # create temporary directory to assemble zip file
          td <- tempfile()
          dir.create(td, showWarnings = FALSE, recursive = FALSE)
          # save log file to temporary directory
          writeLines(error_log(x), file.path(td, "error-log.txt"))
          # copy confgiuration file to temporary directory
          file.copy(app_data$configuration_path, td)
          # zip files
          withr::with_dir(td, utils::zip(con, files = dir(td)))
        }
      )

      ## display error message
      shinyalert::shinyalert(
        title = "Oops...",
        type = "error",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        confirmButtonCol = "#337ab7",
        text = htmltools::tags$span(
          htmltools::tags$span(
            paste0(
              "Something went wrong when importing this project. ",
              "This is likely due to a mistake in the project data. ",
              "To resolve this issue, please download the error log "
            ),
            .noWS = "outside"
          ),
          shiny::downloadLink(
            outputId = "importModal_log_link",
            label = "(download here)"
          ),
          htmltools::tags$span(
            paste0(" and email it to ", project_author_name, " "),
            .noWS = "outside"
          ),
          htmltools::tags$a(
            paste0("(", project_author_email, ")"),
            href = paste0("mailto:", project_author_email)
          )
        )
      )

      ## reset import button
      shinyFeedback::resetLoadingButton("importModal_manual_button")
      enable_html_element("importModal_manual_button")

      ## exit
      return()
    }

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = get_golem_config("mode"))

    ## remove modal
    shiny::removeModal(session)

    ## show help modal
    if (identical(app_data$mode, "beginner")) {
      shinyBS::toggleModal(session, modalId = "helpModal", toggle = "open")
    }

  })
})
