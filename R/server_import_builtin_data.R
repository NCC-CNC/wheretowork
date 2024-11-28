#' Sever function: import builtin data
#'
#' Set behavior for importing projects using builtin option.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_builtin_data)
#' ```
#'
#' @noRd
server_import_builtin_data <- quote({
  
  # set behavior for importing data using the builtin option
  shiny::observeEvent(input$importModal_builtin_button, {
    ## specify dependencies
    shiny::req(input$importModal_builtin_button)
    shiny::req(input$importModal_name)

    ## update import button
    disable_html_element("importModal_builtin_button")

    ## import configuration
    x <- try(
      read_project(
        path = input$importModal_name,
        mode = get_golem_config("mode"),
      ),
      silent = TRUE
    )

    ## throw error if needed
    if (inherits(x, c("try-error", "error"))) {

      ## try to parse project author details
      project_config <- try(
        yaml::read_yaml(input$importModal_name), silent = TRUE
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
            tools::file_path_sans_ext(basename(input$importModal_name)),
            "_log.zip"
          )
        },
        content = function(con) {
          # create temporary directory to assemble zip file
          td <- tempfile()
          dir.create(td, showWarnings = FALSE, recursive = FALSE)
          # save log file to temporary directory
          writeLines(error_log(x), file.path(td, "error-log.txt"))
          # copy configuration file to temporary directory
          file.copy(input$importModal_name, td)
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
      shinyFeedback::resetLoadingButton("importModal_builtin_button")
      enable_html_element("importModal_builtin_button")

      ## exit
      return()
    }

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = get_golem_config("mode"))

    ## remove data modal
    shiny::removeModal(session)
    
    # add side-bar spinner
    shinyjs::runjs(
      "const sidebarSpinner = document.createElement('div');
       sidebarSpinner.classList.add('sidebar-spinner');
       const mapManagerPane_settings = document.querySelector('#mapManagerPane_settings');
       mapManagerPane_settings.appendChild(sidebarSpinner);"
    )    

    ## show help modal if beginner
    if (identical(app_data$mode, "beginner")) {
      shinyBS::toggleModal(session, modalId = "helpModal", toggle = "open")
    }

  })
})
