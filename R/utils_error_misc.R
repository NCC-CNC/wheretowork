#' @include internal.R
NULL

#' Error message
#'
#' Create an error message based on a `try-error` object.
#'
#' @param x `try-error` object.
#'
#' @return `character` value.
#'
#' @export
error_message <- function(x) {
  assertthat::assert_that(inherits(x, c("try-error", "error")))
  out <- paste(strsplit(as.character(x), ": ")[[1]][-1], collapse = ": ")
  out <- paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
  out
}

#' Error log
#'
#' Create an error log based on a `try-error` object.
#'
#' @param x `try-error` object.
#'
#' @details
#' This function checks `attr(x, "log")` for a list of `SimpleError` objects
#' that are used to construct the log. If the argument to `x` does not
#' contain this attribute, then the text `"No error log available."`
#' is returned.
#'
#' @return `character` value.
#'
#' @export
error_log <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, c("try-error", "error")))
  # extract log
  logs <- attr(x, "log")
  # format log
  if (is.list(logs)) {
    ## prepare log error
    out <- c(
      "# wheretowork project import log",
      "",
      "## Summary",
      "",
      error_message(x),
      "## Details",
      "",
      unlist(
        recursive = TRUE, use.names = FALSE,
        lapply(logs, function(x) {
          c(paste("###", x$name), "", as.character(x$details))
        })
      )
    )
  } else {
    out <- c(
     "# wheretowork project import log",
      "",
      "## Summary",
      "",
      as.character(x),
      "",
      "## Details",
      "",
      "Sorry, no further details available."
    )
  }
  # return log
  out
}

#' Show error alert
#'
#' Display an error alert (modal) using a consistent style.
#'
#' @param text `character` message, or HTML tags if `html = TRUE`.
#'
#' @param html `logical` should `text` be rendered as HTML?
#'   Defaults to `FALSE`.
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
show_error_alert <- function(text, html = FALSE) {
  shinyalert::shinyalert(
    title = "Oops",
    text = text,
    size = "s",
    type = "error",
    html = html,
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    showCancelButton = FALSE,
    showConfirmButton = TRUE,
    confirmButtonText = "OK",
    confirmButtonCol = "#337ab7",
    timer = 0,
    animation = TRUE
  )
  invisible(TRUE)
}

#' Show project import error
#'
#' Handle a failed project import. This displays an error alert with a link
#' to download the error log (along with the project configuration file),
#' and contact details for the project author, and then resets the
#' import button.
#'
#' @param x `try-error` object from the failed import.
#'
#' @param configuration_path `character` file path for the project
#'   configuration file.
#'
#' @param button_id `character` HTML element id for the import button.
#'
#' @param output Shiny `output` object (used to set the download handler
#'   for the error log).
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
show_import_error <- function(x, configuration_path, button_id, output) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("try-error", "error")),
    assertthat::is.string(configuration_path),
    assertthat::is.string(button_id)
  )

  # try to parse project author details, falling back to defaults
  project_author_name <- get_golem_config("default_project_name")
  project_author_email <- get_golem_config("default_project_email")
  project_config <- try(yaml::read_yaml(configuration_path), silent = TRUE)
  if (
    !inherits(project_config, "try-error") &&
    assertthat::is.string(project_config$author_name) &&
    assertthat::is.string(project_config$author_email)
  ) {
    project_author_name <- project_config$author_name
    project_author_email <- project_config$author_email
  }

  # prepare download link
  output$importModal_log_link <- shiny::downloadHandler(
    filename = function() {
      paste0(
        tools::file_path_sans_ext(basename(configuration_path)),
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
      file.copy(configuration_path, td)
      # zip files
      withr::with_dir(td, utils::zip(con, files = dir(td)))
    }
  )

  # display error message
  show_error_alert(
    html = TRUE,
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

  # reset import button
  reset_loading_button(button_id)

  invisible(TRUE)
}
