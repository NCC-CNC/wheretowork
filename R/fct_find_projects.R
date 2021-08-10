#' Find projects
#'
#' Find project files in a directory.
#'
#' @param x `character` directory path.
#'
#' @return [tibble::tibble()] object containing the configuration files
#'  and names of projects found in the directory.
#'
#' @examples
#' # find directory with built-in projects
#' d <- system.file("extdata", "projects", package = "wheretowork")
#'
#' # list projects in directory
#' find_projects(d)
#' @export
find_projects <- function(x) {
  # assert argument is valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )

  # find project files
  out <- tibble::tibble(
    path = dir(x, "^.*\\.yaml", full.names = TRUE, recursive = TRUE)
  )

  # identify project names
  out$name <-
    vapply(out$path, FUN.VALUE = character(1), function(x) {
      v <- is_valid_configuration_file(x)
      if (!isTRUE(v)) {
        return(NA_character_)
      }
      yaml::read_yaml(x)$name[[1]]
    })

  # exclude invalid project files
  out <- out[!is.na(out$name), , drop = FALSE]

  # return result
  out
}
