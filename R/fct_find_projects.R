#' Find projects
#'
#' Find project files in a directory.
#'
#' @param x `character` directory path.
#'
#' @param user_groups `character` vector of project group names available that
#'   can be imported.
#'  Defaults to `"public"`.
#'
#' @details
#' Projects that are missing user group information are automatically
#' assigned to the `"private"` user group.
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
find_projects <- function(x, user_groups = "public") {
  # assert argument is valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    is.character(user_groups),
    assertthat::noNA(user_groups)
  )

  # find project files
  out <- tibble::tibble(
    path = dir(x, "^.*\\.yaml", full.names = TRUE, recursive = TRUE)
  )

  # import project data
  project_configs <- lapply(out$path, function(x) {
    v <- is_valid_configuration_file(x)
    if (!isTRUE(v)) {
      return(NULL)
    }
    yaml::read_yaml(x)
  })

  # identify project names
  out$name <- vapply(project_configs, FUN.VALUE = character(1), function(x) {
    if (is.null(x)) return(NA_character_)
    x$name
  })

  # identify if in project in available groups
  out$status <- vapply(project_configs, FUN.VALUE = logical(1), function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.null(x$user_group)) {
      x$user_group <- "private" # default project user group to private
    }
    any(user_groups %in% x$user_group)
  })

  # exclude invalid project files
  out <- out[which(!is.na(out$name) & out$status), , drop = FALSE]

  # return result
  out
}
