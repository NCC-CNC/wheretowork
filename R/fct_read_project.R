#' @include internal.R
NULL

#' Read project
#'
#' Read a project from disk.
#'
#' @param path `character` file path for the configuration file.
#'
#' @param spatial_path `character` file path for spatial data.
#'  Defaults to `NULL` such that the file path is determined using
#'  the argument to `path`.
#'
#' @param attribute_path `character` file path for attribute data.
#'  Defaults to `NULL` such that the file path is determined using
#'  the argument to `path`.
#'
#' @param boundary_path `character` file path for boundary data.
#'  Defaults to `NULL` such that the file path is determined using
#'  the argument to `path`.
#'
#' @param mode `character` mode for importing the objects.
#'  Defaults to `NULL` such that the mode is determined based on
#'  the contents of `path`. If the `mode` is `"advanced"`, then
#'  goal limits and mandatory include settings are disabled.
#'
#' @details
#' Note that the status field for themes and weights will automatically
#' be set to `FALSE` if the goal/weight values are zero.
#' This is to ensure that the application starts with a sensible
#' combination of settings.
#'
#' @return A `list` containing the following elements:
#' \describe{
#' \item{name}{A `character` value indicating the name.}
#' \item{dataset}{A [Dataset] object.}
#' \item{themes}{A `list` of [Theme] objects.}
#' \item{weights}{A `list` of [Weight] objects.}
#' \item{includes}{A `list` of [Include] objects.}
#' \item{mode}{A `character` value indicating the mode.}
#' }
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_data.yaml",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f4 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # read project
#' x <- read_project(f1, f2, f3, f4)
#'
#' # print project
#' print(x)
#' @export
read_project <- function(path,
                         spatial_path = NULL,
                         attribute_path = NULL,
                         boundary_path = NULL,
                         mode = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path)
  )
  if (!is.null(spatial_path)) {
    assertthat::assert_that(
      assertthat::is.string(spatial_path),
      assertthat::noNA(spatial_path)
    )
  }
  if (!is.null(attribute_path)) {
    assertthat::assert_that(
      assertthat::is.string(attribute_path),
      assertthat::noNA(attribute_path)
    )
  }
  if (!is.null(boundary_path)) {
    assertthat::assert_that(
      assertthat::is.string(boundary_path),
      assertthat::noNA(boundary_path)
    )
  }
  if (!is.null(mode)) {
    assertthat::assert_that(
      assertthat::is.string(mode),
      assertthat::noNA(mode)
    )
  }

  # import configuration file
  ## read file
  x <- try(yaml::read_yaml(path), silent = TRUE)
  if (inherits(x, "try-error")) {
    stop("configuration file is not a valid YAML file.")
  }

  # determine if advanced mode
  adv_mode <- identical(mode, "advanced")
  if (is.null(mode)) {
    adv_mode <- identical(x$mode, "advanced")
  }


  # set file paths if needed
  if (is.null(spatial_path)) {
    spatial_path <- file.path(dirname(path), basename(x$spatial_path))
  }
  if (is.null(attribute_path)) {
    attribute_path <- file.path(dirname(path), basename(x$attribute_path))
  }
  if (is.null(boundary_path)) {
    boundary_path <- file.path(dirname(path), basename(x$boundary_path))
  }

  ## verify that file names listed in file match arguments
  if (endsWith(x$spatial_path, ".shp")) {
    message_path <-
      paste(
        paste0(
          "\"",
          tools::file_path_sans_ext(basename(x$spatial_path)),
          c(".shp", ".dbf", ".prj", ".shx"),
          "\""
        ),
        collapse = ", "
      )
  } else {
    message_path <- paste0("\"", basename(x$spatial_path), "\"")
  }
  assertthat::assert_that(
    identical(basename(spatial_path), basename(x$spatial_path)),
    msg = paste0("spatial file(s) should be ", basename(message_path))
  )
  assertthat::assert_that(
    identical(basename(attribute_path), basename(x$attribute_path)),
    msg =
      paste0("attribute file should be \"", basename(x$attribute_path), "\"")
  )
  assertthat::assert_that(
    identical(basename(boundary_path), basename(x$boundary_path)),
    msg = paste0("boundary file should be \"", basename(x$boundary_path), "\"")
  )

  # create dataset
  d <- new_dataset(
    spatial_path = spatial_path,
    attribute_path = attribute_path,
    boundary_path = boundary_path
  )

  # import themes
  ## import data
  themes <- lapply(x$themes, function(x) {
    try(
      new_theme(
        name = x$name,
        feature = lapply(x$feature, function(f) {
          new_feature(
            name = f$name,
            visible = f$visible,
            status = f$status && (f$goal > 1e-5),
            goal = f$goal,
            limit_goal = ifelse(adv_mode, 0, f$limit_goal),
            current = 0, # place-holder value, this is calculated later
            variable = new_variable_from_auto(
              dataset = d,
              index = f$variable$index,
              units = f$variable$units,
              type = f$variable$legend$type,
              colors = f$variable$legend$colors
            )
          )
        })
      ),
      silent = FALSE
    )
  })
  ## throw error message if needed
  fail <- vapply(themes, inherits, logical(1), "try-error")
  fail <- vapply(x$themes[fail], `[[`, character(1), "name")
  assertthat::assert_that(
    length(fail) == 0,
    msg = paste0(
      "failed to import the following themes: ",
      paste(paste0("\"", fail, "\""), collapse = ", ")
    )
  )

  # import weights
  ## import data
  weights <- lapply(x$weights, function(x) {
    try(
      new_weight(
        name = x$name,
        visible = x$visible,
        status = x$status && (x$factor > 1e-5),
        factor = x$factor,
        current = 0, # place-holder value, this is calculated later
        variable = new_variable_from_auto(
          dataset = d,
          index = x$variable$index,
          units = x$variable$units,
          type = x$variable$legend$type,
          colors = x$variable$legend$colors
        )
      ),
      silent = TRUE
    )
  })
  ## throw error message if needed
  fail <- vapply(weights, inherits, logical(1), "try-error")
  fail <- vapply(x$weights[fail], `[[`, character(1), "name")
  assertthat::assert_that(
    length(fail) == 0,
    msg = paste0(
      "failed to import the following includes: ",
      paste(paste0("\"", fail, "\""), collapse = ", ")
    )
  )


  # import includes
  ## import data
  includes <- lapply(x$includes, function(x) {
    try(
      new_include(
        name = x$name,
        variable =
          new_variable(
            dataset = d,
            index = x$variable$index,
            units = x$variable$units,
            total = sum(d$get_attribute_data()[[x$variable$index]]),
            legend = new_manual_legend(
              colors = x$variable$legend$colors,
              labels = x$variable$legend$labels
            )
          ),
        visible = x$visible,
        status = x$status,
        mandatory = ifelse(adv_mode, FALSE, x$mandatory)
      ),
      silent = FALSE
    )
  })
  ## throw error message if needed
  fail <- vapply(includes, inherits, logical(1), "try-error")
  fail <- vapply(x$includes[fail], `[[`, character(1), "name")
  assertthat::assert_that(
    length(fail) == 0,
    msg = paste0(
      "failed to import the following includes: ",
      paste(paste0("\"", fail, "\""), collapse = ", ")
    )
  )

  # calculate current amount held for each feature within each theme + weight
  ss <- new_solution_settings(
    themes = themes, weights = weights, includes = includes,
    parameters = list()
  )
  ss$update_current_held()

  # return result
  list(
    name = x$name,
    dataset = d,
    themes = themes,
    weights = weights,
    includes = includes,
    mode = ifelse(is.null(mode), x$mode, mode)
  )
}
