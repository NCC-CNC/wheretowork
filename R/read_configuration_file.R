#' @include internal.R
NULL

#' Read a configuration file
#'
#' Read a configuration file from disk.
#' The configuration file will contain all the data and settings related
#' to a set of [Theme], [Weight], and [Include] objects.
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
#'   "extdata", "sim_raster_data.yaml", package = "locationmisc")
#' f2 <- system.file(
#'   "extdata", "sim_raster_spatial.tif", package = "locationmisc")
#' f3 <- system.file(
#'  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
#' f4 <- system.file(
#'  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")
#'
#' # read configuration file
#' x <- read_configuration_file(f1, f2, f3, f4)
#'
#' # print results
#' print(x)
#'
#' @export
read_configuration_file <- function(
  path, spatial_path = NULL, attribute_path = NULL, boundary_path = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path))
  if (!is.null(spatial_path)) {
    assertthat::assert_that(
      assertthat::is.string(spatial_path),
      assertthat::noNA(spatial_path))
  }
  if (!is.null(attribute_path)) {
    assertthat::assert_that(
      assertthat::is.string(attribute_path),
      assertthat::noNA(attribute_path))
  }
  if (!is.null(boundary_path)) {
    assertthat::assert_that(
      assertthat::is.string(boundary_path),
      assertthat::noNA(boundary_path))
  }

  # import configuration file
  ## read file
  x <- try(yaml::read_yaml(path), silent = TRUE)
  if (inherits(x, "try-error")) {
    stop("configuration file is not a valid YAML file.")
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
          "\""),
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
    boundary_path = boundary_path)

  # import themes
  ## import data
  themes <- lapply(x$themes, function(x) {
    try(
      new_theme(
        name = x$name,
        feature = lapply(x$feature, function(f) {
          new_feature(
            name = f$name,
            initial_visible = f$initial_visible,
            initial_status = f$initial_status,
            initial_goal = f$initial_goal,
            min_goal = f$min_goal,
            max_goal = f$max_goal,
            step_goal = f$step_goal,
            limit_goal = f$limit_goal,
            current = 0,
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
        initial_visible = x$initial_visible,
        initial_status = x$initial_status,
        initial_factor = x$initial_factor,
        min_factor = x$min_factor,
        max_factor = x$max_factor,
        step_factor = x$step_factor,
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
        initial_visible = x$initial_visible,
        initial_status = x$initial_status,
        mandatory = x$mandatory
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

  # calculate current amount held for each feature within each theme
  ss <- new_solution_settings(
    themes = themes, weights = weights, includes = includes,
    parameters = list())
  ss$update_current_held()

  # return result
  list(
    name = x$name,
    dataset = d,
    themes = themes,
    weights = weights,
    includes = includes,
    mode = x$mode
  )
}
