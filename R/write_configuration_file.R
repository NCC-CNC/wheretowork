#' @include internal.R
NULL

#' Write a configuration file
#'
#' Save a configuration file to disk.
#' The configuration file will contain all the data and settings related
#' to a set of [Theme] and [Weight] objects.
#'
#' @param x `list` of [Theme] and [Weight] objects.
#'
#' @param name `character` name for the scenario.
#'
#' @param path `character` file path to save the configuration file.
#'
#' @param spatial_path `character` file path for the spatial data.
#'
#' @param attribute_path `character` file path for the attribute data.
#'
#' @param boundary_path `character` file path for the attribute data.
#'
#' @param mode `character` mode for running the application.
#'   Defaults to `"advanced"`.
#'
#' @return Invisible `TRUE` indicating success.
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "sim_raster_spatial.tif", package = "locationmisc")
#' f2 <- system.file(
#'  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
#' f3 <- system.file(
#'  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # simulate themes and weights
#' th <- simulate_themes(d, 1, 1, 2)
#' w <- simulate_weights(d, 1)
#'
#' # combine themes and weights into a list
#' l <- append(th, w)
#'
#' # save configuration file to temporary location
#' write_configuration_file(
#'   l,
#'   path = tempfile(),
#'   name = "example",
#'   spatial_path = tempfile(fileext = ".tif"),
#'   attribute_path = tempfile(fileext = ".csv.gz"),
#'   boundary_path = tempfile(fileext = ".csv.gz"))
#'
#' @export
write_configuration_file <- function(
  x, path, name, spatial_path, attribute_path, boundary_path,
  mode = "advanced") {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(x),
    all_list_elements_inherit(x, c("Theme", "Weight")),
    assertthat::is.string(name),
    assertthat::noNA(name),
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::is.string(spatial_path),
    assertthat::noNA(spatial_path),
    assertthat::is.string(attribute_path),
    assertthat::noNA(attribute_path),
    assertthat::is.string(boundary_path),
    assertthat::noNA(boundary_path),
    assertthat::is.string(mode),
    assertthat::noNA(mode))

  # create setting list for themes
  themes_idx <- vapply(x, inherits, what = "Theme", logical(1))
  themes_params <- lapply(x[themes_idx], function(x) x$export())

  # create setting list for weights
  weights_idx <- vapply(x, inherits, what = "Weight", logical(1))
  weights_params <- lapply(x[weights_idx], function(x) x$export())

  # create full settings list
  params <- list(
    name = name,
    spatial_path = spatial_path,
    attribute_path = attribute_path,
    boundary_path = boundary_path,
    mode = mode,
    themes = themes_params,
    weights = weights_params
  )

  # save result to disk
  yaml::write_yaml(params, path)

  # return success
  invisible(TRUE)
}
