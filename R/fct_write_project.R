#' @include internal.R
NULL

#' Write project
#'
#' Save a project to disk.
#'
#' @param x `list` of [Theme], [Weight], and [Include] objects.
#'
#' @param dataset [Dataset] object.
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
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'  "extdata", "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'  package = "wheretowork"
#' )
#' f3 <- system.file(
#'  "extdata", "projects", "sim_raster","sim_raster_boundary.csv.gz",
#'  package = "wheretowork"
#' )
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
#' # save project
#' write_project(
#'   x = l,
#'   name = "example",
#'   dataset = d,
#'   path = tempfile(),
#'   spatial_path = tempfile(fileext = ".tif"),
#'   attribute_path = tempfile(fileext = ".csv.gz"),
#'   boundary_path = tempfile(fileext = ".csv.gz"))
#'
#' @export
write_project <- function(
  x, dataset, path, name, spatial_path, attribute_path, boundary_path,
  mode = "advanced") {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(x),
    all_list_elements_inherit(x, c("Theme", "Weight", "Include")),
    inherits(dataset, "Dataset"),
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

  # create setting list for includes
  includes_idx <- vapply(x, inherits, what = "Include", logical(1))
  includes_params <- lapply(x[includes_idx], function(x) x$export())

  # create full settings list
  params <- list(
    name = name,
    spatial_path = basename(spatial_path),
    attribute_path = basename(attribute_path),
    boundary_path = basename(boundary_path),
    mode = mode,
    themes = themes_params,
    weights = weights_params,
    includes = includes_params
  )

  # save configuration file to disk
  yaml::write_yaml(params, path)

  # save dataset to disk
  dataset$write(spatial_path, attribute_path, boundary_path)

  # return success
  invisible(TRUE)
}
