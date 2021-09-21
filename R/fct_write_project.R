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
#' @param author_name `character` name of the project author.
#'   Defaults to `NULL` such that no author name is encoded in the
#'   project configuration file. This means that the application
#'   will report default contact details.
#'
#' @param author_email `character` email address of the project author.
#'   Defaults to `NULL` such that no email address is encoded in the
#'   project configuration file. This means that the application
#'   will report default contact details.
#'
#' @param mode `character` mode for running the application.
#'   Defaults to `"advanced"`.
#'
#' @param tile_path `character` file path for tiles.
#'   Defaults to `NULL, such that no tiles are created.
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
#'   "extdata", "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
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
#'   boundary_path = tempfile(fileext = ".csv.gz")
#' )
#' @export
write_project <- function(x, dataset, path, name,
                          spatial_path, attribute_path, boundary_path,
                          mode = "advanced",
                          author_name = NULL, author_email = NULL,
                          tile_path = NULL) {
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
    assertthat::noNA(mode)
  )
  if (!is.null(author_name)) {
    assertthat::assert_that(
      assertthat::is.string(author_name),
      assertthat::noNA(author_name)
    )
  }
  if (!is.null(author_email)) {
    assertthat::assert_that(
      assertthat::is.string(author_email),
      assertthat::noNA(author_email)
    )
  }
  if (!identical(class(author_name), class(author_email))) {
    stop("Both arguments to author_name and author_email must be supplied")
  }
  if (!is.null(tile_path)) {
    assertthat::assert_that(
      assertthat::is.string(tile_path),
      assertthat::noNA(tile_path)
    )
  }

  # identify data
  themes_idx <- vapply(x, inherits, what = "Theme", logical(1))
  weights_idx <- vapply(x, inherits, what = "Weight", logical(1))
  includes_idx <- vapply(x, inherits, what = "Include", logical(1))

  # save tiles
  if (!is.null(tile_path)) {
    ## create folder
    dir.create(tile_path, showWarnings = FALSE, recursive = TRUE)
    ## themes
    lapply(x[themes_idx], function(x) {
      lapply(x$feature, function(y) {
        y$variable$write_tiles(tile_path)
      })
    })
    ## weights
    lapply(x[weights_idx], function(x) x$variable$write_tiles(tile_path))
    ## includes
    lapply(x[includes_idx], function(x) x$variable$write_tiles(tile_path))
  }

  # create setting lists
  themes_params <- lapply(x[themes_idx], function(x) x$export())
  weights_params <- lapply(x[weights_idx], function(x) x$export())
  includes_params <- lapply(x[includes_idx], function(x) x$export())

  # create full settings list
  ## add project name
  params <- list()
  params$name <- enc2utf8(name)
  ## add contact details
  if (!is.null(author_name)) {
    params$author_name <- author_name
    params$author_email <- author_email
  }
  ## add paths
  params$spatial_path <- basename(spatial_path)
  params$attribute_path <- basename(attribute_path)
  params$boundary_path <- basename(boundary_path)
  if (!is.null(tile_path)) {
    params$tile_path <- basename(tile_path)
  }
  ## add metadata
  params$mode <- mode
  params$themes <- themes_params
  params$weights <- weights_params
  params$includes <- includes_params

  # save configuration file to disk
  yaml::write_yaml(params, path)

  # save dataset to disk
  dataset$write(spatial_path, attribute_path, boundary_path)

  # return success
  invisible(TRUE)
}
