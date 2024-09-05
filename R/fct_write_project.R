#' @include internal.R
NULL

#' Write project
#'
#' Save a project to disk.
#'
#' @param x `list` of [Theme], [Weight], [Include], and [Exclude] objects.
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
#' @param user_groups `character` vector of user groups than can
#'   access the dataset.
#'   Defaults to `"public"`.
#'   
#' @param wheretowork_version `package_version` of wheretowork
#'   Defaults to `"utils::packageVersion("wheretowork")"`.
#'   
#' @param prioritizr_version `package_version` of prioritizr
#'   Defaults to `"utils::packageVersion("prioritizr")"`.
#'
#' @return Invisible `TRUE` indicating success.
#'
#' @examples
#'  # find data file paths
#'  f1 <- system.file(
#'    "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'    package = "wheretowork"
#'  )
#'  f2 <- system.file(
#'    "extdata", "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'    package = "wheretowork"
#'  )
#'  f3 <- system.file(
#'    "extdata", "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'    package = "wheretowork"
#'  )
#'
#'  # create new dataset
#'  d <- new_dataset(f1, f2, f3)
#'
#'  # simulate themes and weights
#'  th <- simulate_themes(d, 1, 1, 2)
#'  w <- simulate_weights(d, 1)
#'
#'  # combine themes and weights into a list
#'  l <- append(th, w)
#'
#'  # save project
#'  write_project(
#'    x = l,
#'    name = "example",
#'    dataset = d,
#'    path = tempfile(),
#'    spatial_path = tempfile(fileext = ".tif"),
#'    attribute_path = tempfile(fileext = ".csv.gz"),
#'    boundary_path = tempfile(fileext = ".csv.gz")
#'  )
#' @export
write_project <- function(x, dataset, path, name,
                          spatial_path, attribute_path, boundary_path,
                          mode = "advanced", user_groups = "public",
                          author_name = NULL, author_email = NULL,
                          wheretowork_version = utils::packageVersion("wheretowork"), 
                          prioritizr_version = utils::packageVersion("prioritizr")) {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(x),
    all_list_elements_inherit(x, c("Theme", "Weight", "Include", "Exclude")),
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
    assertthat::noNA(mode),
    is.character(user_groups),
    assertthat::noNA(user_groups),
    assert_that(inherits(wheretowork_version, "package_version")),
    assert_that(inherits(prioritizr_version, "package_version"))
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

  # create setting list for themes
  themes_idx <- vapply(x, inherits, what = "Theme", logical(1))
  themes_params <- lapply(x[themes_idx], function(x) x$export())

  # create setting list for weights
  weights_idx <- vapply(x, inherits, what = "Weight", logical(1))
  weights_params <- lapply(x[weights_idx], function(x) x$export())

  # create setting list for includes
  includes_idx <- vapply(x, inherits, what = "Include", logical(1))
  includes_params <- lapply(x[includes_idx], function(x) x$export())
  
  # create setting list for excludes
  excludes_idx <- vapply(x, inherits, what = "Exclude", logical(1))
  excludes_params <- lapply(x[excludes_idx], function(x) x$export())  

  # create full settings list
  ## add project name
  params <- list()
  ## add project name
  params$name <- name
  ## add contact details
  if (!is.null(author_name)) {
    params$author_name <- author_name
    params$author_email <- author_email
  }
  ## add wheretowork version
  params$wheretowork_version <- as.character(wheretowork_version)
  ## add prioritizr version
  params$prioritizr_version <- as.character(prioritizr_version)  
  ## specify application mode
  params$mode <- mode
  ## add user groups
  params$user_groups <- user_groups
  ## add data
  params$spatial_path <- basename(spatial_path)
  params$attribute_path <- basename(attribute_path)
  params$boundary_path <- basename(boundary_path)
  params$themes <- themes_params
  params$weights <- weights_params
  params$includes <- includes_params
  params$excludes <- excludes_params

  # coerce characters to ASCII
  params <- enc2ascii(params)

  # save configuration file to disk
  yaml::write_yaml(params, path)

  # save dataset to disk
  dataset$write(spatial_path, attribute_path, boundary_path)

  # return success
  invisible(TRUE)
}
