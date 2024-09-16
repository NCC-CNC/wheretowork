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
#'  Defaults to `"project"` such that the mode is determined based on
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
#' \item{excludes}{A `list` of [Exclude] objects.}
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
                         mode = "project") {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::is.string(mode),
    assertthat::noNA(mode)
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

  # import configuration file
  ## read file
  x <- try(
    yaml::yaml.load(enc2utf8(paste(readLines(path), collapse = "\n"))),
    silent = TRUE
  )
  if (inherits(x, "try-error")) {
    stop("configuration file is not a valid YAML file.")
  }

  # determine if advanced mode
  if (identical(mode, "project")) {
    adv_mode <- identical(x$mode, "advanced")
  } else {
    adv_mode <- identical(mode, "advanced")
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
  
  # update boundary matrix on older wheretowork projects (backwards compatibility)
  if (is.null(x$prioritizr_version) || x$prioritizr_version < "8.0.4") {
    d$update_bm()
  }

  # initialize error variables
  error_msg <- c()
  error_log <- list()

  # import themes
  ## import data
  themes <- lapply(x$themes, function(x) {
    ### create object
    out <- try(
      new_theme(
        name = x$name,
        feature = lapply(x$feature, function(f) {
          new_feature(
            name = f$name,
            visible = f$visible,
            hidden = f$hidden,
            status = f$status,
            goal = f$goal,
            limit_goal = f$limit_goal,
            current = 0, # place-holder value, this is calculated later
            variable = new_variable_from_auto(
              dataset = d,
              index = f$variable$index,
              units = f$variable$units,
              type = f$variable$legend$type,
              colors = f$variable$legend$colors,
              provenance = f$variable$provenance %||% "missing",
              labels = f$variable$legend$labels %||% "missing",
              hidden = f$hidden
            )
          )
        })
      ),
      silent = FALSE
    )
    ### return error if needed
    if (inherits(out, "try-error")) {
      return(out)
    }
    ### override settings
    for (i in seq_along(out$feature)) {
      if (out$feature[[i]]$goal < 1e-5) {
        out$feature[[i]]$status <- FALSE
      }
      if (adv_mode) {
        out$feature[[i]]$limit_goal <- 0
      }
    }
    ### return result
    out
  })
  ## update error details if needed
  fail <- vapply(themes, inherits, logical(1), "try-error")
  if (any(fail)) {
    error_msg <- c(error_msg, paste0(
      "Failed to import the following Themes: ",
      paste(
        paste0("\"", vapply(x$themes[fail], `[[`, character(1), "name"), "\""),
        collapse = ", "
      )
    ))
    error_log <- append(error_log, lapply(which(fail), function(i) {
      list(name = x$themes[[i]]$name, details = themes[[i]])
    }))
  }

  # import weights
  ## import data
  weights <- lapply(x$weights, function(x) {
    ### create object
    out <- try(
      new_weight(
        name = x$name,
        visible = x$visible,
        hidden = x$hidden,
        status = x$status,
        factor = x$factor,
        current = 0, # place-holder value, this is calculated later
        variable = new_variable_from_auto(
          dataset = d,
          index = x$variable$index,
          units = x$variable$units,
          type = x$variable$legend$type,
          colors = x$variable$legend$colors,
          provenance = x$variable$provenance %||% "missing",
          labels = x$variable$legend$labels %||% "missing",
          hidden = x$hidden
        )
      ),
      silent = TRUE
    )
    ### return error if needed
    if (inherits(out, "try-error")) {
      return(out)
    }
    ### override settings
    if (x$factor < 1e-5) {
      out$status <- FALSE
    }
    ### return result
    out
  })
  ## update error details if needed
  fail <- vapply(weights, inherits, logical(1), "try-error")
  if (any(fail)) {
    error_msg <- c(error_msg, paste0(
      "Failed to import the following Weights: ",
      paste(
        paste0(
          "\"", vapply(x$weights[fail], `[[`, character(1), "name"), "\""
        ),
        collapse = ", "
      )
    ))
    error_log <- append(error_log, lapply(which(fail), function(i) {
      list(name = x$weights[[i]]$name, details = weights[[i]])
    }))
  }

  # import includes
  ## import data
  includes <- lapply(x$includes, function(x) {
    ### create legend
    if (x$hidden) {
      include_legend <- new_null_legend()
    } else {
      include_legend <- new_manual_legend(
        values = c(0, 1),
        colors = x$variable$legend$colors,
        labels = x$variable$legend$labels
      )
    }
    ### create object
    out <- try(
      new_include(
        name = x$name,
        variable = new_variable(
          dataset = d,
          index = x$variable$index,
          units = x$variable$units,
          total = sum(d$get_attribute_data()[[x$variable$index]]),
          legend = include_legend,
          provenance = new_provenance_from_source(
            x$variable$provenance %||% "missing"
          )
        ),
        visible = x$visible,
        hidden =  x$hidden,
        status = x$status,
        mandatory = x$mandatory
      ),
      silent = FALSE
    )
    ### return error if needed
    if (inherits(out, "try-error")) {
      return(out)
    }
    ### override settings
    if (adv_mode) {
      out$mandatory <- FALSE
    }
    ### return result
    out
  })
  
  ## update error details if needed
  fail <- vapply(includes, inherits, logical(1), "try-error")
  if (any(fail)) {
    error_msg <- c(error_msg, paste0(
      "Failed to import the following Includes: ",
      paste(
        paste0(
          "\"", vapply(x$includes[fail], `[[`, character(1), "name"), "\""
        ),
        collapse = ", "
      )
    ))
    error_log <- append(error_log, lapply(which(fail), function(i) {
      list(name = x$includes[[i]]$name, details = includes[[i]])
    }))
  }

  # throw error message if needed
  if (length(error_msg) > 0) {
    error_msg <- simpleError(paste(error_msg, collapse = "\n"))
    attr(error_msg, "log") <- error_log
    return(error_msg)
  }
  
  # import excludes
  ## import data
  excludes <- lapply(x$excludes, function(x) {
    ### create legend
    if (x$hidden) {
      exclude_legend <- new_null_legend()
    } else {
      exclude_legend <- new_manual_legend(
        values = c(0, 1),
        colors = x$variable$legend$colors,
        labels = x$variable$legend$labels
      )
    }
    ### create object
    out <- try(
      new_exclude(
        name = x$name,
        variable = new_variable(
          dataset = d,
          index = x$variable$index,
          units = x$variable$units,
          total = sum(d$get_attribute_data()[[x$variable$index]]),
          legend = exclude_legend,
          provenance = new_provenance_from_source(
            x$variable$provenance %||% "missing"
          )
        ),
        visible = x$visible,
        hidden =  x$hidden,
        status = FALSE, 
        mandatory = x$mandatory
      ),
      silent = FALSE
    )
    ### return error if needed
    if (inherits(out, "try-error")) {
      return(out)
    }
    ### override settings
    if (adv_mode) {
      out$mandatory <- FALSE
    }
    ### return result
    out
  })
  
  ## update error details if needed
  fail <- vapply(excludes, inherits, logical(1), "try-error")
  if (any(fail)) {
    error_msg <- c(error_msg, paste0(
      "Failed to import the following Excludes: ",
      paste(
        paste0(
          "\"", vapply(x$excludes[fail], `[[`, character(1), "name"), "\""
        ),
        collapse = ", "
      )
    ))
    error_log <- append(error_log, lapply(which(fail), function(i) {
      list(name = x$excludes[[i]]$name, details = excludes[[i]])
    }))
  }
  
  # throw error message if needed
  if (length(error_msg) > 0) {
    error_msg <- simpleError(paste(error_msg, collapse = "\n"))
    attr(error_msg, "log") <- error_log
    return(error_msg)
  }
  

  # calculate current amount held for each feature within each theme + weight
  ss <- new_solution_settings(
    themes = themes, weights = weights, includes = includes, excludes = excludes,
    parameters = list()
  )
  ss$update_current_held()

  # return result
  list(
    name = x$name,
    author_name = x$author_name,
    author_email = x$author_email,
    dataset = d,
    themes = themes,
    weights = weights,
    includes = includes,
    excludes = excludes,
    mode = ifelse(identical(mode, "project"), x$mode, mode)
  )
}
