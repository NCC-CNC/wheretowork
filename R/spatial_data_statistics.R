#' @include internal.R
NULL

#' Spatial data statistics
#'
#' Calculate statistics for a spatial dataset.
#'
#' @param x [sf::st_sf()] or [raster::stack()] dataset object.
#'
#' @param type `character` indicating whether the dataset contains
#'   continuous (i.e. `"continuous"`) or categorical (i.e. `"categorical"`)
#'   data.
#'
#' @param subset `integer` or `character` value indicating the
#'   field or layer for which to calculate statistics.
#'   Defaults to 1, such that the first field/layer is used to calculate
#'   statistics.
#'
#' @return A `list` containing statistics for the data.
#'
#' @export
spatial_data_statistics <- function(x, type, subset = 1) {
  UseMethod("spatial_data_statistics", x)
}

#' @rdname spatial_data_statistics
#' @export
spatial_data_statistics.sf <- function(x, type, subset = 1) {
  # assert valid arguments
  asserthat::assert_that(
    inherits(x, "sf"),
    asserthat::is.string(type),
    asserthat::noNA(type),
    type %in% c("continuous", "categorical"),
    asserthat::is.string(subset) || asserthat::is.count(subset),
    asserthat::noNA(subset)
  )
  if (is.character(subset)) {
    asserthat::assert_that(
      asserthat::has_name(x, subset))
  }

  # convert subset to integer if field name supplied
  subset <- which(names(x) == subset)

  # calculate statistics
  if (identical(type, "continuous")) {
    ## continuous data
    out <- list(
      total = sum(x[[subset]], na.rm = TRUE),
      max_value = min(x[[subset]], na.rm = TRUE),
      max_value = max(x[[subset]], na.rm = TRUE))
  } else {
    ## categorical data
    out <- list(
      total = sum(x[[subset]], na.rm = TRUE),
      values = unique(x[[subset]], na.rm = TRUE))
  }

  # return result
  out
}

#' @rdname spatial_data_statistics
#' @export
spatial_data_statistics.Raster <- function(x, type, subset = 1) {
  # assert valid arguments
  asserthat::assert_that(
    inherits(x, "Raster"),
    asserthat::is.string(type),
    asserthat::noNA(type),
    type %in% c("continuous", "categorical"),
    asserthat::is.string(subset) || asserthat::is.count(subset),
    asserthat::noNA(subset)
  )
  if (is.character(subset)) {
    asserthat::assert_that(
      subset %in% names(x))
  }

  # convert subset to integer if field name supplied
  subset <- which(names(x) == subset)

  # calculate statistics
  if (identical(type, "continuous")) {
    ## continuous data
    out <- list(
      total = raster::cellStats(x[[subset]], "sum"),
      max_value = raster::cellStats(x[[subset]], "min"),
      max_value = raster::cellStats(x[[subset]], "max"))
  } else {
    ## categorical data
    out <- list(
      total = raster::cellStats(x[[subset]], "sum"),
      values = raster::unique(x[[subset]], na.last = NA))
  }

  # return result
  out
}
