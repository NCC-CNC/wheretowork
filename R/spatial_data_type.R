#' @include internal.R
NULL

#' Spatial data type
#'
#' Identify if a spatial dataset has continuous or categorical data.
#'
#' @param x [sf::st_sf()] or [raster::stack()] dataset object.
#'
#' @param subset `integer` or `character` value indicating the
#'   field or layer for which to calculate statistics.
#'   Defaults to 1, such that the first field/layer is used to calculate
#'   statistics.
#'
#' @param max_sample `integer` maximum number of cells in a raster stack
#'  to sample when checking if the data are continuous or categorical.
#'  Defaults to 1000.
#'
#' @return A `character` indicating if the data are `"continuous"` or
#'   `"categorical"`.
#'
#' @export
spatial_data_type <- function(x, subset = 1) {
  UseMethod("spatial_data_type", x)
}

#' @rdname spatial_data_type
#' @export
spatial_data_type.sf <- function(x, subset = 1) {
  # assert valid arguments
  asserthat::assert_that(
    inherits(x, "sf"),
    asserthat::is.string(subset) || asserthat::is.count(subset),
    asserthat::noNA(subset))
  if (is.character(subset)) {
    asserthat::assert_that(
      asserthat::has_name(x, subset))
  }

  # determine if data are continuous or categorical
  ## if there are more than 20 unique values, then we assume it's continuous
  out <-
    ifelse(dplyr::n_distinct(x[[subset]]) > 20, "continuous", "categorical")

  # return result
  out
}

#' @rdname spatial_data_type
#' @export
spatial_data_type.Raster <- function(x, subset = 1, max_sample = 1000) {
  # assert valid arguments
  asserthat::assert_that(
    inherits(x, "Raster"),
    asserthat::is.string(type),
    asserthat::noNA(type),
    asserthat::is.count(max_sample),
    asserthat::noNA(max_sample),
    type %in% c("continuous", "categorical"),
    asserthat::is.string(subset) || asserthat::is.count(subset),
    asserthat::noNA(subset))
  if (is.character(subset)) {
    asserthat::assert_that(
      subset %in% names(x))
  }

  # convert subset to integer if field name supplied
  subset <- which(names(x) == subset)

  # extract layer
  x <- x[[subset]]

  # identify cells with no missing values
  cells <- raster::Which(!is.na(x), cells = TRUE)
  if (length(cells) > max_sample) {
    cells <- sample(cells, max_sample)
  }

  # determine if data are continuous or categorical
  ## if there are more than 20 unique values, then we assume it's continuous
  out <-
    ifelse(dplyr::n_distinct(x[cells]) > 20, "continuous", "categorical")

  # return result
  out
}
