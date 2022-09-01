#' @include internal.R
NULL

#' Spatial data type
#'
#' Identify if a spatial dataset has continuous or categorical data.
#'
#' @param x [sf::st_sf()] or [raster::stack()] or [data.frame] dataset object.
#'
#' @param index `integer` or `character` value indicating the
#'   field or layer for which to calculate statistics.
#'   Defaults to 1, such that the first field/layer is used to calculate
#'   statistics.
#'
#' @param max_sample `integer` maximum number of cells in a raster stack
#'  to sample when checking if the data are continuous or categorical.
#'  Defaults to 10000.
#'
#' @param ... not used.
#'
#' @return A `character` indicating if the data are `"continuous"` or
#'   `"categorical"`.
#'
#' @export
spatial_data_type <- function(x, index = 1, ...) {
  UseMethod("spatial_data_type", x)
}

#' @rdname spatial_data_type
#' @export
spatial_data_type.sf <- function(x, index = 1, ...) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::is.string(index) || assertthat::is.count(index),
    assertthat::noNA(index)
  )
  if (is.character(index)) {
    assertthat::assert_that(
      assertthat::has_name(x, index)
    )
  }

  # determine if data are continuous or categorical
  ## if there are more than 20 unique values, then we assume it's continuous
  out <-
    ifelse(n_distinct(x[[index]]) > 20, "continuous", "categorical")

  # return result
  out
}

#' @rdname spatial_data_type
#' @export
spatial_data_type.Raster <- function(x, index = 1, max_sample = 10000, ...) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "Raster"),
    assertthat::is.count(max_sample),
    assertthat::noNA(max_sample),
    assertthat::is.string(index) || assertthat::is.count(index),
    assertthat::noNA(index)
  )
  if (is.character(index)) {
    assertthat::assert_that(
      index %in% names(x)
    )
    index <- which(names(x) == index)
  }

  # extract layer
  x <- x[[index]]

  # identify cells with no missing values
  cells <- raster::Which(!is.na(x), cells = TRUE)
  if (length(cells) > max_sample) {
    cells <- sample(cells, max_sample)
  }

  # determine if data are continuous or categorical
  ## if there are more than 20 unique values, then we assume it's continuous
  out <-
    ifelse(n_distinct(x[cells]) > 20, "continuous", "categorical")

  # return result
  out
}

#' @rdname spatial_data_type
#' @export
spatial_data_type.data.frame <- function(x, index = 1, ...) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, c('tbl_df', 'tbl', 'data.frame')),
    assertthat::is.string(index) || assertthat::is.count(index),
    assertthat::noNA(index)
  )
  if (is.character(index)) {
    assertthat::assert_that(
      assertthat::has_name(x, index)
    )
  }
  
  # determine if data are continuous or categorical
  ## if there are more than 20 unique values, then we assume it's continuous
  out <-
    ifelse(n_distinct(x[[index]]) > 20, "continuous", "categorical")
  
  # return result
  out
}
