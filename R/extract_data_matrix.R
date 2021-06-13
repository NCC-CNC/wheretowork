#' @include internal.R
NULL

#' Extract data matrix
#'
#' Generate a `matrix` object containing data.
#'
#' @param x `list` of [Variable] objects.
#'
#' @details Note that all variables must have the same [Dataset] object.
#'
#' @return A `matrix` object.
#'
#' @export
extract_data_matrix <- function(x) {
  # assert argument is valid
  assertthat::assert_that(
    all_list_elements_inherit(x, "Variable"),
    n_distinct(vapply(x, function(x) x$dataset$id, character(1))) == 1)
  # store data
  d <- x[[1]]$dataset
  # extract variable indices
  idx <- vapply(x, function(z) which(z$index == names(d$data)), integer(1))
  # extract spatial data
  out <- d$get_indices(idx)
  # convert to matrix format
  if (inherits(d$data, "Raster")) {
    out <- out[d$get_finite_cells()]
  } else {
    out <- sf::st_drop_geometry(out)
    out <- t(as.matrix(out))
  }
  # return result
  out
}
