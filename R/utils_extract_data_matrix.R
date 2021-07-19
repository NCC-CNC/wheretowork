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
    is.list(x),
    all_list_elements_inherit(x, "Variable"),
    n_distinct(vapply(x, function(z) z$dataset$id, character(1))) == 1)
  # get attribute data
  d <- x[[1]]$dataset$get_attribute_data()
  # extract variable indices
  idx <- vapply(x, FUN.VALUE = integer(1), function(z) {
    if (is.numeric(z$index)) return(as.integer(z$index))
    which(z$index == names(d))
  })
  # return result
  methods::as(t(as.matrix(d[, idx, drop = FALSE])), "dgCMatrix")
}
