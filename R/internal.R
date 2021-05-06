#' All elements inherit
#'
#' Check if all elements in a `list` object inherit from a class.
#'
#' @param x `list` object.
#'
#' @param class `character` class name.
#
#' @return `logical` indicating if all elements in `x` inherit from `class`.
#'
#' @noRd
all_list_elements_inherit <- function(x, class) {
  assertthat::assert_that(
    is.list(x),
    assertthat::is.string(class),
    assertthat::noNA(class))
  all(vapply(x, inherits, logical(1), class))
}

assertthat::on_failure(all_list_elements_inherit) <- function(call, env) {
  paste0("all ", deparse(call$x), " do not inherit from", deparse(call$class))
}

# Return the correct new line character symbol for the system
nl <- function() {
  ifelse(identical(.Platform$OS.type, "unix"), "\n", "\n\r")
}

# alias for dplr::n_distinct()
n_distinct <- function(x) {
  length(unique(x))
}
