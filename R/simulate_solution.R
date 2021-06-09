#' @include internal.R
NULL

#' Simulate a new solution
#'
#' This function simulates a [Solution] object.
#' It is primarily used for testing the package.
#'
#' @param themes `list` of [Theme] objects.
#'
#' @param weights `list` of [Weight] objects.
#'
#' @return A [Solution] object.
#'
#' @export
simulate_solution <- function(themes, weights) {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(themes),
    is.list(weights),
    length(themes) >= 1,
    length(weights) >= 1,
    all_list_elements_inherit(themes, "Theme"),
    all_list_elements_inherit(weights, "Weight"))

  # simulate statistics
  statistics <- list(
    new_statistic("Total area", stats::runif(1, 1, 1000), "ha"),
    new_statistic("Total perimeter", stats::runif(1, 1, 1000), "km"))

  # simulate weight results
  weight_results <- lapply(weights, function(x) {
    new_weight_results(x, held = stats::runif(1, 0.05, 0.9))

  })

  # simulate theme results
  theme_results <- lapply(themes, function(x) {
    fr <- lapply(x$feature, function(z) {
      new_feature_results(z, held = stats::runif(1, 0.05, 0.9))
    })
    new_theme_results(x, feature_results = fr)
  })

  # return solution
  n <- paste0("solution_", sample.int(1000, 1))
  v <- new_variable(
    dataset = weights[[1]]$variable$dataset,
    index = n, units = "ha", total =  1005,
    legend = new_categorical_legend(
      values = c(0, 1),  colors = c("#FFFFFF", "#000000")))
  new_solution(
    name = n,
    variable = v,
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results)
}
