#' @include internal.R
NULL

#' Simulate a new solution
#'
#' This function simulates a [Solution] object.
#'
#' @param dataset [Dataset] object.
#'
#' @param themes `list` of [Theme] objects.
#'
#' @param weights `list` of [Weight] objects.
#'
#' @param includes `list` of [Include] objects.
#'   Defaults to an empty list such that the solution is not simulated
#'   based on any [Include] objects.
#'
#' @return A [Solution] object.
#'
#' @export
simulate_solution <- function(dataset, themes, weights, includes = list()) {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(themes),
    is.list(weights),
    is.list(includes),
    length(themes) >= 1,
    length(weights) >= 1,
    all_list_elements_inherit(themes, "Theme"),
    all_list_elements_inherit(weights, "Weight")
  )
  if (length(includes) > 0) {
    all_list_elements_inherit(includes, "Include")
  }

  # import data
  data <- dataset$get_spatial_data()
  idx <- dataset$attribute_data[["_index"]]

  # simulate statistics
  statistics <- list(
    new_statistic("Total area", stats::runif(1, 1, 1000), "ha"),
    new_statistic("Total perimeter", stats::runif(1, 1, 1000), "km")
  )

  # simulate weight results
  weight_results <- lapply(weights, function(x) {
    new_weight_results(x, held = stats::runif(1, 0.05, 0.9))
  })

  # simulate theme results
  theme_results <- lapply(themes, function(x) {
    fr <- lapply(x$feature, function(z) {
      new_feature_results(
        z,
        held = stats::runif(1, z$goal, 1.0)
      )
    })
    new_theme_results(x, feature_results = fr)
  })

  # set index names
  vidx <- paste0("solution_", sample.int(1000, 1))

  # simulate underlying data values for solution
  sold <- simulate_binary_spatial_data(data, 1)

  # ensure that includes are selected in solution
  v <- sold[[1]][idx]
  for (i in seq_along(includes)) {
    v <- pmax(v, (includes[[i]]$get_data())[[1]][idx])
  }

  # add new index to data with solution
  dataset$add_index(vidx, v)

  # create variable for solution
  v <- new_variable(
    dataset = dataset,
    index = vidx,
    units = "",
    total = sum(v),
    legend = simulate_solution_legend()
  )

  # return solution
  new_solution(
    name = sub("_", " ", vidx, fixed = TRUE),
    variable = v,
    visible = TRUE,
    parameters = list(
      new_parameter(
        name = "Total area budget",
        value = 0,
        status = FALSE,
        hide = TRUE,
        units = "%"
      ),
      new_parameter(
        name = "Spatial clustering",
        value = 0,
        units = "%"
      )
    ),
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results
  )
}
