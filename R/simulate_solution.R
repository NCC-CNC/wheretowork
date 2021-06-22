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
#' @return A [Solution] object.
#'
#' @export
simulate_solution <- function(dataset, themes, weights) {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(themes),
    is.list(weights),
    length(themes) >= 1,
    length(weights) >= 1,
    all_list_elements_inherit(themes, "Theme"),
    all_list_elements_inherit(weights, "Weight"))

  # import data
  data <- dataset$get_data()

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
      new_feature_results(
        z,
        held = stats::runif(1, z$goal, 1.0)
      )
    })
    new_theme_results(x, feature_results = fr)
  })

  # set index names
  idx <- paste0("solution_", sample.int(1000, 1))

  # simulate underlying data values
  sold <- simulate_binary_spatial_data(data, 1)
  names(sold)[1] <- idx
  if (inherits(data, "sf")) {
    dataset$data <- cbind(data, sf::st_drop_geometry(sold))
    total <- sum(sold[[1]])
  } else {
    dataset$data <- raster::stack(data, sold)
    total <- sum(raster::values(sold), na.rm = TRUE)
  }

  # create variable for
  v <- new_variable(
    dataset = dataset,
    index = idx,
    units = "",
    total = total,
    legend = simulate_solution_legend()
  )

  # return solution
  new_solution(
    name = sub("_", " ", idx, fixed = TRUE),
    variable = v,
    initial_visible = TRUE,
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results
  )
}
