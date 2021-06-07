context("new_solution")

test_that("initialization", {
  # create object
  d <- new_dataset(tempfile())
  v <- new_variable(
    d, index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")))
  x <- new_solution(
    name = "solution001",
    variable = v,
    statistics_results = data.frame(x = 1),
    theme_results = list(y = 2),
    weight_results = list(z = 3),
    id = "solution1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v)
  expect_identical(x$statistics_results, data.frame(x = 1))
  expect_identical(x$theme_results, list(y = 2))
  expect_identical(x$weight_results, list(z = 3))
  expect_identical(x$id, "solution1")
})

test_that("widget methods", {
  # create object
  d <- new_dataset(tempfile())
  v <- new_variable(
    d, index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")))
  x <- new_solution(
    name = "solution001",
    variable = v,
    statistics_results = data.frame(x = 1),
    theme_results = list(y = 2),
    weight_results = list(z = 3),
    id = "solution1")
  # run tests
  ## solution results widget
  expect_identical(
    x$get_solution_results_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      statistics_results = list(x = 1),
      theme_results = list(y = 2),
      weight_results = list(z = 3)
    )
  )
  ## map manager widget
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      statistics_results =  list(x = 1),
      legend = v$legend$get_widget_data(),
      units = v$units,
      type = "solution"
    )
  )
})

test_that("new_solution_from_prioritization", {
  # create objects
  ## raw data
  f <- system.file("extdata", "sim_vector_data.gpkg", package = "locationmisc")
  rd <- sf::read_sf(f)
  rd <- rd[seq_len(min(100, nrow(rd))), drop = FALSE]
  ## set names
  theme_names <-
    list(theme1 = names(rd)[1],
         theme2 = names(rd)[2],
         theme3 = names(rd)[3:8])
  wt_names <- names(rd)[11:15]
  ## create dataset
  d <- new_dataset(rd)
  ## create weights
  wts <- lapply(wt_names, function(x) {
    v <- new_variable_from_auto(dataset = d, index = x)
    new_weight(x, variable = v, initial_factor = runif(1, 0, 100))
  })
  ## create themes
  thms <- lapply(seq_along(theme_names), function(i) {
    fts <- lapply(theme_names[[i]], function(x) {
      v <- new_variable_from_auto(dataset = d, index = x)
      new_feature(x, variable = v, initial_goal = runif(1, 0.2, 1))
    })
    new_theme(name = names(theme_names)[[i]], feature = fts)
  })
  ## create solution settings object
  ss <- new_solution_settings(themes = thms, weights = wts)
  ## create matrices
  rij <- ss$get_rij_matrix()
  wij <- ss$get_wij_matrix()
  ## create object
  x <- new_solution_from_prioritization(
    name = "solution01", dataset = d,
    solution_settings = ss,
    rij = rij, wij = wij, boundary_data = prioritizr::boundary_matrix(rd),
    gap = 0.001, boundary_penalty_gap = 0.1)
  # run tests
  # TODO

})
