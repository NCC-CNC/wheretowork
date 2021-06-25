context("simulate_solution")

test_that("raster dataset (no includes)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  x <- simulate_solution(d, sim_themes, sim_weights)
  # run tests
  expect_is(x, "Solution")
})

test_that("vector dataset (no includes)", {
  # create object
  d <- new_dataset_from_auto(import_simple_vector_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  x <- simulate_solution(d, sim_themes, sim_weights)
  # run tests
  expect_is(x, "Solution")
})

test_that("raster dataset (includes)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  x <- simulate_solution(d, sim_themes, sim_weights, sim_includes)
  # run tests
  expect_is(x, "Solution")
})

test_that("vector dataset (includes)", {
  # create object
  d <- new_dataset_from_auto(import_simple_vector_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  x <- simulate_solution(d, sim_themes, sim_weights, sim_includes)
  # run tests
  expect_is(x, "Solution")
})
