context("simulate_solution")

test_that("simple dataset", {
  # create object
  d <- new_dataset(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  x <- simulate_solution(d, sim_themes, sim_weights)
  # run tests
  expect_is(x, "Solution")
})

test_that("simple dataset", {
  # create object
  d <- new_dataset(import_realistic_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  x <- simulate_solution(d, sim_themes, sim_weights)
  # run tests
  expect_is(x, "Solution")
})
