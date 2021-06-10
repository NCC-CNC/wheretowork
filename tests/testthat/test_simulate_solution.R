context("simulate_solution")

test_that("success", {
  # create object
  sim_data <- simulate_data(3, 2, 3)
  x <- simulate_solution(themes = sim_data$themes, weights = sim_data$weights)
  # run tests
  expect_is(x, "Solution")
})
