context("test_simulate_solution_settings")

test_that("small example", {
  # define paramters
  n_st <- 2
  n_mt <- 1
  n_w <- 3
  # create object
  x <- simulate_solution_settings(n_st, n_mt, n_w)
  # run tests
  expect_is(x, "SolutionSettings")
  expect_length(x$themes, n_st + n_mt)
  expect_length(x$weights, n_w)
  expect_equal(
    sum(vapply(
      x$themes, function(x) inherits(x, "SingleTheme"),  logical(1))), n_st)
  expect_equal(
    sum(vapply(
      x$themes, function(x) inherits(x, "MultiTheme"),  logical(1))), n_mt)
})

test_that("large example", {
  # define paramters
  n_st <- 20
  n_mt <- 30
  n_w <- 10
  # create object
  x <- simulate_solution_settings(n_st, n_mt, n_w)
  # run tests
  expect_is(x, "SolutionSettings")
  expect_length(x$themes, n_st + n_mt)
  expect_length(x$weights, n_w)
  expect_equal(
    sum(vapply(
      x$themes, function(x) inherits(x, "SingleTheme"),  logical(1))), n_st)
  expect_equal(
    sum(vapply(
      x$themes, function(x) inherits(x, "MultiTheme"),  logical(1))), n_mt)
})
