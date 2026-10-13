context("calculate_area_budget_data")

test_that("budget exceeded by locked in planning units", {
  # create data
  area_data <- as.numeric(1:10)
  include_data <- Matrix::Matrix(
    matrix(c(rep(0, 5), rep(1, 5)), nrow = 1, ncol = 10), sparse = TRUE
  )
  include_settings <- tibble::tibble(id = "I1", status = TRUE)
  exclude_data <- Matrix::Matrix(matrix(0, nrow = 0, ncol = 10), sparse = TRUE)
  exclude_settings <- tibble::tibble(
    id = character(0), status = logical(0)
  )
  # calculate result
  x <- calculate_area_budget_data(
    area_data = area_data,
    include_data = include_data,
    include_settings = include_settings,
    exclude_data = exclude_data,
    exclude_settings = exclude_settings,
    overlap = FALSE,
    area_budget_proportion = 0.1,
    boundary_gap = 0
  )
  # run tests
  expect_identical(x$locked_in, c(rep(FALSE, 5), rep(TRUE, 5)))
  expect_identical(x$locked_out, rep(FALSE, 10))
  expect_true(x$exceeded)
})

test_that("budget not exceeded given sufficient budget", {
  # create data
  area_data <- as.numeric(1:10)
  include_data <- Matrix::Matrix(
    matrix(c(rep(0, 5), rep(1, 5)), nrow = 1, ncol = 10), sparse = TRUE
  )
  include_settings <- tibble::tibble(id = "I1", status = TRUE)
  exclude_data <- Matrix::Matrix(matrix(0, nrow = 0, ncol = 10), sparse = TRUE)
  exclude_settings <- tibble::tibble(
    id = character(0), status = logical(0)
  )
  # calculate result
  x <- calculate_area_budget_data(
    area_data = area_data,
    include_data = include_data,
    include_settings = include_settings,
    exclude_data = exclude_data,
    exclude_settings = exclude_settings,
    overlap = FALSE,
    area_budget_proportion = 1,
    boundary_gap = 0
  )
  # run tests
  expect_false(x$exceeded)
})

test_that("override includes excuses overlapping area from the budget", {
  # create data: an include and an exclude that fully overlap on the
  # same (large) set of planning units
  area_data <- as.numeric(1:10)
  include_data <- Matrix::Matrix(
    matrix(c(rep(0, 5), rep(1, 5)), nrow = 1, ncol = 10), sparse = TRUE
  )
  include_settings <- tibble::tibble(id = "I1", status = TRUE)
  exclude_data <- Matrix::Matrix(
    matrix(c(rep(0, 5), rep(1, 5)), nrow = 1, ncol = 10), sparse = TRUE
  )
  exclude_settings <- tibble::tibble(id = "E1", status = TRUE)
  # calculate result without overriding includes (excludes lose precedence)
  x1 <- calculate_area_budget_data(
    area_data = area_data,
    include_data = include_data,
    include_settings = include_settings,
    exclude_data = exclude_data,
    exclude_settings = exclude_settings,
    overlap = FALSE,
    area_budget_proportion = 0.1,
    boundary_gap = 0
  )
  # calculate result with overriding includes (excludes win precedence)
  x2 <- calculate_area_budget_data(
    area_data = area_data,
    include_data = include_data,
    include_settings = include_settings,
    exclude_data = exclude_data,
    exclude_settings = exclude_settings,
    overlap = TRUE,
    area_budget_proportion = 0.1,
    boundary_gap = 0
  )
  # run tests
  ## without the override, the include's full extent is locked in and
  ## the small budget is exceeded
  expect_identical(x1$locked_in, c(rep(FALSE, 5), rep(TRUE, 5)))
  expect_true(x1$exceeded)
  ## with the override, the overlapping planning units are locked out
  ## instead, so nothing is locked in and the same small budget passes
  expect_identical(x2$locked_in, rep(FALSE, 10))
  expect_identical(x2$locked_out, c(rep(FALSE, 5), rep(TRUE, 5)))
  expect_false(x2$exceeded)
})
