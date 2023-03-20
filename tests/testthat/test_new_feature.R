context("new_feature")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend()
  )
  x <- new_feature(
    name = "Intact Alvar",
    variable = v,
    visible = FALSE,
    hidden = TRUE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.2,
    current = 0.56,
    id = "FID1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "Intact Alvar")
  expect_identical(x$variable, v)
  expect_identical(x$visible, FALSE)
  expect_identical(x$loaded, FALSE)
  expect_identical(x$invisible, NA_real_)
  expect_identical(x$hidden, TRUE)
  expect_identical(x$status, FALSE)
  expect_identical(x$goal, 0.2)
  expect_identical(x$limit_goal, 0.2)
  expect_identical(x$current, 0.56)
  expect_identical(x$id, "FID1")
})

test_that("get methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend()
  )
  x <- new_feature(
    name = "Intact Alvar",
    variable = v,
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.2,
    current = 0.56,
    id = "FID1"
  )
  # run tests
  expect_identical(x$get_goal(), 0.2)
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_loaded(), TRUE)
  expect_identical(x$get_invisible(), NA_real_)
  expect_identical(x$get_hidden(), FALSE)
  expect_identical(x$get_current(), 0.56)
})

test_that("set methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend()
  )
  x <- new_feature(
    name = "Intact Alvar",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.2,
    current = 0.56,
    id = "FID1"
  )
  # run tests
  x$set_goal(0.8)
  x$set_status(TRUE)
  x$set_visible(FALSE)
  x$set_loaded(TRUE)
  x$set_invisible(100)
  x$set_current(0.77)
  expect_identical(x$get_goal(), 0.8)
  expect_identical(x$get_status(), TRUE)
  expect_identical(x$get_visible(), FALSE)
  expect_identical(x$get_invisible(), 100)
  expect_identical(x$get_loaded(), TRUE)
  expect_identical(x$get_current(), 0.77)
})

test_that("export method", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend()
  )
  x <- new_feature(
    name = "Intact Alvar",
    variable = v,
    visible = FALSE,
    hidden = TRUE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.2,
    id = "FID1"
  )
  # run tests
  expect_identical(
    x$export(),
    list(
      name = "Intact Alvar",
      variable = x$variable$export(),
      status = FALSE,
      visible = FALSE,
      hidden = TRUE,
      goal = 0.2,
      limit_goal = 0.2
    )
  )
})
