context("new_parameter")

test_that("initialization", {
  # create object
  x <- new_parameter(
    name = "Spatial clumping",
    status = FALSE,
    value = 0.2,
    min_value = 0.01,
    max_value = 0.9,
    step_value = 0.03,
    units = "asdf",
    hide = TRUE,
    id = "P1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$id, "P1")
  expect_equal(x$name, "Spatial clumping")
  expect_equal(x$status, FALSE)
  expect_equal(x$value, 0.2)
  expect_equal(x$min_value, 0.01)
  expect_equal(x$max_value, 0.9)
  expect_equal(x$step_value, 0.03)
  expect_equal(x$hide, TRUE)
  expect_equal(x$units, "asdf")
})

test_that("get methods", {
  # create object
  x <- new_parameter(
    name = "Spatial clumping",
    status = FALSE,
    value = 0.2,
    min_value = 0.01,
    max_value = 0.9,
    step_value = 0.03,
    id = "P1"
  )
  # run tests
  expect_identical(x$get_value(), 0.2)
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_setting("value"), x$get_value())
  expect_identical(x$get_setting("status"), x$get_status())
})

test_that("set methods", {
  # create object
  x <- new_parameter(
    name = "Spatial clumping",
    status = FALSE,
    value = 0.2,
    min_value = 0.01,
    max_value = 0.9,
    step_value = 0.03,
    id = "P1"
  )
  # run tests
  x$set_value(0.8)
  x$set_status(TRUE)
  expect_identical(x$get_value(), 0.8)
  expect_identical(x$get_status(), TRUE)
  x$set_setting("value", 0.3)
  x$set_setting("status", FALSE)
  expect_identical(x$get_value(), 0.3)
  expect_identical(x$get_status(), FALSE)
})

test_that("export method", {
  # create object
  x <- new_parameter(
    name = "Spatial clumping",
    status = FALSE,
    value = 0.2,
    min_value = 0.01,
    max_value = 0.9,
    step_value = 0.03,
    hide = TRUE,
    units = "asdf",
    id = "P1"
  )
  # run tests
  expect_equal(
    x$export(),
    list(
      name = "Spatial clumping",
      status = FALSE,
      value = 0.2,
      min_value = 0.01,
      max_value = 0.9,
      step_value = 0.03,
      hide = TRUE,
      units = "asdf"
    )
  )
})

test_that("widget methods", {
  # create object
  x <- new_parameter(
    name = "Spatial clumping",
    status = FALSE,
    value = 0.2,
    min_value = 0.01,
    max_value = 0.9,
    step_value = 0.03,
    units = "asdf",
    hide = TRUE,
    id = "P1"
  )
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "P1",
      name = "Spatial clumping",
      min_value = 0.01,
      max_value = 0.9,
      value = 0.2,
      step_value = 0.03,
      status = FALSE,
      hide = TRUE,
      units = "asdf"
    )
  )
})
