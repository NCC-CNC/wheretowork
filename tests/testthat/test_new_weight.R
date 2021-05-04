context("new_weight")

test_that("initialization", {
  # create object
  l <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    layer = l,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  expect_equal(x$id, "FID1")
  expect_equal(x$name, "Human Footprint Index")
  expect_equal(x$layer, l)
  expect_equal(x$status, FALSE)
  expect_equal(x$initial_status, FALSE)
  expect_equal(x$factor, 0.2)
  expect_equal(x$initial_factor, 0.2)
  expect_equal(x$min_factor, 0.01)
  expect_equal(x$max_factor, 0.9)
  expect_equal(x$step_factor, 0.03)
})

test_that("get methods", {
  # create object
  l <- new_layer("asdf.txt", 0.2, 200, "ha")
  x <- new_weight(
    "Human Footprint Index", l, FALSE, 0.2, 0.01, 0.9, 0.03, "FID1")
  # run tests
  expect_identical(x$get_factor(), 0.2)
  expect_identical(x$get_status(), FALSE)
})

test_that("set methods", {
  # create object
  l <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    layer = l,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  x$set_factor(0.8)
  x$set_status(TRUE)
  expect_identical(x$get_factor(), 0.8)
  expect_identical(x$get_status(), TRUE)
})

test_that("render methods", {
  # create object
  l <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    layer = l,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  expect_identical(
    x$get_new_solution_manager_data(),
    list(
      id = "FID1",
      name = "Human Footprint Index",
      min_factor = 0.01,
      max_factor = 0.9,
      initial_factor = 0.2,
      step_factor = 0.03,
      initial_status = FALSE)
  )
})
