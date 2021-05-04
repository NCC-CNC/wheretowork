context("new_feature")

test_that("initialization", {
  # create object
  l <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  x <- new_feature(
    name = "Intact Alvar",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.9,
    step_goal = 0.03,
    limit_goal = 0.2,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  # run tests
  expect_identical(x$name, "Intact Alvar")
  expect_identical(x$layer, l)
  expect_identical(x$status, FALSE)
  expect_identical(x$initial_status, FALSE)
  expect_identical(x$goal, 0.2)
  expect_identical(x$initial_goal, 0.2)
  expect_identical(x$min_goal, 0.01)
  expect_identical(x$max_goal, 0.9)
  expect_identical(x$step_goal, 0.03)
  expect_identical(x$limit_goal, 0.2)
  expect_identical(x$current_label, "Now")
  expect_identical(x$icon, shiny::icon("bell"))
  expect_identical(x$id, "FID1")
})

test_that("get methods", {
  # create object
  l <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  x <- new_feature(
    name = "Intact Alvar",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.9,
    step_goal = 0.03,
    limit_goal = 0.2,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  # run tests
  expect_identical(x$get_goal(), 0.2)
  expect_identical(x$get_status(), FALSE)
})

test_that("set methods", {
  # create object
  l <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  x <- new_feature(
    name = "Intact Alvar",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.9,
    step_goal = 0.03,
    limit_goal = 0.2,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  # run tests
  x$set_goal(0.8)
  x$set_status(TRUE)
  expect_identical(x$get_goal(), 0.8)
  expect_identical(x$get_status(), TRUE)
})
