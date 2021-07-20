context("new_feature_results")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "Intact Alvar",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.9,
    step_goal = 0.03,
    limit_goal = 0.2,
    current = 0.56,
    id = "FID1"
  )
  x <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "RID1")
  expect_identical(x$status, FALSE)
  expect_identical(x$goal, 0.2)
  expect_identical(x$held, 0.9)
})
