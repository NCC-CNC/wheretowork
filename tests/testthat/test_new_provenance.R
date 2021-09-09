context("new_provenance")

test_that("initialization", {
  # create object
  x <- new_provenance(
    name = "a", icon = "globe", color = "#123456", description = "asdf"
  )
  # run tests
  expect_is(x, "Provenance")
  suppressMessages(x$print())
  expect_is(x$repr(), "character")
  expect_equal(x$name, "a")
  expect_equal(x$icon, "globe")
  expect_equal(x$color, "#123456")
  expect_equal(x$description, "asdf")
})

test_that("methods", {
  # create object
  x <- new_provenance(
    name = "a", icon = "globe", color = "#123456", description = "asdf"
  )
  # run tests
  expect_equal(x$export(), "a")
  expect_equal(
    x$get_widget_data(),
    list(
      name = "a",
      icon = "globe",
      color = "#123456",
      description = "asdf"
    )
  )
  expect_equal(x$export(), "a")
})

test_that("new_provenance_from_source", {
  expect_is(new_provenance_from_source("missing"), "Provenance")
  expect_is(new_provenance_from_source("regional"), "Provenance")
  expect_is(new_provenance_from_source("national"), "Provenance")
})
