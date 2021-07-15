context("acknowledge_package")

test_that("works", {
  expect_is(acknowledge_packages(c("htmltools", "shiny")), "shiny.tag")
})
