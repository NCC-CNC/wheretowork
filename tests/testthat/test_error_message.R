context("error_message")

test_that("works", {
  # create object
  x <- try("a" + 1, silent = TRUE)
  expect_equal(error_message(x), "Non-numeric argument to binary operator\n")
})
