context("write_excel_workbook")

test_that("single worksheet", {
  # create data
  f <- tempfile(fileext = ".xlsx")
  r <- write_excel_workbook(iris, f)
  # run tests
  expect_true(r)
  expect_true(file.exists(f))
})

test_that("multiple worksheet (default names)", {
  # create data
  f <- tempfile(fileext = ".xlsx")
  r <- write_excel_workbook(list(iris, mtcars), f)
  # run tests
  expect_true(r)
  expect_true(file.exists(f))
})

test_that("multiple worksheet (specified names)", {
  # create data
  f <- tempfile(fileext = ".xlsx")
  r <- write_excel_workbook(list(Iris = iris, Mtcars = mtcars), f)
  # run tests
  expect_true(r)
  expect_true(file.exists(f))
})
