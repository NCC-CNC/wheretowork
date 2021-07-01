context("prepare_fileInput")

test_that("works", {
  # create mock data
  x <- data.frame(
    stringsAsFactors = FALSE,
    datapath = c(tempfile(fileext = ".gz"), tempfile(fileext = ".tif")),
    name = c("file1.csv.gz", "file2.tif"))
  writeLines("test1", x$datapath[[1]])
  writeLines("test2", x$datapath[[2]])
  # prepare files
  prepare_fileInput(x)
  # run tests
  expect_true(file.exists(file.path(dirname(x$datapath[[1]]), x$name[[1]])))
  expect_true(file.exists(file.path(dirname(x$datapath[[2]]), x$name[[2]])))
  expect_equal(
    "test1",
    readLines(file.path(dirname(x$datapath[[1]]), x$name[[1]]))
  )
  expect_equal(
    "test2",
    readLines(file.path(dirname(x$datapath[[2]]), x$name[[2]]))
  )
})
