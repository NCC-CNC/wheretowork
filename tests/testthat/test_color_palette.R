context("color_palette")

test_that("single RColorBrewer palette (n = 5)", {
  expect_equal(
    color_palette("Greens", 5),
    RColorBrewer::brewer.pal(5, "Greens")
  )
})

test_that("single viridisLite palette (n = NULL)", {
  expect_equal(
    color_palette("Greens", NULL),
    RColorBrewer::brewer.pal(9, "Greens")
  )
})

test_that("multiple RColorBrewer palette (n = 12)", {
  expect_equal(
    color_palette("Greens;Purples", 12),
    c(
      RColorBrewer::brewer.pal(9, "Greens"),
      RColorBrewer::brewer.pal(3, "Purples")
    )
  )
})

test_that("multiple RColorBrewer palette (n = NULL)", {
  expect_equal(
    color_palette("Greens;Purples", NULL),
    c(
      RColorBrewer::brewer.pal(9, "Greens"),
      RColorBrewer::brewer.pal(9, "Purples")
    )
  )
})

test_that("multiple viridisLite palette (n = 12)", {
  expect_equal(
    color_palette("viridis", 12),
    viridisLite::viridis(12, option = "viridis")
  )
})

test_that("multiple viridisLite palette (n = NULL)", {
  expect_equal(
    color_palette("viridis", NULL),
    viridisLite::viridis(5, option = "viridis")
  )
})

test_that("multiple viridisLite palette (n = NULL)", {
  expect_equal(
    color_palette("viridis;magma", NULL),
    c(
      viridisLite::viridis(5, option = "viridis"),
      viridisLite::viridis(5, option = "magma")
    )
  )
})

test_that("random palette (n = 5)", {
  x <- color_palette("random", 5)
  expect_length(x, 5)
  expect_equal(anyDuplicated(x), 0)
})

test_that("random palette (n = NULL)", {
  x <- color_palette("random", NULL)
  expect_gt(length(x), 0)
  expect_equal(anyDuplicated(x), 0)
})
