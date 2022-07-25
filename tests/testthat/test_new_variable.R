context("new_variable")

test_that("initialization", {
  skip_if_not_installed("RandomFields")
  # prepare data
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  l <- new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
  p <- new_provenance_from_source("regional")
  # create object
  x <- new_variable(
    dataset = d, index = 2, total = 12, units = "ha", legend = l,
    provenance = p
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, names(rd)[[2]])
  expect_identical(x$total, 12)
  expect_identical(x$units, "ha")
  expect_identical(x$legend, l)
})

test_that("methods", {
  skip_if_not_installed("RandomFields")
  # prepare data
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  l <- new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
  # create object
  x <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha", legend = l
  )
  # run tests
  expect_true(x$verify())
  x$index <- 1000
  expect_error(x$verify())
  x$index <- 2
  expect_equal(x$get_data(), x$dataset$get_index(2))
})

test_that("export method", {
  skip_if_not_installed("RandomFields")
  # prepare data
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  l <- new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
  p <- new_provenance_from_source("regional")
  # create object
  x <- new_variable(
    dataset = d, index = 2, total = 12, units = "ha", legend = l,
    provenance = p
  )
  # run tests
  expect_identical(
    x$export(),
    list(
      index = names(rd)[[2]],
      units = "ha",
      legend = l$export(),
      provenance = p$export()
    )
  )
})

test_that("new_variable_from_auto (continuous)", {
  skip_if_not_installed("RandomFields")
  # prepare data
  rd <- simulate_continuous_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  # create object
  x <- new_variable_from_auto(
    dataset = d, index = 2, units = "ha", colors = "viridis"
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, names(rd)[[2]])
  expect_identical(x$total, raster::cellStats(rd[[2]], "sum"))
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_continuous_legend(
      raster::cellStats(rd[[2]], "min"),
      raster::cellStats(rd[[2]], "max"),
      color_palette("viridis", 5)
    )
  )
})

test_that("new_variable_from_auto (categorical)", {
  skip_if_not_installed("RandomFields")
  # prepare data
  rd <- simulate_categorical_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  # create object
  x <- new_variable_from_auto(
    dataset = d, index = 1, units = "ha", colors = "viridis"
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$total, raster::cellStats(rd[[1]], "sum"))
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_categorical_legend(
      seq_len(raster::cellStats(rd[[1]], "max")),
      color_palette("viridis", raster::cellStats(rd[[1]], "max"))
    )
  )
})

test_that("new_variable_from_auto (categorical, manual legend)", {
  skip_if_not_installed("RandomFields")
  # prepare data
  rd <- simulate_categorical_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  # extract first column in attribute data
  values <- d$attribute_data[[1]]
  # create character vector of unique values (c("value: 1", ...))
  labels <- paste("value: ", as.character(c(na.omit(unique(values)))), sep = "")
  # create character vector of color pallet, same length as labels
  cp <- color_palette(x = "random", n = length(labels))
  # create object
  x <- new_variable_from_auto(
    dataset = d, index = 1, units = "ha", type = "manual", colors = cp, 
    labels = labels
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$total, raster::cellStats(rd[[1]], "sum"))
  expect_identical(x$units, "ha")
  expect_equal(
    length(x$legend$labels),
    length(x$legend$colors)
  )
})

test_that("new_variable_from_metadata (continuous)", {
  # prepare data
  rd <- import_simple_raster_data()
  d <- new_dataset_from_auto(rd)
  # create object
  x <- new_variable_from_metadata(
    dataset = d,
    metadata = list(
      index = 1, type = "continuous", units = "ha", colors = "viridis",
      min_value = 1, max_value = 5, total = 11, provenance = "missing", 
      labels = "missing"
    )
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, names(rd)[[1]])
  expect_identical(x$total, 11)
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_continuous_legend(1, 5, color_palette("viridis", 5))
  )
})

test_that("new_variable_from_metadata (categorical)", {
  # prepare data
  rd <- import_simple_raster_data()
  d <- new_dataset_from_auto(rd)
  # create object
  x <- new_variable_from_metadata(
    dataset = d,
    metadata = list(
      index = 1, type = "categorical", units = "ha", colors = "viridis",
      total = 11, values = seq(1, 6), provenance = "missing", labels = "missing"
    )
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, names(rd)[[1]])
  expect_identical(x$total, 11)
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_categorical_legend(seq(1, 6), color_palette("viridis", 6))
  )
})

test_that("new_variable_from_metadata (categorical, manual legend)", {
  # prepare data
  rd <- import_simple_raster_data()
  d <- new_dataset_from_auto(rd)
  # create object
  x <- new_variable_from_metadata(
    dataset = d,
    metadata = list(
      index = 1, type = "manual", units = "ha", 
      colors = c("#edf8fb", "#ccece6", "#99d8c9", "#66c2a4", "#2ca25f",
                 "#006d2c"),
      total = 11, values = seq(1, 6), provenance = "missing", 
      labels = c("value: 1", "value: 2", "value: 3", "value: 4", "value: 5",
                 "value: 6")
    )
  )
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, names(rd)[[1]])
  expect_identical(x$total, 11)
  expect_identical(x$units, "ha")
  expect_equal(
    length(x$legend$labels),
    length(x$legend$colors)
  )
})

test_that("render (project on the fly)", {
  # find data file paths
  f1 <- system.file(
    "extdata", "projects", "ontario_pilot_albers", "ontario_pilot_albers_spatial.tif",
    package = "wheretowork"
  )
  f2 <- system.file(
    "extdata",  "projects", "ontario_pilot_albers", "ontario_pilot_albers_attribute.csv.gz",
    package = "wheretowork"
  )
  f3 <- system.file(
    "extdata",  "projects", "ontario_pilot_albers", "ontario_pilot_albers_boundary.csv.gz",
    package = "wheretowork"
  )
  # create object
  d <- new_dataset(f1, f2, f3)
  v <- new_variable_from_auto(dataset = d, index = "R1km_Habitat_Forest", units = "km2")
  # render on map
  l <- leaflet::leaflet() %>% leaflet::addTiles()
  m <- v$render(x = l, id = "id", zindex = 1000, visible = TRUE)
  # run tests
  expect_is(m, "leaflet")
})

