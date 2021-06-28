context("read_configuration_file")

test_that("raster data", {
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_layers <- append(sim_themes, append(sim_weights, sim_includes))
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_configuration_file(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner")
  # import data
  x <- read_configuration_file(f1, f2, f3, f4)
  # force ids, file paths, and meta-data to be the same
  ## dataset
  x$dataset$id <- d$id
  x$dataset$spatial_path <- d$spatial_path
  x$dataset$attribute_path <- d$attribute_path
  x$dataset$boundary_path <- d$boundary_path
  x$dataset$spatial_data <- d$spatial_data
  x$dataset$attribute_data <- d$attribute_data
  x$dataset$boundary_data <- d$boundary_data
  ## themes
  for (i in seq_along(x$themes)) {
    x$themes[[i]]$id <- sim_themes[[i]]$id
    for (j in seq_along(x$themes[[i]]$feature)) {
      x$themes[[i]]$feature[[j]]$id <- sim_themes[[i]]$feature[[j]]$id
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
  }
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("spatial data", {
  # simulate data
  d <- new_dataset_from_auto(import_simple_vector_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_layers <- append(sim_themes, append(sim_weights, sim_includes))
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".shp")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_configuration_file(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner")
  # import data
  x <- read_configuration_file(f1, f2, f3, f4)
  # force ids, file paths, and meta-data to be the same
  ## dataset
  x$dataset$id <- d$id
  x$dataset$spatial_path <- d$spatial_path
  x$dataset$attribute_path <- d$attribute_path
  x$dataset$boundary_path <- d$boundary_path
  x$dataset$spatial_data <- d$spatial_data
  x$dataset$attribute_data <- d$attribute_data
  x$dataset$boundary_data <- d$boundary_data
  ## themes
  for (i in seq_along(x$themes)) {
    x$themes[[i]]$id <- sim_themes[[i]]$id
    for (j in seq_along(x$themes[[i]]$feature)) {
      x$themes[[i]]$feature[[j]]$id <- sim_themes[[i]]$feature[[j]]$id
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
  }
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})
