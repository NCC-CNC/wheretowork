context("read_project")

test_that("raster (standard boundary format, continuous)", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2, continuous = TRUE)
  sim_themes <- simulate_themes(d, 2, 2, 2, continuous = TRUE)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)
  # manually insert periods into they don't cause issues
  sim_themes[[1]]$name <- "fake_fake_theme_"
  sim_themes[[2]]$feature[[1]]$name <- "fake_fake_feature_"
  sim_weights[[1]]$name <- "fake_fake_weight_"
  sim_includes[[2]]$name <- "fake_fake_include_"
  sim_excludes[[2]]$name <- "fake_fake_exclude_"
  # compile layers
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".dat.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("raster (standard boundary format, categorical, manual legend)", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2, continuous = FALSE)
  sim_themes <- simulate_themes(d, 2, 2, 2, continuous = FALSE)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)
  # manually insert periods into they don't cause issues
  sim_themes[[1]]$name <- "fake_fake_theme_"
  sim_themes[[2]]$feature[[1]]$name <- "fake_fake_feature_"
  sim_weights[[1]]$name <- "fake_fake_weight_"
  sim_includes[[2]]$name <- "fake_fake_include_"
  sim_excludes[[2]]$name <- "fake_fake_exclude_"
  # compile layers
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".dat.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)  
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("raster (standard boundary format, continous & categorical)", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2, continuous = FALSE)
  sim_themes <- simulate_themes(d, 2, 2, 2, continuous = NA)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)  
  # manually insert periods into they don't cause issues
  sim_themes[[1]]$name <- "fake_fake_theme_"
  sim_themes[[2]]$feature[[1]]$name <- "fake_fake_feature_"
  sim_weights[[1]]$name <- "fake_fake_weight_"
  sim_includes[[2]]$name <- "fake_fake_include_"
  sim_excludes[[2]]$name <- "fake_fake_exclude_"  
  # compile layers
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".dat.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)  
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("sf (standard boundary format)", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_vector_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".shp")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".dat.gz")
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)  
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("raster (Marxan boundary format)", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)  
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("sf (Marxan boundary format)", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_vector_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".shp")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("default spatial, boundary, attribute paths", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)  
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner"
  )
  # import data
  x <- read_project(f1)
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
      x$themes[[i]]$feature[[j]]$pane <- sim_themes[[i]]$feature[[j]]$pane
    }
  }
  ## weights
  for (i in seq_along(x$weights)) {
    x$weights[[i]]$id <- sim_weights[[i]]$id
    x$weights[[i]]$pane <- sim_weights[[i]]$pane
  }
  ## includes
  for (i in seq_along(x$includes)) {
    x$includes[[i]]$id <- sim_includes[[i]]$id
    x$includes[[i]]$pane <- sim_includes[[i]]$pane
  }
  ## excludes
  for (i in seq_along(x$excludes)) {
    x$excludes[[i]]$id <- sim_excludes[[i]]$id
    x$excludes[[i]]$pane <- sim_excludes[[i]]$pane
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)  
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
})

test_that("contact details", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)  
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner",
    author_name = "Greg McGregerson",
    author_email = "greg.mcgregerson@supergreg.greg"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
  # tests
  expect_equal(x$author_name, "Greg McGregerson")
  expect_equal(x$author_email, "greg.mcgregerson@supergreg.greg")
})

test_that("provenance", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)  
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner",
    author_name = "Greg McGregerson",
    author_email = "greg.mcgregerson@supergreg.greg"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
  # tests
  expect_equal(x$author_name, "Greg McGregerson")
  expect_equal(x$author_email, "greg.mcgregerson@supergreg.greg")
})

test_that("force hide theme, weight, include and exclude", {
  skip_if_not_installed("RandomFields")
  # simulate data
  d <- new_dataset_from_auto(import_simple_raster_data())
  sim_weights <- simulate_weights(d, 2)
  sim_themes <- simulate_themes(d, 2, 2, 2)
  sim_includes <- simulate_includes(d, 2)
  sim_excludes <- simulate_excludes(d, 2)
  sim_layers <- append(append(sim_themes, append(sim_weights, sim_includes)), sim_excludes)
  # manually calculate current amount held
  ss <- new_solution_settings(sim_themes, sim_weights, sim_includes, sim_excludes, list())
  ss$update_current_held()
  # manually set weight factors
  sim_weights <- lapply(sim_weights, function(x) {
    x$factor <- round(runif(1, 0.1), 3)
    x
  })
  # generate file paths
  f1 <- tempfile(fileext = ".yaml")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".csv.gz")
  f4 <- tempfile(fileext = ".csv.gz")
  # save configuration file
  write_project(
    x = sim_layers,
    dataset = d,
    name = "test",
    f1, f2, f3, f4,
    mode = "beginner",
    author_name = "Greg McGregerson",
    author_email = "greg.mcgregerson@supergreg.greg"
  )
  # import data
  x <- read_project(f1, f2, f3, f4, force_hidden = TRUE)
  # tests
  for (theme in x$themes) {
    ## features
    for (feature in theme$feature)
      expect_is(feature$variable$legend, "NullLegend")
      expect_identical(feature$hidden, TRUE)
  }
  ## weights
  for (weight in x$weights) {
    expect_is(weight$variable$legend, "NullLegend")
    expect_identical(weight$hidden, TRUE)
  }
  ## includes
  for (include in x$includes) {
    expect_is(include$variable$legend, "NullLegend")
    expect_identical(include$hidden, TRUE)
  }
  ## excludes
  for (exclude in x$excludes) {
    expect_is(exclude$variable$legend, "NullLegend")
    expect_identical(exclude$hidden, TRUE)
  }  
})
