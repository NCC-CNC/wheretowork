context("read_project")

test_that("SpatRaster (standard boundary format, continuous)", {
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
  sim_excludes[[1]]$downloadable <- FALSE
  sim_excludes[[2]]$downloadable <- FALSE
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
    x$excludes[[i]]$downloadable <- FALSE 
  }  
  # run tests
  expect_equal(x$themes, sim_themes)
  expect_equal(x$weights, sim_weights)
  expect_equal(x$includes, sim_includes)
  expect_equal(x$excludes, sim_excludes)
  expect_equal(x$name, "test")
  expect_equal(x$mode, "beginner")
  ## attributes: excludes
  for (i in seq_along(x$excludes)) {
    expect_equal(x$excludes[[i]]$downloadable, FALSE)
    expect_equal(x$excludes[[i]]$hidden, FALSE)
    expect_equal(x$excludes[[i]]$loaded, TRUE)
    expect_equal(x$excludes[[i]]$visible, TRUE)
    expect_equal(x$excludes[[i]]$status, FALSE)
  }
  ## attributes: includes
  for (i in seq_along(x$includes)) {
    expect_equal(x$includes[[i]]$downloadable, TRUE)
    expect_equal(x$includes[[i]]$hidden, FALSE)
    expect_equal(x$includes[[i]]$loaded, TRUE)
    expect_equal(x$includes[[i]]$visible, TRUE)
    expect_equal(x$includes[[i]]$status, TRUE)
  }
  ## attributes: weights
  for (i in seq_along(x$weights)) {
    expect_equal(x$weights[[i]]$downloadable, TRUE)
    expect_equal(x$weights[[i]]$hidden, FALSE)
    expect_equal(x$weights[[i]]$loaded, TRUE)
    expect_equal(x$weights[[i]]$visible, TRUE)
    expect_equal(x$weights[[i]]$status, TRUE)
  }
  ## attributes: themes
  for (i in seq_along(x$themes)) {
    for (j in seq_along(x$themes[[i]]$feature)) {
      expect_equal(x$themes[[i]]$feature[[j]]$downloadable, TRUE)
      expect_equal(x$themes[[i]]$feature[[j]]$hidden, FALSE)
      expect_equal(x$themes[[i]]$feature[[j]]$loaded, TRUE)
      expect_equal(x$themes[[i]]$feature[[j]]$visible, TRUE)
      expect_equal(x$themes[[i]]$feature[[j]]$status, TRUE)
    }
  }  
})

test_that("SpatRaster (standard boundary format, categorical, manual legend)", {
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

test_that("SpatRaster (standard boundary format, continous & categorical)", {
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

test_that("SpatRaster (Marxan boundary format)", {
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
    author_name = "Greg_McGregerson",
    author_email = "greg_mcgregerson@supergreg_greg"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
  # tests
  expect_equal(x$author_name, "Greg_McGregerson")
  expect_equal(x$author_email, "greg_mcgregerson@supergreg_greg")
})

test_that("provenance", {
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
    author_name = "Greg_McGregerson",
    author_email = "greg_mcgregerson@supergreg_greg"
  )
  # import data
  x <- read_project(f1, f2, f3, f4)
  # get simulated themes provenance list
  sim_themes_prov <- lapply(sim_themes, function(x) {
    lapply(x$feature, function(y) {
      y$variable$provenance
    })
  })
  # get read project themes provenance list
  read_themes_prov <- lapply(x$themes, function(x) {
    lapply(x$feature, function(y) {
      y$variable$provenance
    })
  })
  # get simulated weights provenance list
  sim_weights_prov <- lapply(sim_weights, function(x) {
    x$variable$provenance
  })
  # get read project weights provenance list
  read_weights_prov <- lapply(x$weights, function(x) {
    x$variable$provenance
  })
  # get simulated includes provenance list
  sim_includes_prov <- lapply(sim_includes, function(x) {
    x$variable$provenance
  })
  # get read project includes provenance list
  read_includes_prov <- lapply(x$includes, function(x) {
    x$variable$provenance
  })  
  # get simulated excludes provenance list
  sim_excludes_prov <- lapply(sim_excludes, function(x) {
    x$variable$provenance
  })
  # get read project excludes provenance list
  read_excludes_prov <- lapply(x$excludes, function(x) {
    x$variable$provenance
  })    
  # tests
  expect_equal(sim_themes_prov, read_themes_prov)
  expect_equal(sim_weights_prov, read_weights_prov)
  expect_equal(sim_includes_prov, read_includes_prov)
  expect_equal(sim_excludes_prov, read_excludes_prov)
})
