context("new_dataset")

test_that("raster (from memory)", {
  # prepare data
  spatial_data <- import_simple_raster_data()
  idx <- raster::Which(!is.na(spatial_data), cells = TRUE)
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    `_index` = idx)
  boundary_data <- prioritizr::boundary_matrix(spatial_data)
  # create object
  x <- new_dataset(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$spatial_path, "memory")
  expect_identical(x$attribute_path, "memory")
  expect_identical(x$boundary_path, "memory")
  expect_identical(x$spatial_data, spatial_data)
  expect_identical(x$attribute_data, attribute_data)
  expect_identical(x$boundary_data, boundary_data)
  expect_identical(x$get_spatial_data(), spatial_data)
  expect_identical(x$get_attribute_data(), attribute_data)
  expect_identical(x$get_boundary_data(), boundary_data)
  expect_identical(
    x$get_index(1),
    {
      y <- spatial_data
      y[attribute_data[["_index"]]] <- attribute_data$V1
      names(y) <- "V1"
      y
    }
  )
  expect_identical(
    x$get_index(c(2, 1)),
    {
      y <- spatial_data
      y <- raster::stack(spatial_data, spatial_data)
      y[[1]][attribute_data[["_index"]]] <- attribute_data$V2
      y[[2]][attribute_data[["_index"]]] <- attribute_data$V1
      names(y) <- c("V2", "V1")
      y
    }
  )
  expect_identical(
    x$get_index("V1"),
    {
      y <- spatial_data
      y[attribute_data[["_index"]]] <- attribute_data$V1
      names(y) <- "V1"
      y
    }
  )
  expect_identical(
    x$get_index(c("V2", "V1")),
    {
      y <- raster::stack(spatial_data, spatial_data)
      y[[1]][attribute_data[["_index"]]] <- attribute_data$V2
      y[[2]][attribute_data[["_index"]]] <- attribute_data$V1
      names(y) <- c("V2", "V1")
      y
    }
  )
  expect_true(x$has_index(2))
  expect_equal(x$has_index(c(1, 2)), c(TRUE, TRUE))
  expect_true(x$has_index("V2"))
  expect_equal(x$has_index(c("V1", "V2")), c(TRUE, TRUE))
  expect_false(x$has_index("ASDFG"))
  expect_false(x$has_index(3))
  expect_equal(x$has_index(c("V1", "ASDFG")), c(TRUE, FALSE))
  expect_identical(
    x$get_bbox(),
    setNames(
      as.list(as.list(raster::extent(spatial_data))),
      c("xmin", "xmax", "ymin", "ymax"))
  )
})

test_that("methods (sf from memory)", {
  # prepare data
  spatial_data <- import_simple_vector_data()
  idx <- seq_len(nrow(spatial_data))
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    `_index` = idx)
  boundary_data <- prioritizr::boundary_matrix(spatial_data)
  # create object
  x <- new_dataset(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$spatial_path, "memory")
  expect_identical(x$attribute_path, "memory")
  expect_identical(x$boundary_path, "memory")
  expect_identical(x$spatial_data, spatial_data)
  expect_identical(x$attribute_data, attribute_data)
  expect_identical(x$boundary_data, boundary_data)
  expect_identical(x$get_spatial_data(), spatial_data)
  expect_identical(x$get_attribute_data(), attribute_data)
  expect_identical(x$get_boundary_data(), boundary_data)
  expect_identical(
    x$get_index(1),
    {
      y <- spatial_data
      y$V1 <- attribute_data$V1
      y <- y[, "V1"]
      attr(y, "agr") <- NULL
      y
    }
  )
  expect_identical(
    x$get_index("V1"),
    {
      y <- spatial_data
      y$V1 <- attribute_data$V1
      y <- y[, "V1"]
      attr(y, "agr") <- NULL
      y
    }
  )
  expect_true(x$has_index(2))
  expect_true(x$has_index("V2"))
  expect_false(x$has_index("ASDFG"))
  expect_false(x$has_index(3))
  expect_identical(
    x$get_bbox(),
    setNames(
      as.list(as.list(raster::extent(spatial_data))),
      c("xmin", "xmax", "ymin", "ymax"))
  )
})

test_that("raster (from file)", {
  # prepare data
  spatial_data <- import_simple_raster_data()
  idx <- raster::Which(!is.na(spatial_data), cells = TRUE)
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    `_index` = idx)
  boundary_data <- prioritizr::boundary_matrix(spatial_data)[idx, idx]
  # create object from memory
  d <- new_dataset(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data)
  # write object to disk
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".csv.gz")
  f3 <- tempfile(fileext = ".csv.gz")
  d$write(f1, f2, f3)
  # create object from file paths
  x <- new_dataset(f1, f2, f3)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$spatial_path, f1)
  expect_identical(x$attribute_path, f2)
  expect_identical(x$boundary_path, f3)
  x$import()
  expect_equal(raster::values(x$spatial_data), raster::values(spatial_data))
  expect_equal(x$attribute_data, attribute_data)
  expect_identical(x$boundary_data, boundary_data)
  expect_true(x$has_index(2))
  expect_equal(x$has_index(c(1, 2)), c(TRUE, TRUE))
  expect_true(x$has_index("V2"))
  expect_equal(x$has_index(c("V1", "V2")), c(TRUE, TRUE))
  expect_false(x$has_index("ASDFG"))
  expect_false(x$has_index(3))
  expect_equal(x$has_index(c("V1", "ASDFG")), c(TRUE, FALSE))
  expect_identical(
    x$get_bbox(),
    setNames(
      as.list(as.list(raster::extent(spatial_data))),
      c("xmin", "xmax", "ymin", "ymax"))
  )
})

test_that("methods (sf from file)", {
  # prepare data
  spatial_data <- import_simple_vector_data()
  idx <- seq_len(nrow(spatial_data))
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    `_index` = idx)
  boundary_data <- prioritizr::boundary_matrix(spatial_data)
  # create object from memory
  d <- new_dataset(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data)
  # write object to disk
  f1 <- tempfile(fileext = ".shp")
  f2 <- tempfile(fileext = ".csv.gz")
  f3 <- tempfile(fileext = ".csv.gz")
  d$write(f1, f2, f3)
  # create object from file paths
  x <- new_dataset(f1, f2, f3)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$spatial_path, f1)
  expect_identical(x$attribute_path, f2)
  expect_identical(x$boundary_path, f3)
  x$import()
  expect_equal(
    sf::st_set_crs(x$spatial_data, 3857),
    sf::st_set_crs(spatial_data, 3857)
  )
  expect_equal(x$attribute_data, attribute_data)
  expect_identical(x$boundary_data, boundary_data)
  expect_equal(
    sf::st_set_crs(x$get_spatial_data(), 3857),
    sf::st_set_crs(spatial_data, 3857)
  )
  expect_equal(x$get_attribute_data(), attribute_data)
  expect_identical(x$get_boundary_data(), boundary_data)
  expect_true(x$has_index(2))
  expect_true(x$has_index("V2"))
  expect_false(x$has_index("ASDFG"))
  expect_false(x$has_index(3))
  expect_identical(
    x$get_bbox(),
    setNames(
      as.list(as.list(raster::extent(spatial_data))),
      c("xmin", "xmax", "ymin", "ymax"))
  )
})
