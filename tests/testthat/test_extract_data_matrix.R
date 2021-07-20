context("extract_data_matrix")

test_that("single variable", {
  # prepare data
  spatial_data <- import_simple_raster_data()
  idx <- raster::Which(!is.na(spatial_data), cells = TRUE)
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    `_index` = idx
  )
  boundary_data <- prioritizr::boundary_matrix(spatial_data)
  # create dataset
  d <- new_dataset(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data
  )
  # create variable
  v <- new_variable_from_auto(dataset = d, index = "V1")
  # create object
  m <- extract_data_matrix(list(v))
  # run tests
  expect_is(m, "dgCMatrix")
  expect_equal(ncol(m), length(idx))
  expect_equal(nrow(m), 1)
  expect_equal(c(as.matrix(m)), attribute_data$V1)
  expect_equal(rownames(m), "V1")
})

test_that("multiple variables", {
  # prepare data
  spatial_data <- import_simple_raster_data()
  idx <- raster::Which(!is.na(spatial_data), cells = TRUE)
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    `_index` = idx
  )
  boundary_data <- prioritizr::boundary_matrix(spatial_data)
  # create dataset
  d <- new_dataset(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data
  )
  # create variables
  v1 <- new_variable_from_auto(dataset = d, index = "V1")
  v2 <- new_variable_from_auto(dataset = d, index = "V2")
  # create object
  m <- extract_data_matrix(list(v2, v1))
  # run tests
  expect_is(m, "dgCMatrix")
  expect_equal(ncol(m), length(idx))
  expect_equal(nrow(m), 2)
  expect_equal(c(t(as.matrix(m))), c(attribute_data$V2, attribute_data$V1))
  expect_equal(rownames(m), c("V2", "V1"))
})
