context("calculate_boundary_matrix")

# create a raster with 3 x 2 cells
create_boundary_raster <- function(crs, xmin = 0, ymin = 0, res = 100) {
  terra::rast(
    nrows = 2, ncols = 3,
    xmin = xmin, xmax = xmin + (3 * res),
    ymin = ymin, ymax = ymin + (2 * res),
    crs = crs, vals = seq_len(6)
  )
}

# US survey foot in meters
us_ft_m <- 1200 / 3937

test_that("crs_units_to_m", {
  expect_equal(crs_units_to_m(create_boundary_raster("EPSG:5070")), 1)
  expect_equal(crs_units_to_m(create_boundary_raster("EPSG:2229")), us_ft_m)
  expect_error(
    crs_units_to_m(create_boundary_raster("EPSG:4326", res = 1))
  )
  expect_equal(
    crs_units_to_m(
      sf::st_as_sf(terra::as.polygons(create_boundary_raster("EPSG:2229")))
    ),
    us_ft_m
  )
})

test_that("raster (meters)", {
  r <- create_boundary_raster("EPSG:5070")
  x <- calculate_boundary_matrix(r)
  expect_equal(
    as.matrix(x),
    as.matrix(prioritizr::boundary_matrix(r)),
    check.attributes = FALSE
  )
  expect_equal(unname(Matrix::diag(x)), rep(400, 6))
  expect_equal(x[1, 2], 100)
})

test_that("sf (meters)", {
  p <- sf::st_as_sf(terra::as.polygons(create_boundary_raster("EPSG:5070")))
  x <- calculate_boundary_matrix(p)
  expect_equal(unname(Matrix::diag(x)), rep(400, 6))
  expect_equal(x[1, 2], 100)
})

test_that("raster (US survey feet)", {
  # lengths are converted from feet to meters
  x <- calculate_boundary_matrix(create_boundary_raster("EPSG:2229"))
  y <- calculate_boundary_matrix(create_boundary_raster("EPSG:5070"))
  expect_equal(as.matrix(x), as.matrix(y) * us_ft_m, check.attributes = FALSE)
  expect_equal(unname(Matrix::diag(x)), rep(400 * us_ft_m, 6))
})

test_that("sf (US survey feet)", {
  # lengths are in meters
  r <- create_boundary_raster("EPSG:2229")
  x <- calculate_boundary_matrix(sf::st_as_sf(terra::as.polygons(r)))
  y <- calculate_boundary_matrix(
    sf::st_as_sf(terra::as.polygons(create_boundary_raster("EPSG:5070")))
  )
  expect_equal(as.matrix(x), as.matrix(y) * us_ft_m, check.attributes = FALSE)
  expect_equal(unname(Matrix::diag(x)), rep(400 * us_ft_m, 6))
  ## lengths are consistent with the same data as a raster
  expect_equal(
    as.matrix(x), as.matrix(calculate_boundary_matrix(r)),
    check.attributes = FALSE
  )
})

test_that("write and read dataset (US survey feet)", {
  # create data
  d1 <- new_dataset_from_auto(create_boundary_raster("EPSG:2229"))
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".csv.gz")
  f3 <- tempfile(fileext = ".csv.gz")
  # write and read data
  d1$write(f1, f2, f3)
  d2 <- new_dataset(f1, f2, f3)
  # run tests
  ## boundary lengths are in meters
  expect_equal(
    unname(Matrix::diag(d2$get_boundary_data())), rep(400 * us_ft_m, 6)
  )
  expect_equal(
    as.matrix(d1$get_boundary_data()),
    as.matrix(d2$get_boundary_data()),
    check.attributes = FALSE
  )
  ## areas are in m^2
  expect_equal(d2$get_planning_unit_areas(), rep((100 * us_ft_m)^2, 6))
  # clean up
  unlink(c(f1, f2, f3))
})

test_that("solution perimeter is reported in km (US survey feet)", {
  # create data
  r <- create_boundary_raster("EPSG:2229")
  r <- c(r, r * 2)
  names(r) <- c("a", "b")
  d <- new_dataset_from_auto(r)
  v1 <- new_variable_from_auto(dataset = d, index = "a")
  v2 <- new_variable_from_auto(dataset = d, index = "b")
  f1 <- new_feature(name = "F1", variable = v1, goal = 1, id = "F1")
  f2 <- new_feature(name = "F2", variable = v2, goal = 1, id = "F2")
  th <- new_theme("T1", list(f1, f2), id = "T1")
  p1 <- new_parameter("Spatial clustering", id = "P1")
  p2 <- new_parameter("Gap", id = "P2")
  ss <- new_solution_settings(
    themes = list(th), weights = list(), includes = list(),
    excludes = list(), parameters = list(p1, p2)
  )
  # generate result that selects all planning units
  r <- min_set_result(
    id = "R1",
    area_data = d$get_planning_unit_areas(),
    boundary_data = d$get_boundary_data(),
    theme_data = ss$get_theme_data(),
    weight_data = ss$get_weight_data(),
    include_data = ss$get_include_data(),
    exclude_data = ss$get_exclude_data(),
    theme_settings = ss$get_theme_settings(),
    weight_settings = ss$get_weight_settings(),
    include_settings = ss$get_include_settings(),
    exclude_settings = ss$get_exclude_settings(),
    parameters = ss$parameters,
    gap_1 = 0,
    boundary_gap = 0
  )
  x <- new_solution_from_result(
    id = "S1",
    result = r,
    name = "sol",
    visible = TRUE,
    downloadable = TRUE,
    dataset = d,
    settings = ss,
    legend = new_manual_legend(
      values = c(0, 1),
      colors = c("#00FFFF00", "#112233FF"),
      labels = c("not selected", "selected")
    )
  )
  stats <- stats::setNames(
    vapply(x$statistics, function(s) s$value, numeric(1)),
    vapply(x$statistics, function(s) s$name, character(1))
  )
  # run tests
  expect_equal(r$values, rep(1, 6))
  ## result perimeter is in meters (outer boundary of 300 x 200 ft extent,
  ## noting that prioritizr::eval_boundary_summary() applies an edge factor
  ## of 0.5 to exposed boundaries by default)
  expect_equal(r$perimeter, 1000 * us_ft_m * 0.5)
  ## reported perimeter is in km
  expect_equal(stats[["Total perimeter"]], r$perimeter * 1e-3)
  ## reported area is in km^2
  expect_equal(stats[["Total area"]], 6 * (100 * us_ft_m)^2 * 1e-6)
})

test_that("raster (longitude/latitude)", {
  # create data
  r <- create_boundary_raster("EPSG:4326", xmin = 0, ymin = 50, res = 1)
  x <- calculate_boundary_matrix(r)
  # calculate expected result using geodesic lengths of cell polygons
  y <- calculate_boundary_matrix(
    sf::st_as_sf(terra::as.polygons(r, aggregate = FALSE))
  )
  # run tests
  expect_equal(as.matrix(x), as.matrix(y), check.attributes = FALSE)
  ## cells in the same row share an edge along a meridian (1 degree latitude)
  expect_equal(
    x[1, 2],
    terra::distance(
      cbind(0, 51), cbind(0, 52), lonlat = TRUE, pairwise = TRUE
    )
  )
  ## cells in the same column share an edge along a parallel (at 51 degrees)
  expect_equal(
    x[1, 4],
    terra::distance(
      cbind(0, 51), cbind(1, 51), lonlat = TRUE, pairwise = TRUE
    )
  )
  ## cells closer to the pole have shorter perimeters
  expect_lt(x[1, 1], x[4, 4])
})

test_that("raster (longitude/latitude, missing values)", {
  # create data
  r <- create_boundary_raster("EPSG:4326", xmin = 0, ymin = 50, res = 1)
  r[3] <- NA
  x <- calculate_boundary_matrix(r)
  # calculate expected result using geodesic lengths of cell polygons
  y <- calculate_boundary_matrix(
    sf::st_as_sf(terra::as.polygons(r, aggregate = FALSE, na.rm = TRUE))
  )
  idx <- terra::cells(r)
  # run tests
  expect_equal(
    as.matrix(x[idx, idx]), as.matrix(y), check.attributes = FALSE
  )
  expect_equal(sum(x[3, ]), 0)
})

test_that("sf (longitude/latitude)", {
  # create data
  p <- sf::st_as_sf(
    terra::as.polygons(
      create_boundary_raster("EPSG:4326", xmin = 0, ymin = 50, res = 1),
      aggregate = FALSE
    )
  )
  x <- calculate_boundary_matrix(p)
  # run tests
  ## lengths are geodesic meters (not degrees or web mercator units)
  expect_equal(
    x[1, 2],
    terra::distance(
      cbind(0, 51), cbind(0, 52), lonlat = TRUE, pairwise = TRUE
    )
  )
  ## lengths are consistent with the same data in an equal area projection
  y <- calculate_boundary_matrix(sf::st_transform(p, "ESRI:102001"))
  expect_equal(as.matrix(x), as.matrix(y), tolerance = 1e-2,
    check.attributes = FALSE
  )
})

test_that("new_dataset_from_auto (longitude/latitude)", {
  # create data
  r <- create_boundary_raster("EPSG:4326", xmin = 0, ymin = 50, res = 1)
  p <- sf::st_as_sf(terra::as.polygons(r, aggregate = FALSE))
  names(p)[1] <- "V1"
  d1 <- new_dataset_from_auto(r)
  d2 <- new_dataset_from_auto(p)
  # run tests
  expect_equal(
    as.matrix(d1$get_boundary_data()),
    as.matrix(calculate_boundary_matrix(r)),
    check.attributes = FALSE
  )
  expect_equal(
    as.matrix(d2$get_boundary_data()),
    as.matrix(calculate_boundary_matrix(p)),
    check.attributes = FALSE
  )
  expect_equal(
    as.matrix(d1$get_boundary_data()),
    as.matrix(d2$get_boundary_data()),
    check.attributes = FALSE
  )
})

test_that("write and read dataset (longitude/latitude)", {
  # create data
  r <- create_boundary_raster("EPSG:4326", xmin = 0, ymin = 50, res = 1)
  r[3] <- NA
  d1 <- new_dataset_from_auto(r)
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".csv.gz")
  f3 <- tempfile(fileext = ".csv.gz")
  # write and read data
  d1$write(f1, f2, f3)
  d2 <- new_dataset(f1, f2, f3)
  # run tests
  expect_equal(d1$get_planning_unit_areas(), d2$get_planning_unit_areas())
  expect_equal(
    as.matrix(d1$get_boundary_data()),
    as.matrix(d2$get_boundary_data()),
    check.attributes = FALSE
  )
  # clean up
  unlink(c(f1, f2, f3))
})
