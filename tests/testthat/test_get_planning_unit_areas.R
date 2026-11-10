context("get_planning_unit_areas")

# create a raster with 100 x 100 unit cells (in CRS units) and a missing cell
create_area_raster <- function(crs, xmin = 0, ymin = 0, res = 100) {
  r <- terra::rast(
    nrows = 2, ncols = 3,
    xmin = xmin, xmax = xmin + (3 * res),
    ymin = ymin, ymax = ymin + (2 * res),
    crs = crs
  )
  terra::values(r) <- c(1, 2, NA, 4, 5, 6)
  names(r) <- "V1"
  r
}

# create a grid of 100 x 100 unit squares (in CRS units)
create_area_vector <- function(crs, xmin = 0, ymin = 0, res = 100) {
  bbox <- sf::st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmin + (3 * res), ymax = ymin + (2 * res)),
    crs = sf::st_crs(crs)
  )
  sf::st_sf(
    V1 = seq_len(6),
    geometry = sf::st_make_grid(bbox, cellsize = res, n = c(3, 2))
  )
}

# US survey foot in meters
us_ft_m <- 1200 / 3937

test_that("raster (meters)", {
  d <- new_dataset_from_auto(create_area_raster("EPSG:5070"))
  x <- d$get_planning_unit_areas()
  expect_equal(x, rep(100 * 100, 5))
})

test_that("raster (US survey feet)", {
  d <- new_dataset_from_auto(create_area_raster("EPSG:2229"))
  x <- d$get_planning_unit_areas()
  expect_equal(x, rep((100 * us_ft_m)^2, 5))
})

test_that("raster (longitude/latitude)", {
  # create raster with 1 x 1 degree cells spanning different latitudes
  r <- create_area_raster("EPSG:4326", xmin = 0, ymin = 0, res = 1)
  d <- new_dataset_from_auto(r)
  x <- d$get_planning_unit_areas()
  # calculate expected areas using areas of cell polygons,
  # noting that sf::st_area() uses a spherical model (via s2) and
  # terra::cellSize() uses an ellipsoidal model, so results differ slightly
  p <- sf::st_as_sf(terra::as.polygons(r, aggregate = FALSE, na.rm = FALSE))
  y <- as.numeric(sf::st_area(p))[d$get_attribute_data()[["_index"]]]
  # run tests
  expect_length(x, 5)
  expect_equal(x, y, tolerance = 1e-2)
  ## cells closer to the equator are larger
  expect_gt(x[4], x[1])
  ## 1 x 1 degree cell near the equator is roughly 12,300 km^2
  expect_equal(x[4] * 1e-6, 12308, tolerance = 1e-2)
})

test_that("sf (meters)", {
  d <- new_dataset_from_auto(create_area_vector("EPSG:5070"))
  x <- d$get_planning_unit_areas()
  expect_equal(x, rep(100 * 100, 6))
})

test_that("sf (US survey feet)", {
  d <- new_dataset_from_auto(create_area_vector("EPSG:2229"))
  x <- d$get_planning_unit_areas()
  expect_equal(x, rep((100 * us_ft_m)^2, 6))
})

test_that("sf (longitude/latitude)", {
  # create data
  d1 <- new_dataset_from_auto(
    create_area_vector("EPSG:4326", xmin = 0, ymin = 0, res = 1)
  )
  d2 <- new_dataset_from_auto(
    create_area_raster("EPSG:4326", xmin = 0, ymin = 0, res = 1)
  )
  x <- d1$get_planning_unit_areas()
  # run tests
  expect_length(x, 6)
  ## areas are in m^2 and match the equivalent raster cells,
  ## noting that sf grid cells are ordered from the bottom row up,
  ## raster cells are ordered from the top row down,
  ## the raster is missing the third cell, and
  ## sf (spherical) and terra (ellipsoidal) areas differ slightly
  expect_equal(
    x[c(4, 5, 1, 2, 3)],
    d2$get_planning_unit_areas(),
    tolerance = 1e-2
  )
})

test_that("raster and sf areas are consistent across CRS units", {
  # create data
  r_m <- create_area_raster("EPSG:5070", res = 100 * us_ft_m)
  r_ft <- create_area_raster("EPSG:2229", res = 100)
  v_m <- create_area_vector("EPSG:5070", res = 100 * us_ft_m)
  v_ft <- create_area_vector("EPSG:2229", res = 100)
  # run tests
  expect_equal(
    new_dataset_from_auto(r_m)$get_planning_unit_areas(),
    new_dataset_from_auto(r_ft)$get_planning_unit_areas()
  )
  expect_equal(
    new_dataset_from_auto(v_m)$get_planning_unit_areas(),
    new_dataset_from_auto(v_ft)$get_planning_unit_areas()
  )
})

test_that("calculate_variable_area_km2 (US survey feet)", {
  # create data
  d <- new_dataset_from_auto(create_area_raster("EPSG:2229"))
  v <- new_variable_from_auto(dataset = d, index = 1)
  # run tests
  expect_equal(
    calculate_variable_area_km2(v),
    5 * (100 * us_ft_m)^2 * 1e-6
  )
})
