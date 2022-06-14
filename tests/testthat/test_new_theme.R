context("new_theme")

test_that("initialization (single feature)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.2567,
    id = "FID1"
  )
  x <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "FS1")
  expect_identical(x$name, "FS")
  expect_identical(x$feature, list(f))
})

test_that("export method (single feature)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.2567,
    id = "FID1"
  )
  x <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  # run tests
  expect_equal(
    x$export(),
    list(
      name = x$name,
      feature = lapply(x$feature, function(x) x$export())
    )
  )
})

test_that("get methods (single feature)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.2567,
    id = "FID1"
  )
  x <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  # run tests
  expect_identical(x$get_feature_goal(), 0.2)
  expect_identical(x$get_feature_visible(), TRUE)
  expect_identical(x$get_feature_hidden(), FALSE)
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_feature_status(), FALSE)
  expect_identical(x$get_feature_current(), 0.2567)
  expect_identical(x$get_setting("feature_goal"), x$get_feature_goal())
  expect_identical(x$get_setting("feature_status"), x$get_feature_status())
  expect_identical(x$get_setting("feature_visible"), x$get_feature_visible())
  expect_identical(x$get_setting("feature_current"), x$get_feature_current())
  expect_identical(x$get_setting("visible"), x$get_visible())
})

test_that("set methods (single feature)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = TRUE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.2567,
    id = "FID1"
  )
  x <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  # run tests
  x$set_feature_goal(0.8)
  x$set_feature_visible(FALSE)
  x$set_feature_status(TRUE)
  x$set_feature_current(0.213)
  expect_identical(x$get_feature_goal(), 0.8)
  expect_identical(x$get_feature_visible(), FALSE)
  expect_identical(x$get_feature_status(), TRUE)
  expect_identical(x$get_feature_current(), 0.213)
  x$set_setting("feature_goal", 0.5)
  x$set_setting("feature_status", FALSE)
  x$set_setting("feature_visible", TRUE)
  x$set_setting("feature_current", 0.12)
  expect_identical(x$get_feature_goal(), 0.5)
  expect_identical(x$get_feature_status(), FALSE)
  expect_identical(x$get_feature_visible(), TRUE)
  expect_identical(x$get_feature_current(), 0.12)
  x$set_visible(FALSE)
  expect_identical(x$get_visible(), FALSE)
})

test_that("widget methods (single feature)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = FALSE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  x <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FS1",
      name = "FS",
      feature_name = "F1",
      feature_id = "FID1",
      feature_status = FALSE,
      feature_total_amount = v$total,
      feature_current_held = 0.034,
      feature_min_goal = 0,
      feature_max_goal = 1,
      feature_goal = 0.2,
      feature_limit_goal = 0.05,
      feature_step_goal = 0.01,
      feature_provenance = list(v$provenance$get_widget_data()),
      units = "ha"
    )
  )
  ## map manager
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "FS1",
      name = "FS",
      feature_name = "F1",
      feature_id = "FID1",
      feature_visible = FALSE,
      feature_hidden = FALSE,
      feature_legend = list(v$legend$get_widget_data()),
      feature_provenance = list(v$provenance$get_widget_data()),
      units = "ha",
      type = "theme"
    )
  )
})

test_that("initialization (multiple features)", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  f1 <- new_feature(
    name = "F1",
    variable = new_variable_from_auto(dataset = d, index = 1, units = "ha"),
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.02,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = new_variable_from_auto(dataset = d, index = 2, units = "ha"),
    visible = FALSE,
    hidden = TRUE,
    status = TRUE,
    goal = 0.21,
    limit_goal = 0.021,
    current = 0.5,
    id = "FID2"
  )
  x <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "MF1")
  expect_identical(x$name, "MF")
  expect_identical(x$feature, list(f1, f2))
})

test_that("get methods (multiple features)", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  f1 <- new_feature(
    name = "F1",
    variable = new_variable_from_auto(dataset = d, index = 1, units = "ha"),
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.02,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = new_variable_from_auto(dataset = d, index = 2, units = "ha"),
    visible = FALSE,
    hidden = TRUE,
    status = TRUE,
    goal = 0.21,
    limit_goal = 0.021,
    current = 0.5,
    id = "FID2"
  )
  x <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  # run tests
  expect_identical(x$get_feature_goal(), c(0.2, 0.21))
  expect_identical(x$get_feature_visible(), c(TRUE, FALSE))
  expect_identical(x$get_feature_hidden(), c(FALSE, TRUE))
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_feature_status(), c(FALSE, TRUE))
  expect_identical(x$get_feature_current(), c(0.245, 0.5))
  expect_identical(x$get_feature_goal(), x$get_setting("feature_goal"))
  expect_identical(x$get_feature_status(), x$get_setting("feature_status"))
  expect_identical(x$get_feature_visible(), x$get_setting("feature_visible"))
  expect_identical(x$get_feature_current(), x$get_setting("feature_current"))
  expect_identical(x$get_setting("visible"), x$get_visible())
})

test_that("set methods (multiple features)", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  f1 <- new_feature(
    name = "F1",
    variable = new_variable_from_auto(dataset = d, index = 1, units = "ha"),
    visible = TRUE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.02,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = new_variable_from_auto(dataset = d, index = 2, units = "ha"),
    visible = FALSE,
    status = TRUE,
    goal = 0.21,
    limit_goal = 0.021,
    current = 0.5,
    id = "FID2"
  )
  x <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  # run tests
  x$set_feature_goal(c(0.89, 0.26))
  x$set_feature_status(c(TRUE, FALSE))
  x$set_feature_visible(c(FALSE, FALSE))
  x$set_feature_current(c(0.43, 0.21))
  expect_identical(x$get_feature_goal(), c(0.89, 0.26))
  expect_identical(x$get_feature_status(), c(TRUE, FALSE))
  expect_identical(x$get_feature_visible(), c(FALSE, FALSE))
  expect_identical(x$get_feature_current(), c(0.43, 0.21))
  x$set_setting("feature_goal", c(0.33, 0.67))
  x$set_setting("feature_status", c(FALSE, FALSE))
  x$set_setting("feature_visible", c(TRUE, TRUE))
  x$set_setting("feature_current", c(0.22, 0.99))
  expect_identical(x$get_feature_goal(), c(0.33, 0.67))
  expect_identical(x$get_feature_status(), c(FALSE, FALSE))
  expect_identical(x$get_feature_visible(), c(TRUE, TRUE))
  expect_identical(x$get_feature_current(), c(0.22, 0.99))
  x$set_visible(FALSE)
  expect_identical(x$get_visible(), FALSE)
})

test_that("export method (multiple features)", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  f1 <- new_feature(
    name = "F1",
    variable = new_variable_from_auto(dataset = d, index = 1, units = "ha"),
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.02,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = new_variable_from_auto(dataset = d, index = 2, units = "ha"),
    visible = FALSE,
    hidden = TRUE,
    status = TRUE,
    goal = 0.21,
    limit_goal = 0.021,
    current = 0.5,
    id = "FID2"
  )
  x <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  # run tests
  expect_equal(
    x$export(),
    list(
      name = x$name,
      feature = lapply(x$feature, function(x) x$export())
    )
  )
})

test_that("widget methods (multiple features)", {
  skip_if_not_installed("RandomFields")
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  f1 <- new_feature(
    name = "F1",
    variable = new_variable_from_auto(dataset = d, index = 1, units = "ha"),
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.02,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = new_variable_from_auto(dataset = d, index = 2, units = "ha"),
    visible = FALSE,
    hidden = TRUE,
    status = TRUE,
    goal = 0.21,
    limit_goal = 0.021,
    current = 0.523,
    id = "FID2"
  )
  x <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "MF1",
      name = "MF",
      feature_name = c("F1", "F2"),
      feature_id = c("FID1", "FID2"),
      feature_status = c(FALSE, TRUE),
      feature_total_amount = c(f1$variable$total, f2$variable$total),
      feature_current_held = c(0.245, 0.523),
      feature_min_goal = c(0, 0),
      feature_max_goal = c(1, 1),
      feature_goal = c(0.2, 0.21),
      feature_limit_goal = c(0.02, 0.021),
      feature_step_goal = c(0.01, 0.01),
      feature_provenance = lapply(
        x$feature, function(x) x$variable$provenance$get_widget_data()
      ),
      units = "ha"
    )
  )
  ## map manager settings
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "MF1",
      name = "MF",
      feature_name = c("F1", "F2"),
      feature_id = c("FID1", "FID2"),
      feature_visible = c(TRUE, FALSE),
      feature_hidden = c(FALSE, TRUE),
      feature_legend = lapply(
        x$feature, function(x) x$variable$legend$get_widget_data()
      ),
      feature_provenance = lapply(
        x$feature, function(x) x$variable$provenance$get_widget_data()
      ),
      units = "ha",
      type = "theme"
    )
  )
})

test_that("render on map (project on the fly)", {
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
  f <- new_feature(
    name = "Forest",
    variable = v,
    visible = TRUE,
    status = FALSE,
    goal = 0.2,
    limit_goal = 0.05,
    current = 0.2567,
    id = "FID1"
  )
  t <- new_theme(
    name = "Habitat",
    feature = f,
    id = "TID1"
  )
  # render map
  l <- leaflet::leaflet() %>% leaflet::addTiles()
  m <- t$render_on_map(x = l, zindex = 1000)
  # run tests
  expect_is(m, "leaflet")
})
