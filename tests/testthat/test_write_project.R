context("write_project")

test_that("wheretowork and prioritizr package versions", {
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
  )
  # read attribute.yaml
  x <- yaml::yaml.load(enc2utf8(paste(readLines(f1), collapse = "\n")))
  
  # tests
  expect_equal(x$data_prep_date, as.character(Sys.Date()))
  expect_equal(x$wheretowork_version, as.character(packageVersion("wheretowork")))
  expect_equal(x$prioritizr_version, as.character(packageVersion("prioritizr")))
})
