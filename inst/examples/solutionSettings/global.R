# set options
options("rgdal_show_exportToProj4_warnings" = "none")

# load package
library(shiny)
library(locationmisc)

# create example solution settings object
## find data file paths
f1 <- system.file(
  "extdata", "sim_raster_spatial.tif", package = "locationmisc")
f2 <- system.file(
  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
f3 <- system.file(
  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")

## create dataset
d <- new_dataset(f1, f2, f3)

## create variables
v1 <- new_variable(
  dataset = d, index = 1, total = 12, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#1b9e77")))
v2 <- new_variable(
  dataset = d, index = 2, total = 14, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#d95f02")))
v3 <- new_variable(
  dataset = d, index = 3, total = 78, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#7570b3")))
v4 <- new_variable(
  dataset = d, index = 4, total = 90, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#e31a1c")))
v5 <- new_variable(
  dataset = d, index = 5, total = 12, units = "",
  legend = simulate_include_legend())
v6 <- new_variable(
  dataset = d, index = 6, total = 23, units = "",
  legend = simulate_include_legend())

## create a weight using dataset
w <- new_weight(name = "Human Footprint Index", variable = v1, id = "HFP")

## create features using dataset
f1 <-
  new_feature(
    name = "Possum", variable = v2, initial_goal = 0.2, current = 0.1,)
f2 <-
  new_feature(
    name = "Forests", variable = v3, initial_goal = 0.5, current = 0.5)
f3 <-
  new_feature(
      name = "Shrubs", variable = v4, initial_goal = 0.6, current = 0.9)
fts <- lapply(seq_len(5), function(i) {
  v <- new_variable(
    dataset = d, index = i, total = runif(1, 90, 200), units = "km²",
    legend = new_continuous_legend(1, 100, c("#000000", "#e31a1c")))
  new_feature(
    name = paste0("Turnipus spp. ", i), variable = v,
    initial_goal = runif(1, 0.6, 0.99), current = runif(1))
})

## create themes using the features
t1 <- new_single_theme("Species", f1, id = "SPECIES")
t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "ER")
t3 <- new_multi_theme("Turnips", fts)

## create a include using dataset
i1 <- new_include(
  name = "National protected area", variable = v5, id = "I1", mandatory = TRUE)
i2 <- new_include(
  name = "Nature reserves", variable = v6, id = "I2", mandatory = FALSE)

## create a parameter
p <- new_parameter(name = "Spatial clumping", id = "P1")

## create solution setting
ss <- new_solution_settings(
  themes = list(t1, t2, t3), weights = list(w), includes = list(i1, i2),
  parameters = list(p))
