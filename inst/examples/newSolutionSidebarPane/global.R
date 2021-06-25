# set options
options("rgdal_show_exportToProj4_warnings" = "none")

# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# find data file paths
f1 <- system.file(
  "extdata", "sim_raster_spatial.tif", package = "locationmisc")
f2 <- system.file(
  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
f3 <- system.file(
  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")

# create dataset
d <- new_dataset(f1, f2, f3)

# simulate themes, weights, and a solution
sim_themes <- simulate_themes(d, 3, 4, 2)
sim_weights <- simulate_weights(d, 4)
sim_includes <- simulate_includes(d, 4)
sim_parameters <- list(new_parameter(name = "Spatial clumping"))

# create new solution settings widget
ss <- new_solution_settings(
  themes = sim_themes, weights = sim_weights, includes = sim_includes,
  parameters = sim_parameters)
