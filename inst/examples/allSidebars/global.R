# set options
options("rgdal_show_exportToProj4_warnings" = "none")

# load package
library(shiny)
library(locationmisc)

# set seed
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

# simulate themes, weights, includes and a solution
sim_themes <- simulate_themes(d, 2, 2, 1)
sim_weights <- simulate_weights(d, 4)
sim_includes <- simulate_includes(d, 3)
sim_parameter <- new_parameter(name = "Spatial clumping", id = "P1")
sim_solution <- simulate_solution(d, sim_themes, sim_weights, sim_includes)

# create solution settings
ss <- new_solution_settings(
  sim_themes, sim_weights, sim_includes, list(sim_parameter))

# create map manager
mm <- new_map_manager(
  append(sim_solution, append(sim_themes, append(sim_weights, sim_includes))))
