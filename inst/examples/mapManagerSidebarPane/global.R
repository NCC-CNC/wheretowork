# set options
options("rgdal_show_exportToProj4_warnings" = "none")

# load package
library(shiny)
library(leaflet)
library(leafgl)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(200)
RandomFields::RFoptions(seed = 200)

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
sim_themes <- simulate_themes(d, 1, 1, 1)
sim_weights <- simulate_weights(d, 10)
sim_solution <- simulate_solution(d, sim_themes, sim_weights)

# create list of all map layers
l <- append(sim_solution, append(sim_themes, sim_weights))

# create a map manager object
mm <- new_map_manager(l)
