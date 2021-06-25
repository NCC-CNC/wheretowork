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

# simulate themes, weights, and a solution
sim_themes <- simulate_themes(d, 2, 2, 2)
sim_weights <- simulate_weights(d, 2)
