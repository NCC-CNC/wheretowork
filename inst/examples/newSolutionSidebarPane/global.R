# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# import data
d <- new_dataset(import_simple_raster_data())

# simulate themes, weights, and a solution
sim_themes <- simulate_themes(d, 3, 4)
sim_weights <- simulate_weights(d, 4)

# create new solution settings widget
ss <- new_solution_settings(
  themes = sim_themes, weights = sim_weights)
