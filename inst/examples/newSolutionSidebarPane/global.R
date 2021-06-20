# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# simulate data
d <- import_simple_raster_data()
sim_data <- simulate_data(d, 5, 5, 8)

# create new solution settings widget
ss <- new_solution_settings(
  themes = sim_data$themes, weights = sim_data$weights)
