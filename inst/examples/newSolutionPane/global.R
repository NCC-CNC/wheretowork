# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# create solution settings object
ss <- simulate_solution_settings(3, 3, 4)
