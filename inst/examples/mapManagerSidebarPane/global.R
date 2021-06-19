# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(300)

# import data
d <- import_simple_raster_data()

# simulate themes and weights
sim_data <- locationmisc::simulate_data(d, 3, 4, 4)

# simulate solution
sol <- simulate_solution(sim_data$themes, sim_data$weights)

# create list of all map layers
l <- append(sol, append(sim_data$themes, sim_data$weights))

# create a map manager object
mm <- new_map_manager(l)
