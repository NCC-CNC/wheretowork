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

# import data
# d <- new_dataset(import_simple_vector_data())
d <- new_dataset(locationmisc:::import_simple_raster_data())

# simulate themes, weights, and a solution
sim_themes <- simulate_themes(d, 1, 1, 1)
sim_weights <- simulate_weights(d, 10)
sim_solution <- simulate_solution(d, sim_themes, sim_weights)

# create list of all map layers
l <- append(sim_solution, append(sim_themes, sim_weights))

# create a map manager object
mm <- new_map_manager(l)
