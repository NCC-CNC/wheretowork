# load package
library(shiny)
library(locationmisc)

# set seed
set.seed(500)

# import data
d <- new_dataset(import_simple_raster_data())

# simulate themes, weights, and a solution
sim_themes <- simulate_themes(d, 3, 4)
sim_weights <- simulate_weights(d, 4)
