# load package
library(shiny)
library(locationmisc)

# set seed
set.seed(500)

# simulate data
sim_data <- simulate_data(3, 2, 3)
