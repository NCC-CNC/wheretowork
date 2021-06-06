# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# create items for the map manager
# create dataset object
d <- new_dataset(tempfile())

# create variables
v1 <- new_variable(
  dataset = d, index = 1, total = 14, units = "ha",
  legend = new_categorical_legend(
    c(1e-5, 567, 2938239, 1e7, 1e8),
    c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a6cee3")))

v2 <- new_variable(
  dataset = d, index = 2, total = 14, units = "ha",
  legend = new_continuous_legend(
    0, 100, c("#fee8c8", "#fdbb84", "#e34a33")))

## create weight objects
w1 <- new_weight("Human footprint index", v1)
w2 <- new_weight("Fire risk", v2)

## simulate themes and weights
sim_data <- locationmisc::simulate_data(1, 1, 1)

## add a categorical legend into the multi-themes
sim_data$themes[[2]]$feature[[1]]$variable <- v1

## create list of all map layers
l <- append(append(sim_data$themes, sim_data$weights), list(w1, w2))

# create a map manager object
mm <- new_map_manager(l)
