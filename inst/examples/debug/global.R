# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# create items for the map manager
## create dataset object
d <- new_dataset(
  source = tempfile(), total = 14, units = "ha",
  legend = new_categorical_legend(
    c(1e-5, 567, 2938239, 1e7, 1e8),
    c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a6cee3")))

## create weight object
w <- new_weight("Human footprint index", d)

## simulate themes and weights
ss <- simulate_solution_settings(5, 5, 3)

## create list of all map layers
l <- append(append(ss$themes, ss$weights), list(w))

# create a map manager object
mm <- new_map_manager(l)
