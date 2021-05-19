# load package
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(500)

# create items for the map manager
## create dataset objects
d1 <- new_dataset(
  source = tempfile(), total = 12, units = "ha",
  legend = new_continuous_legend(
    32, 95,
    c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59")))
d2 <- new_dataset(
  source = tempfile(), total = 14, units = "ha",
  legend = new_categorical_legend(
    c(1e-5, 567, 2938239, 1e7, 1e8),
    c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a6cee3")))

## create weight objects
## note that we only use weights because only they are currently implemented
w1 <- new_weight("Human footprint index", d1)
w2 <- new_weight("Agricultural pressure", d2)

# create a map manager object
mm <- new_map_manager(list(w1, w2))
