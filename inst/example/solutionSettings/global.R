# load package
library(locationmisc)

# create example solution settings object
## create layurs
l1 <- new_layer(source = tempfile(), current = 0.1, total = 12, units = "ha")
l2 <- new_layer(source = tempfile(), current = 0.5, total = 14, units = "ha")
l3 <- new_layer(source = tempfile(), current = 0.9, total = 78, units = "ha")
l4 <- new_layer(source = tempfile(), current = 0.4, total = 90, units = "ha")

## create a weight using a layer
w <- new_weight(name = "Human Footprint Index", layer = l1, id = "HFP")

## create features using layers
f1 <- new_feature(name = "Possum", layer = l2, initial_goal = 0.2)
f2 <- new_feature(name = "Forests", layer = l3, initial_goal = 0.5)
f3 <- new_feature(name = "Shrubs", layer = l4, initial_goal = 0.6)

## create themes using the features
t1 <- new_single_theme("Species", f1, id = "SPECIES")
t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "ER")

## create solution setting
ss <- new_solution_settings(themes = list(t1, t2), weights = list(w))
