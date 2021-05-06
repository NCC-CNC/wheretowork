# load package
library(locationmisc)

# create example solution settings object
## create layers
l1 <- new_layer(source = tempfile(), total = 12, units = "ha")
l2 <- new_layer(source = tempfile(), total = 14, units = "ha")
l3 <- new_layer(source = tempfile(), total = 78, units = "ha")
l4 <- new_layer(source = tempfile(), total = 90, units = "ha")


## create a weight using a layer
w <- new_weight(name = "Human Footprint Index", layer = l1, id = "HFP")

## create features using layers
f1 <-
  new_feature(
    name = "Possum", layer = l2, initial_goal = 0.2, current = 0.1,)
f2 <-
  new_feature(
    name = "Forests", layer = l3, initial_goal = 0.5, current = 0.5)
f3 <-
  new_feature(
      name = "Shrubs", layer = l4, initial_goal = 0.6, current = 0.9)
fts <- lapply(seq_len(5), function(i) {
  l <- new_layer(
    source = tempfile(), total = runif(1, 90, 200), units = "km²")
  new_feature(
    name = paste0("Turnipus spp. ", i), layer = l,
    initial_goal = runif(1, 0.6, 0.99), current = runif(1))
})
## create themes using the features
t1 <- new_single_theme("Species", f1, id = "SPECIES")
t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "ER")
t3 <- new_multi_theme("Turnips", fts, mandatory = TRUE)

## create solution setting
ss <- new_solution_settings(themes = list(t1, t2, t3), weights = list(w))
