# load package
library(shiny)
library(locationmisc)

# create example solution settings object
## create datasets
l1 <- new_dataset(
  source = tempfile(), total = 12, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#1b9e77")))
l2 <- new_dataset(
  source = tempfile(), total = 14, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#d95f02")))
l3 <- new_dataset(
  source = tempfile(), total = 78, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#7570b3")))
l4 <- new_dataset(
  source = tempfile(), total = 90, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#e31a1c")))

## create a weight using a dataset
w <- new_weight(name = "Human Footprint Index", dataset = l1, id = "HFP")

## create features using datasets
f1 <-
  new_feature(
    name = "Possum", dataset = l2, initial_goal = 0.2, current = 0.1,)
f2 <-
  new_feature(
    name = "Forests", dataset = l3, initial_goal = 0.5, current = 0.5)
f3 <-
  new_feature(
      name = "Shrubs", dataset = l4, initial_goal = 0.6, current = 0.9)
fts <- lapply(seq_len(5), function(i) {
  l <- new_dataset(
    source = tempfile(), total = runif(1, 90, 200), units = "km²",
    legend = new_continuous_legend(1, 100, c("#000000", "#e31a1c")))
  new_feature(
    name = paste0("Turnipus spp. ", i), dataset = l,
    initial_goal = runif(1, 0.6, 0.99), current = runif(1))
})
## create themes using the features
t1 <- new_single_theme("Species", f1, id = "SPECIES")
t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "ER")
t3 <- new_multi_theme("Turnips", fts, mandatory = TRUE)

## create solution setting
ss <- new_solution_settings(themes = list(t1, t2, t3), weights = list(w))
