# set options
options("rgdal_show_exportToProj4_warnings" = "none")

# load package
library(shiny)
library(DT)
library(locationmisc)

# set seed
set.seed(600)

# find data file paths
f1 <- system.file(
  "extdata", "sim_raster_spatial.tif", package = "locationmisc")
f2 <- system.file(
  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
f3 <- system.file(
  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")

# create dataset
d <- new_dataset(f1, f2, f3)

# simulate themes, weights, and a solution
sim_themes <- simulate_themes(d, 3, 2)
sim_weights <- simulate_weights(d, 5)

# randomly set weight values
for (i in seq_along(sim_weights)) {
  sim_weights[[i]]$status <- runif(1) > 0.5
  sim_weights[[i]]$factor <- runif(1) * 100
}

# set theme values
sim_themes[[1]]$feature[[1]]$status <- FALSE
for (i in seq_along(sim_themes[[4]]$feature)) {
  sim_themes[[4]]$feature[[i]]$status <- FALSE
}
sim_themes[[5]]$feature[[1]]$status <- FALSE

# simulate solutions
sols <- lapply(seq_len(5), function(x) {
  simulate_solution(d, sim_themes, sim_weights)
})

# extract solution names
sol_names <-
  setNames(
    vapply(sols, `[[`, character(1), "id"),
    vapply(sols, `[[`, character(1), "name")
  )
