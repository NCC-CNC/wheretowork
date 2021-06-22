# Initialization
## set parameters
n_single_themes <- 3
n_multi_themes <- 2
n_weights <- 3

## load packages
devtools::load_all()
library(prioritizr)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)

## set seed for reproducibility
set.seed(500)

# Import data
raster_data <- new_dataset(import_realistic_raster_data())
vector_data <- new_dataset(import_realistic_vector_data())

# Simulate data
sim_raster_data <- append(
  simulate_themes(raster_data, n_single_themes, n_multi_themes),
  simulate_weights(raster_data, n_weights))

sim_vector_data <- append(
  simulate_themes(vector_data, n_single_themes, n_multi_themes),
  simulate_weights(raster_data, n_weights))

# Exports
## raster data
writeNamedRaster(
  raster_data$data,
  "inst/extdata/sim_raster_data.tif",
  NAflag = -9999, overwrite = TRUE)

## weights data
sf::st_write(
  vector_data$data,
  "inst/extdata/sim_vector_data.gpkg",
  append = FALSE)

## configuration files
write_configuration_file(
  x = sim_raster_data,
  path = "inst/extdata/sim_raster_data.yaml",
  name = "Example GeoTIFF dataset",
  data_path = "inst/extdata/sim_raster_parameters.tif",
  mode = "advanced")

write_configuration_file(
  x = sim_vector_data,
  path = "inst/extdata/sim_vector_data.yaml",
  name = "Example GPKG dataset",
  data_path = "inst/extdata/sim_vector_parameters.gpkg",
  mode = "beginner")
