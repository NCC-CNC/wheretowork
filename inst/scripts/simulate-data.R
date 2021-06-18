# Initialization
## set parameters
n_single_themes <- 5
n_multi_themes <- 15
n_weights <- 6

## load packages
devtools::load_all()
library(prioritizr)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)

## set seed for reproducibility
set.seed(500)

# Simulate datasets
## raster data
sim_raster_data <- locationmisc::simulate_data(
  simulate_realistic_raster_data(),
  n_single_themes, n_multi_themes, n_weights)

## vector data
sim_vector_data <- locationmisc::simulate_data(
  simulate_realistic_vector_data(),
  n_single_themes, n_multi_themes, n_weights)

# Exports
## raster data
writeNamedRaster(
  sim_raster_data$weights[[1]]$variable$dataset$data,
  "inst/extdata/sim_raster_data.tif",
  NAflag = -9999, overwrite = TRUE)

## weights data
sf::st_write(
  sim_vector_data$weights[[1]]$variable$dataset$data,
  "inst/extdata/sim_vector_data.gpkg",
  append = FALSE)

## configuration files
write_configuration_file(
  x = append(sim_vector_data$themes, sim_vector_data$weights),
  path = "inst/extdata/sim_vector_data.yaml",
  name = "Example GPKG dataset",
  data_path = "inst/extdata/sim_vector_data.gpkg",
  mode = "beginner")

write_configuration_file(
  x = append(sim_raster_data$themes, sim_raster_data$weights),
  path = "inst/extdata/sim_raster_data.yaml",
  name = "Example GeoTIFF dataset",
  data_path = "inst/extdata/sim_raster_data.tif",
  mode = "advanced")
