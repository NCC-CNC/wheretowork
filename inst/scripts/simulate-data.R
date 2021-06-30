# Initialization
## set parameters
n_single_themes <- 3
n_multi_themes <- 2
n_weights <- 3
n_includes <- 2

## load packages
devtools::load_all()

## set seed for reproducibility
set.seed(500)

# Import data
raster_data <- new_dataset_from_auto(import_simple_raster_data())
vector_data <- new_dataset_from_auto(import_simple_vector_data())

# Simulate data
sim_raster_data <-
  append(
    simulate_themes(raster_data, n_single_themes, n_multi_themes),
    append(
      simulate_weights(raster_data, n_weights),
      simulate_includes(raster_data, n_includes)
    )
  )

sim_vector_data <-
  append(
    simulate_themes(vector_data, n_single_themes, n_multi_themes),
    append(
      simulate_weights(vector_data, n_weights),
      simulate_includes(vector_data, n_includes)
    )
  )

# Remove "layer" from attribute data
raster_data$attribute_data <- raster_data$attribute_data[, -1, drop = FALSE]

# Exports
## configuration files
write_configuration_file(
  x = sim_raster_data,
  dataset = raster_data,
  path = "inst/extdata/sim_raster_data.yaml",
  name = "Example GeoTIFF dataset",
  spatial_path = "inst/extdata/sim_raster_spatial.tif",
  attribute_path = "inst/extdata/sim_raster_attribute.csv.gz",
  boundary_path = "inst/extdata/sim_raster_boundary.csv.gz",
  mode = "advanced")

write_configuration_file(
  x = sim_vector_data,
  dataset = vector_data,
  path = "inst/extdata/sim_vector_data.yaml",
  name = "Example GPKG dataset",
  spatial_path = "inst/extdata/sim_vector_spatial.shp",
  attribute_path = "inst/extdata/sim_vector_attribute.csv.gz",
  boundary_path = "inst/extdata/sim_vector_boundary.csv.gz",
  mode = "beginner")
