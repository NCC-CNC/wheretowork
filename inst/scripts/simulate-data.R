# Initialization
## set parameters
n_single_themes <- 3
n_multi_themes <- 2
n_weights <- 3
n_includes <- 2

## load packages
devtools::load_all()
library(sf)
library(dplyr)

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

# Create projects with only themes
sim_raster_data2 <- sim_raster_data[seq_len(n_single_themes + n_multi_themes)]
sim_vector_data2 <- sim_vector_data[seq_len(n_single_themes + n_multi_themes)]

# Create project with some hidden layers
sim_raster_data3 <- lapply(sim_raster_data, function(x) x$clone(deep = TRUE))
## single theme
idx <- 1
sim_raster_data3[[idx]]$feature[[1]]$hidden <- TRUE
sim_raster_data3[[idx]]$feature[[1]]$visible <- FALSE
## multi theme (only first feature not visible)
idx <- n_single_themes + 1
sim_raster_data3[[idx]]$feature[[1]]$hidden <- TRUE
sim_raster_data3[[idx]]$feature[[1]]$visible <- FALSE
## multi theme (all features not visible)
idx <- n_single_themes + 2
sim_raster_data3[[idx]]$feature <- lapply(
  sim_raster_data3[[idx]]$feature, function(x) {
    x$hidden <- TRUE
    x$visible <- FALSE
    x
  }
)
## weight
idx <- n_single_themes + n_multi_themes + 1
sim_raster_data3[[idx]]$hidden <- TRUE
sim_raster_data3[[idx]]$visible <- FALSE
## include
idx <- n_single_themes + n_multi_themes + n_weights + 1
sim_raster_data3[[idx]]$hidden <- TRUE
sim_raster_data3[[idx]]$visible <- FALSE

# Remove "layer" from attribute data
raster_data$attribute_data <- raster_data$attribute_data[, -1, drop = FALSE]

# Exports
## create folders if needed
dir.create(
  "inst/extdata/projects/sim_raster", recursive = TRUE, showWarnings = FALSE
)
dir.create(
  "inst/extdata/projects/sim_raster2", recursive = TRUE, showWarnings = FALSE
)
dir.create(
  "inst/extdata/projects/sim_raster3", recursive = TRUE, showWarnings = FALSE
)
dir.create(
  "inst/extdata/projects/sim_vector", recursive = TRUE, showWarnings = FALSE
)
dir.create(
  "inst/extdata/projects/sim_vector2", recursive = TRUE, showWarnings = FALSE
)
dir.create(
  "inst/extdata/shapefile", recursive = TRUE, showWarnings = FALSE
)

## raster project (with themes + includes + weights)
write_project(
  x = sim_raster_data,
  dataset = raster_data,
  name = "Example GeoTIFF dataset",
  path =
    "inst/extdata/projects/sim_raster/sim_raster_data.yaml",
  spatial_path =
    "inst/extdata/projects/sim_raster/sim_raster_spatial.tif",
  attribute_path =
    "inst/extdata/projects/sim_raster/sim_raster_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/sim_raster/sim_raster_boundary.csv.gz",
  mode = "advanced",
  user_groups = "public",
  author_name = "Richard Schuster",
  author_email = "richard.schuster@natureconservancy.ca"
)

## raster project (with only themes)
write_project(
  x = sim_raster_data2,
  dataset = raster_data,
  name = "Example GeoTIFF dataset (themes)",
  path =
    "inst/extdata/projects/sim_raster2/sim_raster2_data.yaml",
  spatial_path =
    "inst/extdata/projects/sim_raster2/sim_raster2_spatial.tif",
  attribute_path =
    "inst/extdata/projects/sim_raster2/sim_raster2_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/sim_raster2/sim_raster2_boundary.dat.gz",
  mode = "advanced",
  user_groups = "admin",
  author_name = "Richard Schuster",
  author_email = "richard.schuster@natureconservancy.ca"
)

## raster project (with themes + includes + weights, and some hidden)
write_project(
  x = sim_raster_data3,
  dataset = raster_data,
  name = "Example GeoTIFF dataset (hidden)",
  path =
    "inst/extdata/projects/sim_raster3/sim_raster3_data.yaml",
  spatial_path =
    "inst/extdata/projects/sim_raster3/sim_raster3_spatial.tif",
  attribute_path =
    "inst/extdata/projects/sim_raster3/sim_raster3_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/sim_raster3/sim_raster3_boundary.csv.gz",
  mode = "advanced",
  user_groups = "admin",
  author_name = "Richard Schuster",
  author_email = "richard.schuster@natureconservancy.ca"
)

## vector project (with themes + includes + weights)
write_project(
  x = sim_vector_data,
  dataset = vector_data,
  name = "Example Shapefile dataset",
  path =
    "inst/extdata/projects/sim_vector/sim_vector_data.yaml",
  spatial_path =
    "inst/extdata/projects/sim_vector/sim_vector_spatial.shp",
  attribute_path =
    "inst/extdata/projects/sim_vector/sim_vector_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/sim_vector/sim_vector_boundary.csv.gz",
  mode = "beginner",
  user_groups = "public",
  author_name = "Richard Schuster",
  author_email = "richard.schuster@natureconservancy.ca"
)

## vector project (with only themes)
write_project(
  x = sim_vector_data2,
  dataset = vector_data,
  name = "Example Shapefile dataset (themes)",
  path =
    "inst/extdata/projects/sim_vector2/sim_vector2_data.yaml",
  spatial_path =
    "inst/extdata/projects/sim_vector2/sim_vector2_spatial.shp",
  attribute_path =
    "inst/extdata/projects/sim_vector2/sim_vector2_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/sim_vector2/sim_vector2_boundary.dat.gz",
  mode = "beginner",
  user_groups = "admin",
  author_name = "Richard Schuster",
  author_email = "richard.schuster@natureconservancy.ca"
)

## shapefile
{vector_data$spatial_data} %>%
select(-`_index`) %>%
{bind_cols(., select(vector_data$attribute_data, -`_index`))} %>%
as_Spatial() %>%
raster::shapefile("inst/extdata/shapefile/sim_shapefile.shp", overwrite = TRUE)
