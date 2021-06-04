# Initialization
## set parameters
n_single_themes <- 5
n_multi_themes <- 10
n_weights <- 5
raster_cell_size <- 30000  # 30 x 30 km  grid cells

## load packages
devtools::load_all()
library(prioritizr)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)

## set seed for reproducibility
set.seed(500)

# Preliminary processing
## unzip ecodistricts data
data_dir <- tempfile()
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
utils::unzip(
  system.file("extdata", "ECODISTRICT_V2_2_SHP.zip", package = "locationmisc"),
  exdir = data_dir)

## import ecodistricts data
cd_vector_data <- sf::read_sf(file.path(data_dir, "ECODISTRICT_V2_2_SHP.shp"))

## clean ecodistricsts data
cd_vector_data <-
  cd_vector_data %>%
  sf::st_make_valid() %>%
  mutate(id = seq_along(nrow(.))) %>%
  select(id)

## create rasterized version of data
cd_raster_data <-
  cd_vector_data %>%
  raster::raster(res = raster_cell_size) %>%
  fasterize::fasterize(sf = cd_vector_data)

# Main processing
## simulate names for features, themes, and weights
sim_names <-
  simulate_solution_settings(
    n_single_themes = n_single_themes,
    n_multi_themes = n_multi_themes,
    n_weights = n_weights)

## simulate themes
theme_raster_data <-
  lapply(sim_names$themes, function(x) {
    r <- prioritizr::simulate_species(cd_raster_data, n = length(x$feature))
    names(r) <- vapply(x$feature, function(z) z$name, character(1))
    r
  }) %>%
  setNames(vapply(sim_names$themes, function(z) z$name, character(1)))

## simulate weights
weight_raster_data <-
  prioritizr::simulate_species(cd_raster_data, n = n_weights) %>%
  setNames(vapply(sim_names$weights, function(z) z$name, character(1)))

## combine features and weights into a single raster stack
sim_raster_data <-
  theme_raster_data %>%
  do.call(what = raster::stack)

## create vector data
sim_vector_data <-
  cd_vector_data %>%
  bind_cols(exact_extract(sim_raster_data, cd_vector_data, "mean")) %>%
  select(-id)

# Exports
## raster data
sf::st_write(sim_vector_data, "sim_raster_data.tif")

## weights data
sf::st_write(sim_vector_data, "sim_vector_data.gpkg")

## configuration files
write.table(config_data, "sim_raster_data.csv", row.names = FALSE, quote = "\"")
write.table(config_data, "sim_vector_data.csv", row.names = FALSE, quote = "\"")
