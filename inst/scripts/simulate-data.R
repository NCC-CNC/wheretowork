# Initialization
## set parameters
n_single_themes <- 5
n_multi_themes <- 10
n_weights <- 5
raster_cell_size <- 50000  # 50 x 50 km  grid cells

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
  select(id) %>%
  sf::st_transform(3857)

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
sim_theme_names <- vapply(sim_names$themes, function(x) x$name, character(1))
sim_feature_names <-
  {sim_names$themes} %>%
  lapply(function(x) vapply(x$feature, function(z) z$name, character(1))) %>%
  do.call(what = c)
sim_weight_names <- vapply(sim_names$weights, function(x) x$name, character(1))

## simulate themes
theme_raster_data <-
  lapply(sim_names$themes, function(x) {
    cd_raster_data %>%
    raster::setExtent(c(0, 10, 0, 10)) %>%
    prioritizr::simulate_species(n = length(x$feature)) %>%
    raster::setExtent(cd_raster_data)
  })

## simulate weights
weight_raster_data <-
  cd_raster_data %>%
  raster::setExtent(c(0, 10, 0, 10)) %>%
  prioritizr::simulate_species(n = n_weights) %>%
  raster::setExtent(cd_raster_data)

## combine features and weights into a single raster stack
sim_raster_data <-
  theme_raster_data %>%
  do.call(what = raster::stack) %>%
  raster::stack(weight_raster_data) %>%
  setNames(
    c(sim_feature_names, sim_weight_names) %>%
    make.names() %>%
    gsub(pattern = ".", replacement = "_", fixed = TRUE) %>%
    gsub(pattern = "(_)\\1+", replacement = "\\1"))

## force first theme to contain categorical values
sim_raster_data[[1]][] <-
  sample(seq(1, 5), ncell(sim_raster_data[[1]]), replace = TRUE)
sim_raster_data[[1]] <- raster::mask(sim_raster_data[[1]], sim_raster_data[[2]])

## create vector data
sim_vector_data <-
  cd_vector_data %>%
  bind_cols(
    sim_raster_data %>%
    exact_extract(cd_vector_data, "mean") %>%
    setNames(names(sim_raster_data))) %>%
  select(-id)

# Exports
## raster data
writeNamedRaster(
  sim_raster_data,
  "inst/extdata/sim_raster_data.tif",
  NAflag = -9999, overwrite = TRUE)

## weights data
sf::st_write(
  sim_vector_data,
  "inst/extdata/sim_vector_data.gpkg",
  append = FALSE)
