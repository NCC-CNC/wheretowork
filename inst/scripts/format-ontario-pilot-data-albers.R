# Initialization
## load packages
devtools::load_all()
library(raster)
library(dplyr)

## define variables
zip_path <- system.file(
  "extdata", "data", "ontario-pilot-data.zip", package = "wheretowork"
)
metadata_path <- system.file(
  "extdata", "data", "ontario-pilot-metadata.csv", package = "wheretowork"
)
study_area_file <- "R1km_Study_Area.tif"

# Preliminary processing
## prepare raster data
data_dir <- tempfile()
dir.create(data_dir, recursive = FALSE,  showWarnings = FALSE)
unzip(zip_path, exdir = data_dir)

## import metadata
metadata <- tibble::as_tibble(
  utils::read.table(
    metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE,
    comment.char = ""
  )
)

## validate data
assertthat::assert_that(
  all(metadata$type %in% c("theme", "include", "weight")),
  all(file.exists(file.path(data_dir, metadata$file)))
)

## import data
### planning units
study_area_data <- raster::raster(file.path(data_dir, study_area_file))

### themes, includes and weights original rasters
raster_data <- lapply(file.path(data_dir, metadata$file), function(x) {
  raster_x <- raster::raster(x)
  if (raster::compareRaster(study_area_data, raster_x, stopiffalse=FALSE)) {
    raster_x
  } else {
    print(paste0(names(raster_x), ": can not stack"))
    print(paste0("... aligning to ", names(study_area_data)))
    raster::projectRaster(raster_x, to = study_area_data, method = "ngb")
  }
}) %>% raster::stack()

## standardize data
mask_data <- round(
  sum(is.na(raster::stack(study_area_data, raster_data))) < 0.5
)
raster_data <- raster::mask(raster_data, mask_data)
study_area_data <- raster::mask(study_area_data, mask_data)

## extract data
theme_data <- raster_data[[which(metadata$type == "theme")]]
include_data <- raster_data[[which(metadata$type == "include")]]
weight_data <- raster_data[[which(metadata$type == "weight")]]

# Main processing
## prepare theme data
names(theme_data) <- gsub(".", "_", names(theme_data), fixed = TRUE)
theme_names <- metadata$name[metadata$type == "theme"]
theme_colors <- metadata$color[metadata$type == "theme"]

## prepare include data
include_data <- round(include_data > 0.5)
include_names <- metadata$name[metadata$type == "include"]
include_colors <- metadata$color[metadata$type == "include"]

## prepare weight data
weight_data <- raster::clamp(weight_data, lower = 0)

### extract crisis ecosystems
tnc_crisis_raster <- weight_data[["R1km_TNC_Crisis_Ecosystems"]]
tnc_crisis_endangered_raster <- round(tnc_crisis_raster > 1.5)
tnc_crisis_vulnerable_raster <- round(
  tnc_crisis_raster > 0.5 & tnc_crisis_raster < 1.5
)

## add layers to weight data
idx <- which(names(weight_data) != "R1km_TNC_Crisis_Ecosystems")
weight_data <- raster::stack(
  weight_data[[idx]], tnc_crisis_endangered_raster, tnc_crisis_vulnerable_raster
)
names(weight_data) <- gsub(".", "_", names(weight_data), fixed = TRUE)
weight_names <- c(
  metadata$name[metadata$type == "weight"][idx],
  "TNC Crisis Ecosystems (Endangered)",
  "TNC Crisis Ecosystems (Vulnerable)"
)
weight_colors <- c(
  metadata$color[metadata$type == "weight"][idx],
  "#fd8d3c",
  "#800026"
)

## validate processed data
assertthat::assert_that(
  ### themes
  all(raster::cellStats(theme_data, "max") == 1),
  all(raster::cellStats(theme_data, "min") == 0),
  ### includes
  all(raster::cellStats(include_data, "max") == 1),
  all(raster::cellStats(include_data, "min") == 0),
  ### weights
  all(raster::cellStats(weight_data, "min") >= 0)
)

## create objects
### create dataset
dataset <- new_dataset_from_auto(
  raster::stack(theme_data, include_data, weight_data)
)

### create themes
themes <- lapply(seq_len(raster::nlayers(theme_data)), function(i) {
  new_theme(
    name = theme_names[i],
    new_feature(
      name = names(theme_data)[i],
      goal = 0.2,
      current = 0,
      limit_goal = 0,
      visible = i == 1L,
      variable = new_variable(
        dataset = dataset,
        index = names(theme_data)[i],
        units = "",
        total = raster::cellStats(theme_data[[i]], "sum"),
        legend = new_categorical_legend(
          values = c(0, 1),
          colors = c("#00000000", theme_colors[i])
        )
      )
    )
  )
})

### create includes
#### initialize includes
includes <- lapply(seq_len(raster::nlayers(include_data)), function(i) {
  new_include(
    name = include_names[i],
    visible = FALSE,
    variable = new_variable(
      dataset = dataset,
      index = names(include_data)[i],
      units = "",
      total = raster::cellStats(include_data[[i]], "sum"),
      legend = new_manual_legend(
        labels = c("not included", "include"),
        colors = c("#00000000", include_colors[i])
      )
    )
  )
})

### manually coerce "Protected areas (buffered) weight to disabled by default
idx <- which(names(include_data) == "R1km_Protected_Areas_Plus_Buffer")
assertthat::assert_that(assertthat::is.count(idx))
includes[[idx]]$status <- FALSE

### create weights
weights <- lapply(seq_len(raster::nlayers(weight_data)), function(i) {
  #### prepare variable
  if (startsWith(weight_colors[i], "#")) {
    v <- new_variable(
      dataset = dataset,
      index = names(weight_data)[i],
      units = "",
      total = raster::cellStats(weight_data[[i]], "sum"),
      legend = new_categorical_legend(
        values = c(0, 1),
        c("#00000000", weight_colors[i])
      )
    )
  } else {
    v <- new_variable_from_auto(
     dataset = dataset,
     index = names(weight_data)[i],
     units = "",
     type = "auto",
     colors = weight_colors[i]
    )
  }
  #### create weight
  new_weight(name = weight_names[i], variable = v, visible = FALSE)
})

# Exports
## create folders if needed
dir.create(
  "inst/extdata/projects/ontario_pilot_albers", recursive = TRUE, showWarnings = FALSE
)

## save project to disk
write_project(
  x = append(themes, append(includes, weights)),
  dataset = dataset,
  name = "Ontario pilot dataset Albers",
  path =
    "inst/extdata/projects/ontario_pilot_albers/ontario_pilot_albers.yaml",
  spatial_path =
    "inst/extdata/projects/ontario_pilot_albers/ontario_pilot_albers_spatial.tif",
  attribute_path =
    "inst/extdata/projects/ontario_pilot_albers/ontario_pilot_albers_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/ontario_pilot_albers/ontario_pilot_albers_boundary.csv.gz",
  mode = "advanced",
  author_name = "Richard Schuster",
  author_email = "richard.schuster@natureconservancy.ca"
)
