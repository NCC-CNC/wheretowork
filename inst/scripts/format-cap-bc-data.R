# Initialization
## load packages
devtools::load_all()
library(raster)
library(dplyr)

## define variables
metadata_path <- file.path(
  "inst", "extdata", "data", "cap-bc-metadata.csv"
)
study_area_file <- "Planning Units.tif"

# Preliminary processing
## prepare raster data
data_dir <- file.path("inst", "extdata", "data", "cap-bc-data")

## import metadata
metadata <- tibble::as_tibble(
  utils::read.table(
    metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE,
    comment.char = ""
  )
)

print(file.path(data_dir, metadata$file))
## validate data
assertthat::assert_that(
  all(metadata$type %in% c("theme", "include", "weight")),
  all(file.exists(file.path(data_dir, metadata$file)))
)

## import data
study_area_data <- raster::raster(file.path(data_dir, study_area_file))
study_area_data <- raster::projectRaster(
  study_area_data, crs = as(sf::st_crs(3857), "CRS")
)
raster_data <- lapply(file.path(data_dir, metadata$file), function(x) {
  raster::projectRaster(raster::raster(x), to = study_area_data, method = "ngb")
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
theme_groups <- metadata$group[metadata$type == "theme"]
theme_groups <- theme_groups[!duplicated(theme_groups)]
theme_names <- metadata$name[metadata$type == "theme"]
theme_colors <- metadata$color[metadata$type == "theme"]

## prepare include data
include_data <- round(include_data > 0.5)
include_names <- metadata$name[metadata$type == "include"]
include_colors <- metadata$color[metadata$type == "include"]

## prepare weight data
weight_data <- raster::clamp(weight_data, lower = 0)

names(weight_data) <- gsub(".", "_", names(weight_data), fixed = TRUE)
weight_names <- metadata$name[metadata$type == "weight"]
weight_colors <- metadata$color[metadata$type == "weight"]

## validate processed data  TODO
# assertthat::assert_that(
#   ### themes
#   all(raster::cellStats(theme_data, "max") == 1),
#   all(raster::cellStats(theme_data, "min") == 0),
#   ### includes
#   all(raster::cellStats(include_data, "max") == 1),
#   all(raster::cellStats(include_data, "min") == 0),
#   ### weights
#   all(raster::cellStats(weight_data, "min") >= 0)
# )

## create objects
### create dataset
dataset <- new_dataset_from_auto(
  raster::stack(theme_data, include_data, weight_data)
)

### create themes
themes <- lapply(seq_len(length(theme_groups)), function(t) {
  theme_data <- raster_data[[which(metadata$group == theme_groups[t])]]
  theme_names <- metadata$name[metadata$group == theme_groups[t]]
  theme_colors <- metadata$color[metadata$group == theme_groups[t]]


  features_in_themes <- lapply(seq_len(raster::nlayers(theme_data)), function(i) {
    # Variable for the feature
    if (startsWith(theme_colors[i], "#")) {
      v <- new_variable(
        dataset = dataset,
        index = names(theme_data)[i],
        units = "",
        total = raster::cellStats(theme_data[[i]], "sum"),
        legend = new_categorical_legend(
          values = c(0, 1),
          c("#00000000", theme_colors[i])
        )
      )
    } else {
      v <- new_variable_from_auto(
       dataset = dataset,
       index = names(theme_data)[i],
       units = "",
       type = "auto",
       colors = theme_colors[i]
      )
    }
    new_feature(
      name = theme_names[i],
      visible = i == 1L,
      variable = v,
    )
  })

  new_theme(
    name = theme_groups[t],
    feature = features_in_themes,
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
  "inst/extdata/projects/cap_bc", recursive = TRUE, showWarnings = FALSE
)

## save project to disk
write_project(
  x = append(themes, append(includes, weights)),
  dataset = dataset,
  name = "CAP-BC",
  path =
    "inst/extdata/projects/cap_bc/cap_bc.yaml",
  spatial_path =
    "inst/extdata/projects/cap_bc/cap_bc_spatial.tif",
  attribute_path =
    "inst/extdata/projects/cap_bc/cap_bc_attribute.csv.gz",
  boundary_path =
    "inst/extdata/projects/cap_bc/cap_bc_boundary.csv.gz",
  mode = "advanced",
  author_name = "Xavier Corredor Llano",
  author_email = "llano@unbc.ca"
)
