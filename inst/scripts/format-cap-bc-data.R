# Initialization
## load packages
devtools::load_all()
library(raster)
library(dplyr)

# Get input data file paths ----

## define variables
metadata_path <- file.path(
  "inst", "extdata", "data", "cap-bc-metadata.csv"
)
study_area_file <- "Planning Units.tif"

# Preliminary processing
## prepare raster data
data_dir <- file.path("inst", "extdata", "data", "cap-bc-data")

# Read-in data ----

## Import formatted csv (metadata) as tibble
metadata <- tibble::as_tibble(
  utils::read.table(
    metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE,
    comment.char = ""
  )
)

## Validate metadata
assertthat::assert_that(
  all(metadata$Type %in% c("theme", "include", "weight")),
  all(file.exists(file.path(data_dir, metadata$File)))
)

## Import study area (planning units) raster
study_area_data <- raster::raster(file.path(data_dir, study_area_file))

## Import themes, includes and weights rasters as a raster stack. If raster
## variable does not stack to study area, re-project raster variable so it aligns
## to the study area
raster_data <- lapply(file.path(data_dir, metadata$File), function(x) {
  raster_x <- raster::raster(x)
  if (raster::compareRaster(study_area_data, raster_x, stopiffalse=FALSE)) {
    raster_x
  } else {
    print(paste0(names(raster_x), ": can not stack"))
    print(paste0("... aligning to ", names(study_area_data)))
    raster::projectRaster(raster_x, to = study_area_data, method = "ngb")
  }
}) %>% raster::stack()

# Pre-Processing ----

## Create a mask layer. The mask layer maps NA cells in the raster stack. Any
## NA found in a variable will cause the raster stack to all have an NA
mask_data <- round(
  sum(is.na(raster::stack(study_area_data, raster_data))) < 0.5
)

## "Clip" all the raster data to the mask layer
raster_data <- raster::mask(raster_data, mask_data)
study_area_data <- raster::mask(study_area_data, mask_data)

# Subset data using metadata tibble ----

## Prepare theme inputs
theme_data <- raster_data[[which(metadata$Type == "theme")]]
names(theme_data) <- gsub(".", "_", names(theme_data), fixed = TRUE)
theme_names <- metadata$Name[metadata$Type == "theme"]
theme_groups <- metadata$Theme[metadata$Type == "theme"]
theme_colors <- metadata$Color[metadata$Type == "theme"]
theme_labels <- metadata$Labels[metadata$Type == "theme"]
theme_units <- metadata$Unit[metadata$Type == "theme"]
theme_visible <- metadata$Visible[metadata$Type == "theme"]
theme_provenance <- metadata$Provenance[metadata$Type == "theme"]

## Prepare include inputs
include_data <- raster_data[[which(metadata$Type == "include")]]
include_data <- round(include_data > 0.5)
include_names <- metadata$Name[metadata$Type == "include"]
include_colors <- metadata$Color[metadata$Type == "include"]
include_labels <- metadata$Labels[metadata$Type == "include"]
include_units <- metadata$Unit[metadata$Type == "include"]
include_visible <- metadata$Visible[metadata$Type == "include"]
include_provenance <- metadata$Provenance[metadata$Type == "include"]

## Prepare weight inputs
weight_data <- raster_data[[which(metadata$Type == "weight")]]
weight_data <- raster::clamp(weight_data, lower = 0)
weight_names <- metadata$Name[metadata$Type == "weight"]
weight_colors <- metadata$Color[metadata$Type == "weight"]
weight_labels <- metadata$Labels[metadata$Type == "weight"]
weight_units <- metadata$Unit[metadata$Type == "weight"]
weight_visible <- metadata$Visible[metadata$Type == "weight"]
weight_provenance <- metadata$Provenance[metadata$Type == "weight"]
weight_legend <- metadata$Legend[metadata$Type == "weight"]

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

# Instantiate Where To Work objects ----

## Create data set
dataset <- new_dataset_from_auto(
  raster::stack(theme_data, include_data, weight_data)
)

print("creating themes")

## Create themes ----
## 1. Loop of unique theme groups.
themes <- lapply(seq_along(unique(theme_groups)), function(i) {

  ## 2. store temp variables associated with group (i)
  curr_theme_groups <- unique(theme_groups)[i]
  curr_theme_data <- theme_data[[which(theme_groups == curr_theme_groups)]]
  curr_theme_data_names <- names(curr_theme_data)
  curr_theme_names <- theme_names[theme_groups == curr_theme_groups]
  curr_theme_colors <- theme_colors[theme_groups == curr_theme_groups]
  curr_theme_labels <- theme_labels[theme_groups == curr_theme_groups]
  curr_theme_units <- theme_units[theme_groups == curr_theme_groups]
  curr_theme_visible <- theme_visible[theme_groups == curr_theme_groups]
  curr_theme_provenance <- theme_provenance[theme_groups == curr_theme_groups]

  ## 3. Create list of features (j) associated with group
  curr_features <- lapply(seq_along(curr_theme_names), function(j) {
    new_feature(
      name = curr_theme_names[j],
      goal = 0.2,
      current = 0,
      limit_goal = 0,
      visible = curr_theme_visible[j],
      variable = new_variable(
        dataset = dataset,
        index = curr_theme_data_names[j],
        units = curr_theme_units[j],
        total = raster::cellStats(curr_theme_data[[j]], "sum"),
        legend = new_manual_legend(
          values = c(0, 1),
          colors = c("#00000000", curr_theme_colors[j]),
          labels = unlist(strsplit(curr_theme_labels[j], ","))
        ),
        provenance = new_provenance_from_source(curr_theme_provenance[j])
      )
    )
  })

  # Create theme from list of features
  curr_theme <- new_theme(curr_theme_groups,curr_features)

  # return theme
  curr_theme
})


print("Creating includes")

## Create includes ----

## Loop over each raster in include_data
includes <- lapply(seq_len(raster::nlayers(include_data)), function(i) {
  new_include(
    name = include_names[i],
    visible = include_visible[i],
    variable = new_variable(
      dataset = dataset,
      index = names(include_data)[i],
      units = include_units[i],
      total = raster::cellStats(include_data[[i]], "sum"),
      legend = new_manual_legend(
        values = c(0, 1),
        colors = c("#00000000", include_colors[i]),
        labels = unlist(strsplit(include_labels[i], ","))
      ),
      provenance = new_provenance_from_source(include_provenance[i])
    )
  )
})

print("Creating weights")

## Create weights ----
## Loop over each raster in weight_data
weights <- lapply(seq_len(raster::nlayers(weight_data)), function(i) {
  ## prepare variable (categorical legend)
  if (identical(weight_legend[i], "manual")) {
    v <- new_variable_from_auto(
      dataset = dataset,
      index = names(weight_data)[i],
      units = weight_units[i],
      type = "manual",
      colors = trimws(unlist(strsplit(weight_colors[i], ","))),
      provenance = weight_provenance[i],
      labels = unlist(strsplit(weight_labels[i], ","))
    )
  } else { ## prepare variable (continuous legend, automatically identified)
    v <- new_variable_from_auto(
     dataset = dataset,
     index = names(weight_data)[i],
     units = weight_units[i],
     type = "auto",
     colors = weight_colors[i],
     provenance = weight_provenance[i],
     labels = "missing"
    )
  }
  ## Create weight
  new_weight(name = weight_names[i], variable = v, visible = weight_visible[i])
})

# Export Where To Work objects ----

## Create output folder if needed
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
  author_name = "Xavier C. Llano",
  author_email = "llano@unbc.ca"
)
