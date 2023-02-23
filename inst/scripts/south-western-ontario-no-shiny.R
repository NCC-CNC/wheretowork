# Initialization ----

## load packages
devtools::load_all()
library(raster)
library(dplyr)

# Get input data file paths ----
configuration_path <- system.file(
  "extdata", "projects", "south_western_ontario", "south_western_ontario.yaml", package = "wheretowork"
)

attribute_path <- system.file(
  "extdata", "projects", "south_western_ontario", "south_western_ontario_attribute.csv.gz", package = "wheretowork"
)

boundary_path<- system.file(
  "extdata", "projects", "south_western_ontario", "south_western_ontario_boundary.csv.gz", package = "wheretowork"
)

spatial_path <- system.file(
  "extdata", "projects", "south_western_ontario", "south_western_ontario_spatial.tif", package = "wheretowork"
)

# Read-project ----
x <- wheretowork::read_project(
  path = configuration_path, 
  spatial_path = spatial_path,
  attribute_path = attribute_path, 
  boundary_path = boundary_path
)

# Define parameters ----
area_budget_parameter <-
  wheretowork::new_parameter(
    name = "Total area budget",
    status = FALSE,
    value = 1,
    id = "budget_parameter"
  )

boundary_gap_parameter <-
  wheretowork::new_parameter(
    name = "Spatial clustering",
    status = FALSE,
    value = 1,
    id = "spatial_parameter"
  )

solution_layer_parameter <-
  wheretowork::new_parameter(
    name = "Hide solution layer from map",
    id = "solution_layer_parameter"
  )

overlap_parameter <-
  wheretowork::new_parameter(
    name = "Override includes",
    status = FALSE,
    id = "overlap_parameter"
  )

fileinput_parameter <-
  wheretowork::new_parameter(
    name = "Input configurations",
    id = "fileinput_parameter"
  )  

## Generate solution settings ----
ss <- wheretowork::new_solution_settings(
  themes = x$themes,
  weights = x$weights,
  includes = x$includes,
  excludes = x$excludes,
  parameters = list(area_budget_parameter, boundary_gap_parameter, 
    solution_layer_parameter, overlap_parameter, fileinput_parameter)
)

## Update solution settings ----
settings_path <- system.file(
  "extdata", "data", "south-western-ontario-ss-update_configs.yaml", package = "wheretowork"
)
settings_lst <- try(
  yaml::yaml.load(enc2utf8(paste(readLines(settings_path), collapse = "\n"))),
  silent = TRUE
)
updated_ss <- try(ss$update_ss(settings_lst), silent = FALSE)

# Generate result object ----
cache <- cachem::cache_mem()
if (ss$get_parameter("budget_parameter")$status) {
  #### if budget specified, then use the min shortfall formulation
  r <- try(
    wheretowork::min_shortfall_result(
      id = uuid::UUIDgenerate(),
      area_budget_proportion = c(
        ss$get_parameter("budget_parameter")$value * 
        ss$get_parameter("budget_parameter")$status
      ) / 100,
      area_data = x$dataset$get_planning_unit_areas(),
      boundary_data = x$dataset$get_boundary_data(),
      theme_data = ss$get_theme_data(),
      weight_data = ss$get_weight_data(),
      include_data = ss$get_include_data(),
      exclude_data = ss$get_exclude_data(),
      theme_settings = ss$get_theme_settings(),
      weight_settings = ss$get_weight_settings(),
      include_settings = ss$get_include_settings(),
      exclude_settings = ss$get_exclude_settings(),
      parameters = lapply(ss$parameters, function(x) x$clone()),
      overlap = ss$get_parameter("overlap_parameter")$status,
      gap_1 = get_golem_config("solver_gap_1"),
      gap_2 = get_golem_config("solver_gap_2"),
      boundary_gap = c(
        ss$get_parameter("spatial_parameter")$value *
        ss$get_parameter("spatial_parameter")$status
      ) / 100,
      cache = cache,
      time_limit_1 = get_golem_config("solver_time_limit_1"),
      time_limit_2 = get_golem_config("solver_time_limit_2"),
      verbose = get_golem_config("verbose")
    ),
    silent = TRUE
  )
} else {
  #### else, then use the min set formulation
  r <- try(
    wheretowork::min_set_result(
      id = uuid::UUIDgenerate(),
      area_data = x$dataset$get_planning_unit_areas(),
      boundary_data = x$dataset$get_boundary_data(),
      theme_data = ss$get_theme_data(),
      weight_data = ss$get_weight_data(),
      include_data = ss$get_include_data(),
      exclude_data = ss$get_exclude_data(),
      theme_settings = ss$get_theme_settings(),
      weight_settings = ss$get_weight_settings(),
      include_settings = ss$get_include_settings(),
      exclude_settings = ss$get_exclude_settings(),
      parameters = lapply(ss$parameters, function(x) x$clone()),
      overlap = ss$get_parameter("overlap_parameter")$status,
      gap_1 = get_golem_config("solver_gap_1"),
      gap_2 = get_golem_config("solver_gap_2"),
      boundary_gap = c(
        ss$get_parameter("spatial_parameter")$value *
          ss$get_parameter("spatial_parameter")$status
      ) / 100,
      cache = cache,
      time_limit_1 = get_golem_config("solver_time_limit_1"),
      time_limit_2 = get_golem_config("solver_time_limit_2"),
      verbose = get_golem_config("verbose")
    ),
    silent = TRUE
  )
}

# Generate solution object ----
s <- wheretowork::new_solution_from_result(
  id = uuid::UUIDgenerate(),
  result = r,
  name = "new solution",
  visible = TRUE,
  hidden = FALSE,
  dataset = x$dataset,
  settings = ss,
  legend = new_manual_legend(
    values = c(0, 1),
    colors = c("#00FFFF00", "#2ca25f"),
    labels = c("not selected", "selected")
  )
)

# Explore outputs ----
## Render solution on leaflet map ----
s$render_on_map(leaflet::leaflet(), 400) %>%
  leaflet::addTiles()

## Render tables ----
s$render_summary_results()
s$render_theme_results()
s$render_weight_results()
s$render_include_results()


