app_global <- quote({
  # initialize asynchronous processing
  if (identical(get_golem_config("strategy"), "auto")) {
    if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
      future::plan("sequential")
    } else {
      future::plan("multisession")
    }
  } else {
    future::plan(get_golem_config("strategy"))
  }

  # set seed for reproducibility
  set.seed(200)

  # define parameters for solution settings
  area_budget_parameter <-
    new_parameter(
      name = "Total area budget",
      status = FALSE,
      value = 0,
      min_value = 0,
      max_value = 100,
      step_value = 1,
      units = "%",
      hide = TRUE,
      id = "budget_parameter"
    )

  boundary_gap_parameter <-
    new_parameter(
      name = "Spatial clustering",
      status = FALSE,
      value = 0,
      min_value = 0,
      max_value = 100,
      step_value = 1,
      units = "%",
      hide = FALSE,
      id = "spatial_parameter"
    )

  # define global variables
  ## note that we use an environment here because they are mutable objects and
  ## so we don't have to worry about using the super-assignment operator
  app_data <- list2env(
    list(
      ## file paths
      configuration_path = NULL,
      spatial_path = NULL,
      boundary_path = NULL,
      attribute_path = NULL,
      ## settings
      mode = NULL,
      ## objects
      dataset = NULL,
      parameters = list(boundary_gap_parameter),
      themes = NULL,
      weights = NULL,
      includes = NULL,
      solutions = list(),
      ## data
      bbox = NULL,
      theme_data = NULL,
      weight_data = NULL,
      include_data = NULL,
      boundary_data = NULL,
      solution_ids = character(0),
      ## widgets
      mm = NULL,
      ss = NULL
    )
  )

  # find built-in projects
  project_dir <- get_golem_config("projects")
  if (identical(project_dir, "default")) {
    project_dir <- system.file("extdata", "projects", package = "wheretowork")
  }
  project_data <- find_projects(project_dir)
})
