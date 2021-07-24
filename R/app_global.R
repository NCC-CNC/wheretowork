app_global <- quote({
  # print initial memory usage
  if (isTRUE(wheretowork::get_golem_config("monitor"))) {
      cli::cli_rule()
      golem::print_dev("Initial memory used: ")
      golem::print_dev(pryr::mem_used())
  }

  # initialize asynchronous processing
  if (identical(wheretowork::get_golem_config("strategy"), "auto")) {
    if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
      future::plan("sequential")
    } else {
      future::plan("multisession")
    }
  } else {
    future::plan(wheretowork::get_golem_config("strategy"))
  }

  # set seed for reproducibility
  set.seed(200)

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
      themes = NULL,
      weights = NULL,
      includes = NULL,
      solutions = list(),
      cache = cachem::cache_mem(),
      ## data
      bbox = NULL,
      theme_data = NULL,
      weight_data = NULL,
      include_data = NULL,
      area_data = NULL,
      boundary_data = NULL,
      new_solution_id = NULL,
      solution_ids = character(0),
      ## widgets
      mm = NULL,
      ss = NULL
    )
  )

  # find built-in projects
  project_dir <- wheretowork::get_golem_config("projects")
  if (identical(project_dir, "default")) {
    project_dir <- system.file("extdata", "projects", package = "wheretowork")
  }
  project_data <- wheretowork::find_projects(project_dir)

})
