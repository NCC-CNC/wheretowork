app_global <- quote({
  # set seed for reproducibility
  set.seed(200)

  # print initial memory usage
  if (isTRUE(wheretowork::get_golem_config("monitor"))) {
      cli::cli_rule()
      golem::print_dev("Initial memory used: ")
      golem::print_dev(pryr::mem_used())
  }

  # initialize asynchronous processing
  ## identify strategy
  strategy <- wheretowork::get_golem_config("strategy")
  if (identical(strategy, "auto")) {
    if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
      strategy <- "multicore"
    } else if (identical(.Platform$OS.type, "unix")) {
      strategy <- "multicore"
    } else {
      strategy <- "multisession"
    }
  }
  ## set future settings
  options(
    future.wait.timeout = wheretowork::get_golem_config("worker_time_out")
  )
  ## implement strategy
  golem::print_dev(paste("plan strategy:", strategy))
  assertthat::assert_that(
    strategy %in% c("sequential", "cluster", "multicore", "multisession"),
    msg = "not a valid strategy"
  )
  suppressWarnings(future::plan(strategy, workers = 2))

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
      solution_ids = character(0),
      ## widgets
      mm = NULL,
      ss = NULL,
      ## state variables
      new_solution_id = NULL,
      task = NULL
    )
  )

  # find built-in projects
  project_dir <- wheretowork::get_golem_config("projects")
  if (identical(project_dir, "default")) {
    project_dir <- system.file("extdata", "projects", package = "wheretowork")
  }
  project_data <- wheretowork::find_projects(project_dir)

})
