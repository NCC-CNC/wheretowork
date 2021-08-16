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

  # find built-in projects
  # if environmental variable "FORCE_DEFAULT_PROJECTS=true":
  #   then use built-in projects
  # if "projects: default" in golem-config.yml:
  #   then use built-in projects
  # else:
  #   then import projects from location specified in golem-config.yml
  if (!identical(Sys.getenv("FORCE_DEFAULT_PROJECTS"), "true")) {
    project_dir <- wheretowork::get_golem_config("projects")
    if (identical(project_dir, "default")) {
      project_dir <- system.file("extdata", "projects", package = "wheretowork")
    }
  } else {
    project_dir <- system.file("extdata", "projects", package = "wheretowork")
  }

  # import projects
  project_data <- wheretowork::find_projects(project_dir)

})
