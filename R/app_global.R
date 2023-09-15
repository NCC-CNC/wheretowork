app_global <- quote({
  # set seed for reproducibility
  set.seed(200)

  # print initial memory usage
  if (isTRUE(wheretowork::get_golem_config("monitor"))) {
      cli::cli_rule()
      golem::print_dev("Initial memory used: ")
      golem::print_dev(pryr::mem_used())
  }

  # initialize file upload limits
  options(shiny.maxRequestSize = 1000*1024^2) # 1GB

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
  #   then use built-in projects distributed with shiny app
  #
  # elif "projects: default" in golem-config.yml:
  #   then use built-in projects distributed with shiny app
  #
  # elif environmental variable "SHINYPROXY_USERGROUPS=public"
  #   then use projects only public projects available at the
  #   location "projects" location in golem config
  #
  # else:
  #   then import projects from location specified in golem-config.yml

  # set user group
  user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")
  user_groups <- tolower(gsub(" ", "", user_groups, fixed = TRUE))
  if (nchar(user_groups) == 0) {
    user_groups <- "public"
    # set user group to staff-advanced if running app locally for development
    if (identical(golem::app_dev(), TRUE)) {
      user_groups <- "staff-advanced"
    }
  }

  user_groups <- strsplit(user_groups, ",", fixed = TRUE)[[1]]

  # ensure that public projects are always available
  user_groups <- unique(c("public", user_groups))

  # set project data directory
  if (identical(Sys.getenv("FORCE_DEFAULT_PROJECTS"), "true")) {
    project_dir <- system.file("extdata", "projects", package = "wheretowork")
  } else if (identical(wheretowork::get_golem_config("projects"), "default")) {
    project_dir <- system.file("extdata", "projects", package = "wheretowork")
  } else {
    project_dir <- wheretowork::get_golem_config("projects")
  }

  # import projects
  project_data <- wheretowork::find_projects(project_dir, user_groups)

})
