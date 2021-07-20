#' @include internal.R
NULL

#' Generate minimum shortfall solution
#'
#' Create a new [Solution] object by generating a prioritization
#' using the minimum shortfall formulation of the reserve selection problem.
#'
#' @param name `character` name for new solution.
#'
#' @param dataset [Dataset] object to store the solution.
#'
#' @param settings [SolutionSettings] object with settings.
#'
#' @param area_budget_proportion `numeric` budget for the solution.
#'   This should be a proportion (with values ranging between 0 and 1)
#'   indicating the maximum spatial extent of the solution.
#'
#' @param theme_data [Matrix::sparseMatrix()] containing the theme data.
#'  Defaults to being automatically calculated from `settings`.
#'
#' @param weight_data [Matrix::sparseMatrix()] containing the weight data.
#'  Defaults to being automatically calculated from `settings`.
#'
#' @param include_data [Matrix::sparseMatrix()] containing the include data.
#'  Defaults to being automatically calculated from `settings`.
#'
#' @param boundary_data [Matrix::sparseMatrix()] containing the boundary data.
#'  Defaults to being automatically extracted from `dataset`.
#'
#' @param gap `numeric` relative optimality gap value. Defaults to 0.
#'
#' @param boundary_budget_proportion `numeric` gap value used to control
#'   the level of spatial clustering in the solution. Defaults to 0.1
#'
#' @param legend_color `character` legend color.
#'
#' @return A [Solution] object containing the solution.
#'
#' @examples
#' # TODO.
#' @export
min_shortfall_solution <- function(name, dataset, settings,
                                   area_budget_proportion,
                                   theme_data = settings$get_theme_data(),
                                   weight_data = settings$get_weight_data(),
                                   include_data = settings$get_include_data(),
                                   boundary_data = dataset$get_boundary_data(),
                                   gap = 0,
                                   boundary_budget_proportion = 0.1,
                                   legend_color = "#FF0000") {
  # validate arguments
  assertthat::assert_that(
    ## name
    assertthat::is.string(name),
    assertthat::noNA(name),
    ## dataset
    inherits(dataset, "Dataset"),
    ## settings
    inherits(settings, "SolutionSettings"),
    ## area_budget_proportion
    assertthat::is.number(area_budget_proportion),
    assertthat::noNA(area_budget_proportion),
    isTRUE(area_budget_proportion >= 0),
    isTRUE(area_budget_proportion <= 1),
    ## theme_data
    inherits(theme_data, "dgCMatrix"),
    ## weight_data
    inherits(weight_data, "dgCMatrix"),
    ## include_data
    inherits(include_data, "dgCMatrix"),
    ## gap
    assertthat::is.number(gap),
    assertthat::noNA(gap),
    ## boundary_budget_proportion
    assertthat::is.number(boundary_budget_proportion),
    assertthat::noNA(boundary_budget_proportion),
    isTRUE(boundary_budget_proportion >= 0),
    isTRUE(boundary_budget_proportion <= 1),
    ## legend_color
    assertthat::is.string(legend_color),
    assertthat::noNA(legend_color)
  )

  # prepare settings
  ## extract values
  theme_settings <- settings$get_theme_settings()
  weight_settings <- settings$get_weight_settings()
  include_settings <- settings$get_include_settings()
  ## ensure data match settings
  assertthat::assert_that(
    identical(theme_settings$id, rownames(theme_data)),
    identical(weight_settings$id, rownames(weight_data)),
    identical(include_settings$id, rownames(include_data))
  )

  # calculate targets
  ## extract values
  targets <-
    tibble::tibble(
      feature = theme_settings$id,
      type = "absolute",
      sense = ">=",
      target = dplyr::if_else(
        theme_settings$status,
        theme_settings$total * theme_settings$goal,
        theme_settings$total * theme_settings$limit
      )
    )
  ## round values down to account for floating point issues
  targets$target <- floor(targets$target * 1e+3) / 1e+3
  ## adjust values to prevent CBC from throwing an error and crashing R session
  targets$target <- pmax(targets$target, 1e-5)

  # calculate locked in values
  locked_in <- matrix(
    include_settings$status,
    byrow = TRUE,
    nrow = nrow(include_data), ncol = ncol(include_data)
  )
  locked_in <- as.logical(colSums(locked_in * include_data) > 0)

  # calculate locked out values
  ## initialize matrix
  locked_out <- as.matrix(weight_data)
  ## identify planning units to lock out per each weight
  for (i in seq_len(nrow(locked_out))) {
    ## skip if not using weights
    if (!weight_settings$status[i]) {
      locked_out[i, ] <- FALSE
    } else {
      ## identify threshold
      thresh <- quantile(
        x = locked_out[i, ],
        probs = min((100 - weight_settings$factor[i]) / 100, 1),
        names = FALSE
      )
      ## identify planning units to lock out for given weight
      locked_out[i, ] <- locked_out[i, ] > thresh
    }
  }
  ## identify planning unit to lock out for solutions
  ## note that locked in planning units are not locked out
  locked_out <- !locked_in & (colSums(locked_out) > 0.5)

  # calculate cost values
  pu_areas <- dataset$get_planning_unit_areas()
  cost <- scales::rescale(pu_areas, to = c(0.01, 1))

  # calculate budgets for multi-objective optimization
  total_budget <- sum(cost) * area_budget_proportion
  if (boundary_budget_proportion >= 1e-5) {
    initial_budget <- (1 - boundary_budget_proportion) * total_budget
  } else {
    initial_budget <- total_budget
  }

  # verify that problem if feasible with locked in planning units
  if (sum(cost[locked_in]) > min(initial_budget, total_budget)) {
    stop("code_1")
  }

  # calculate feature data
  features <-
    data.frame(id = seq_len(nrow(theme_settings)), name = theme_settings$id)

  # generate initial prioritization
  ## this simply just aims to maximize feature representation given
  ## the area budget, and locked in/out constraints
  initial_problem <-
    suppressWarnings(prioritizr::problem(cost, features, theme_data)) %>%
    prioritizr::add_min_shortfall_objective(budget = initial_budget) %>%
    prioritizr::add_manual_targets(targets) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_cbc_solver(gap = gap, verbose = FALSE)
  ### add locked in constraints if needed
  if (any(locked_in)) {
    initial_problem <-
      initial_problem %>%
      prioritizr::add_locked_in_constraints(locked_in)
  }
  ### add locked out constraints if needed
  if (any(locked_out)) {
    initial_problem <-
      initial_problem %>%
      prioritizr::add_locked_out_constraints(locked_out)
  }
  ### generate solution
  initial_solution <-
    c(prioritizr::solve(initial_problem, run_checks = FALSE))

  # generate second prioritization
  ## this formulation aims to minimize fragmentation,
  ## whilst ensuring that total cost does do not exceed the budget
  if (boundary_budget_proportion >= 1e-5) {
    ### calculate targets based on feature representation in initial solution
    main_targets <- targets
    main_targets$target <- rowSums(
      matrix(
        initial_solution,
        byrow = TRUE,
        ncol = ncol(theme_data), nrow = nrow(theme_data)
      ) *
        theme_data
    )
    ### prepare adjacency matrix for connectivity penalties
    ### note we use connectivity penalties because we want the solution
    ### to be as near as possible to the budget, even if the result has
    ### high perimeter because we included planning units with high perimeter
    adj_data <- boundary_data
    Matrix::diag(adj_data) <- 0
    adj_data <- Matrix::drop0(adj_data)
    ### generate prioritization
    main_problem <-
      suppressWarnings(
        prioritizr::problem(
          rep(0, length(cost)),
          rbind(
            features,
            data.frame(id = nrow(main_targets) + 1, name = "cost")
          ),
          rbind(theme_data, cost)
        )
      ) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_connectivity_penalties(
        penalty = 1, data = adj_data
      ) %>%
      prioritizr::add_manual_targets(
        rbind(
          main_targets,
          tibble::tibble(
            feature = "cost",
            type = "absolute",
            sense = "<=",
            target = total_budget
          )
        )
      ) %>%
      prioritizr::add_binary_decisions() %>%
      prioritizr::add_cbc_solver(gap = gap, verbose = FALSE)
    ### add locked in constraints if needed
    if (any(locked_in)) {
      main_problem <-
        main_problem %>%
        prioritizr::add_locked_in_constraints(locked_in)
    }
    ### add locked out constraints if needed
    if (any(locked_out)) {
      main_problem <-
        main_problem %>%
        prioritizr::add_locked_out_constraints(locked_out)
    }
    ### generate solution
    main_solution <-
      c(prioritizr::solve(main_problem, run_checks = FALSE))
  } else {
    ### if the boundary_gap setting is very low,
    ### then we will just use the initial solution because the
    ### second prioritization is unlikely to be very different from the first
    main_solution <- initial_solution
    main_problem <- initial_problem
  }

  # calculate results
  ## summary statistics
  ### preliminary calculations
  area_m <- sum(main_solution * pu_areas)
  perimeter_m <-
    prioritizr::eval_boundary_summary(
      x = main_problem,
      solution = main_solution,
      data = boundary_data
    )$boundary[[1]]
  reserve_sizes_m <-
    reserve_sizes(
      x = main_solution, areas = pu_areas, boundary_matrix = boundary_data
    )
  ### calculate statistics
  statistics_results <-
    list(
      new_statistic(
        name = "Total size",
        value = area_m * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      ),
      new_statistic(
        name = "Total perimeter",
        value = perimeter_m * 1e-3,
        units = "km"
      ),
      new_statistic(
        name = "Total number of reserves",
        value = length(reserve_sizes_m),
        units = ""
      ),
      new_statistic(
        name = "Smallest reserve size",
        value = min(reserve_sizes_m) * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      ),
      new_statistic(
        name = "Average reserve size",
        value = mean(reserve_sizes_m) * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      ),
      new_statistic(
        name = "Largest reserve size",
        value = max(reserve_sizes_m) * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      )
    )

  ## theme representation
  ### calculate amount held for each feature
  feature_held <- matrix(
    main_solution,
    byrow = TRUE,
    ncol = ncol(theme_data), nrow = nrow(theme_data)
  )
  feature_held <- rowSums(feature_held * theme_data) / rowSums(theme_data)
  names(feature_held) <- rownames(theme_data)

  ### calculate results for each theme separately
  theme_results <- lapply(seq_along(settings$themes), function(i) {
    #### extract theme
    th <- settings$themes[[i]]
    #### return theme results
    new_theme_results(
      theme = th,
      feature_results = lapply(seq_along(th$feature), function(f) {
        new_feature_results(
          feature = th$feature[[f]],
          held = feature_held[[th$feature[[f]]$id]]
        )
      })
    )
  })

  ## weight coverage
  ### calculate amount held for each weight
  weight_held <- matrix(
    main_solution,
    byrow = TRUE,
    ncol = ncol(weight_data), nrow = nrow(weight_data)
  )
  weight_held <- rowSums(weight_held * weight_data) / rowSums(weight_data)
  names(weight_held) <- rownames(weight_data)
  ### calculate results for each weight separately
  weight_results <- lapply(seq_along(settings$weights), function(i) {
    #### extract theme
    w <- settings$weights[[i]]
    #### return weight results
    new_weight_results(
      weight = w,
      held = weight_held[[w$id]]
    )
  })

  # identify index for storing data
  idx <- last(make.names(c(dataset$get_names(), name), unique = TRUE))
  idx <- gsub(".", "_", idx, fixed = TRUE)

  # create variable to store solution
  dataset$add_index(index = idx, values = main_solution)
  v <- new_variable(
    dataset = dataset,
    index = idx,
    total = sum(main_solution),
    units = "",
    legend = new_manual_legend(
      colors = c("#00FFFF00", legend_color),
      labels = c("not selected", "selected")
    )
  )

  # return solution object
  new_solution(
    name = name,
    variable = v,
    visible = TRUE,
    statistics = statistics_results,
    theme_results = theme_results,
    weight_results = weight_results,
    id = uuid::UUIDgenerate()
  )
}
