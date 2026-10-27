#' @include internal.R utils_calculate_area_budget_data.R
NULL

#' Generate result using minimum shortfall formulation
#'
#' Create a new [Result] object by generating a prioritization
#' using the minimum shortfall formulation of the reserve selection problem.
#'
#' @inheritParams min_set_result
#
#' @param area_budget_proportion `numeric` budget for the solution.
#'   This should be a proportion (with values ranging between 0 and 1)
#'   indicating the maximum spatial extent of the solution.
#'
#' @param weight_importance `numeric` total relative importance of the
#'   weights compared with the goals (which have an importance of 1).
#'   Must be greater than 0 and no more than 1. Defaults to 0.25.
#'
#' @param min_budget_proportion `numeric` minimum proportion of the area
#'   budget that the solution must use when weights are enabled and the goals
#'   cannot all be met (ranging between 0 and 1), or as much as is feasible.
#'   Defaults to 0.99.
#'
#' @inherit min_set_result return
#'
#' @details
#' If any weights are enabled, then the goals (minimum shortfall objective)
#' and each weight (maximum weighted sum objective) are combined using the
#' reference point approach (see [prioritizr::add_ref_point_approach()]).
#' The goals have an importance of 1, and `weight_importance` is shared among
#' the weights in proportion to their factors. Each weight is rescaled by its
#' total. The best value for each weight is calculated by solving its problem
#' with proportion decisions, subject to the same constraints as the
#' solution. The best value for the goals is stored in `cache`. If the goals can
#' all be met within the area budget, then solutions must meet them;
#' otherwise solutions must use at least `min_budget_proportion` of the
#' area budget.
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#' v3 <- new_variable_from_auto(dataset = d, index = 3)
#' v4 <- new_variable_from_auto(dataset = d, index = 4)
#' v5 <- new_variable_from_auto(dataset = d, index = 5)
#' v6 <- new_variable_from_auto(dataset = d, index = 6)
#'
#' # create a weight using a variable
#' w <- new_weight(
#'   name = "Human Footprint Index", variable = v1,
#'   factor = -90, status = FALSE, id = "W1"
#' )
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   goal = 0.2, status = FALSE, current = 0.5, id = "F1"
#' )
#' f2 <- new_feature(
#'   name = "Forests", variable = v3,
#'   goal = 0.3, status = FALSE, current = 0.9, id = "F2"
#' )
#' f3 <- new_feature(
#'   name = "Shrubs", variable = v4,
#'   goal = 0.6, status = TRUE, current = 0.4, id = "F3"
#' )
#'
#' # create themes using the features
#' t1 <- new_theme("Species", f1, id = "T1")
#' t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
#'
#' # create an included using a variable
#' i <- new_include(
#'   name = "Protected areas", variable = v5,
#'   status = FALSE, id = "I1"
#' )
#'
#' # create an excluded using a variable
#' e <- new_exclude(
#'   name = "Expensive areas", variable = v6,
#'   status = FALSE, id = "E1"
#' )
#'
#' # create parameters
#' p1 <- new_parameter(name = "Spatial clustering")
#' p2 <- new_parameter(name = "Optimality gap")
#'
#' # create solution settings using the themes and weight
#' ss <- new_solution_settings(
#'   themes = list(t1, t2), weights = list(w), includes = list(i),
#'   excludes = list(e), parameters = list(p1, p2)
#' )
#'
#' # create result
#' x <- min_shortfall_result(
#'  id = "R1",
#'  area_budget_proportion = 0.78,
#'  area_data = d$get_planning_unit_areas(),
#'  boundary_data = d$get_boundary_data(),
#'  theme_data = ss$get_theme_data(),
#'  weight_data = ss$get_weight_data(),
#'  include_data = ss$get_include_data(),
#'  exclude_data = ss$get_exclude_data(),
#'  theme_settings = ss$get_theme_settings(),
#'  weight_settings = ss$get_weight_settings(),
#'  include_settings = ss$get_include_settings(),
#'  exclude_settings = ss$get_exclude_settings(),
#'  parameters = ss$parameters,
#'  gap_1 = p2$value * p2$status,
#'  boundary_gap = p1$value * p1$status
#' )
#'
#' # print object
#' print(x)
#' @export
min_shortfall_result <- function(area_budget_proportion,
                                 area_data,
                                 boundary_data,
                                 theme_data,
                                 weight_data,
                                 include_data,
                                 exclude_data,
                                 theme_settings,
                                 weight_settings,
                                 include_settings,
                                 exclude_settings,
                                 parameters,
                                 overlap = FALSE,
                                 gap_1 = 0,
                                 gap_2 = 0,
                                 boundary_gap = 0.1,
                                 cache = cachem::cache_mem(),
                                 time_limit_1 = .Machine$integer.max,
                                 time_limit_2 = .Machine$integer.max,
                                 verbose = FALSE,
                                 id = uuid::UUIDgenerate(),
                                 try_gurobi = FALSE,
                                 weight_importance = 0.25,
                                 min_budget_proportion = 0.99) {

  # validate arguments
  assertthat::assert_that(
    ## id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ## area_budget_proportion
    assertthat::is.number(area_budget_proportion),
    assertthat::noNA(area_budget_proportion),
    isTRUE(area_budget_proportion >= 0),
    isTRUE(area_budget_proportion <= 1),
    ## time_limit_1
    assertthat::is.count(time_limit_1),
    assertthat::noNA(time_limit_1),
    ## time_limit_2
    assertthat::is.count(time_limit_2),
    assertthat::noNA(time_limit_2),
    ## area_data
    is.numeric(area_data),
    assertthat::noNA(area_data),
    ## boundary_data
    inherits(boundary_data, c("dsCMatrix", "dgCMatrix")),
    ncol(boundary_data) == length(area_data),
    ## theme_data
    inherits(theme_data, "dgCMatrix"),
    ncol(theme_data) == length(area_data),
    ## weight_data
    inherits(weight_data, "dgCMatrix"),
    ncol(weight_data) == length(area_data),
    ## include_data
    inherits(include_data, "dgCMatrix"),
    ncol(include_data) == length(area_data),
    ## exclude_data
    inherits(exclude_data, "dgCMatrix"),
    ncol(exclude_data) == length(area_data),    
    ## theme_settings
    inherits(theme_settings, "data.frame"),
    nrow(theme_settings) == nrow(theme_data),
    identical(theme_settings$id, rownames(theme_data)),
    ## weight_settings
    inherits(weight_settings, "data.frame"),
    nrow(weight_settings) == nrow(weight_data),
    ## include_settings
    inherits(include_settings, "data.frame"),
    nrow(include_settings) == nrow(include_data),
    ## exclude_settings
    inherits(exclude_settings, "data.frame"),
    nrow(exclude_settings) == nrow(exclude_data),    
    ## parameters
    is.list(parameters),
    all_list_elements_inherit(parameters, "Parameter"),
    ## overlap
    assertthat::noNA(overlap),
    assertthat::is.flag(overlap),
    ## gap_1
    assertthat::is.number(gap_1),
    assertthat::noNA(gap_1),
    ## gap_2
    assertthat::is.number(gap_2),
    assertthat::noNA(gap_2),
    ## boundary_gap
    assertthat::is.number(boundary_gap),
    assertthat::noNA(boundary_gap),
    ## cache
    inherits(cache, "cachem"),
    ## try_gurobi
    assertthat::is.flag(try_gurobi),
    ## weight_importance
    assertthat::is.number(weight_importance),
    assertthat::noNA(weight_importance),
    isTRUE(weight_importance > 0),
    isTRUE(weight_importance <= 1),
    ## min_budget_proportion
    assertthat::is.number(min_budget_proportion),
    assertthat::noNA(min_budget_proportion),
    isTRUE(min_budget_proportion >= 0),
    isTRUE(min_budget_proportion <= 1)
  )
  if (nrow(weight_settings) > 0) {
    assertthat::assert_that(
      identical(weight_settings$id, rownames(weight_data))
    )
  }
  if (nrow(include_settings) > 0) {
    assertthat::assert_that(
      identical(include_settings$id, rownames(include_data))
    )
  }
  if (nrow(exclude_settings) > 0) {
    assertthat::assert_that(
      identical(exclude_settings$id, rownames(exclude_data))
    )
  }  

  # generate feature data
  features <- data.frame(
    id = seq_len(nrow(theme_settings)),
    name = theme_settings$id,
    stringsAsFactors = FALSE
  )

  # generate rij data
  rij_data <- theme_data

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
  ## adjust values to prevent solver from throwing error and crashing R session
  targets$target <- pmax(targets$target, 1e-5)

  # calculate locked in/out planning units and area budget feasibility
  budget_info <- calculate_area_budget_data(
    area_data = area_data,
    include_data = include_data,
    include_settings = include_settings,
    exclude_data = exclude_data,
    exclude_settings = exclude_settings,
    overlap = overlap,
    area_budget_proportion = area_budget_proportion,
    boundary_gap = boundary_gap
  )
  locked_in <- budget_info$locked_in
  locked_out <- budget_info$locked_out

  # calculate weight data
  ## each weight is an objective (not a constraint), #414 and #446
  wn_idx <- which(
    weight_settings$status & (abs(weight_settings$factor) > 1e-10)
  )
  ## skip weights with only zero values
  wn_idx <- wn_idx[
    Matrix::rowSums(abs(weight_data[wn_idx, , drop = FALSE])) > 1e-10
  ]
  wn_data <- weight_data[wn_idx, , drop = FALSE]
  ## scale values to avoid numerical issues
  for (i in seq_len(nrow(wn_data))) {
    wn_data[i, ] <- wn_data[i, ] * (100 / max(abs(wn_data[i, ])))
  }
  ## negative weights are multiplied by -1 so high values are avoided
  wn_sign <- sign(weight_settings$factor[wn_idx])
  ## share the total weight importance in proportion to the factors
  wn_importance <- abs(weight_settings$factor[wn_idx])
  wn_importance <- weight_importance * wn_importance / sum(wn_importance)

  # extract cost and budget values calculated above
  cost <- budget_info$cost
  total_budget <- budget_info$total_budget
  initial_budget <- budget_info$initial_budget

  # verify that problem if feasible with locked in planning units
  if (budget_info$exceeded) {
    stop("WtW: Total area budget setting is too low given the selected",
         "Includes. Try increasing the total area budget or deselecting ",
         " some of the Includes.")
  }

  # generate cache key based on settings
  key <- digest::digest(
    list(
      themes = theme_settings,
      weights = weight_settings,
      includes = include_settings,
      excludes = exclude_settings,
      overlap = overlap,
      area_budget_proportion = area_budget_proportion,
      ## spatial clustering reduces the budget for the initial solution
      initial_budget = initial_budget,
      weight_importance = weight_importance,
      min_budget_proportion = min_budget_proportion
    )
  )

  # generate solution
  if (!isTRUE(cache$exists(key))) {
    ## extract indices for planning unit with at least some data
    initial_pu_idx <- which(
      Matrix::colSums(theme_data) > 0 |
      Matrix::colSums(weight_data) > 0 |
      Matrix::colSums(include_data) > 0 |
      Matrix::colSums(exclude_data) > 0
    )
    ## calculate minimum budget so weights cannot reduce the area, #446,
    ## bounded so that the problem is always feasible
    initial_avail_cost <- cost[initial_pu_idx][!locked_out[initial_pu_idx]]
    initial_min_budget <- 0
    if (length(initial_avail_cost) > 0) {
      initial_min_budget <- min(
        min_budget_proportion * initial_budget,
        initial_budget - max(initial_avail_cost),
        sum(initial_avail_cost)
      )
    }
    ## generate initial prioritization problem with subset of planning units
    ## this is needed to prevent CBC from crashing, #158
    ## this prioritization just aims to maximize feature representation given
    ## the area budget, and locked in/out constraints
    initial_problem <-
      suppressWarnings(prioritizr::problem(
        x = cost[initial_pu_idx],
        features = features,
        rij_matrix = rij_data[, initial_pu_idx, drop = FALSE])
      ) %>%
      prioritizr::add_min_shortfall_objective(budget = initial_budget) %>%
      prioritizr::add_manual_targets(targets) %>%
      prioritizr::add_binary_decisions()

    ## define functions to add constraints, so that the problems used to
    ## calculate the best value for each weight have the same constraints
    ## as the initial problem
    ### add locked in/out constraints if needed
    add_locked_constraints <- function(x) {
      if (any(locked_in[initial_pu_idx])) {
        x <- prioritizr::add_locked_in_constraints(
          x, locked_in[initial_pu_idx]
        )
      }
      if (any(locked_out[initial_pu_idx])) {
        x <- prioritizr::add_locked_out_constraints(
          x, locked_out[initial_pu_idx]
        )
      }
      x
    }
    ### if all goals can be met, then add constraints to meet them,
    ### otherwise add the minimum budget constraint if needed
    add_goal_or_budget_constraints <- function(x, goals_met) {
      if (goals_met) {
        for (i in seq_len(nrow(targets))) {
          x <- prioritizr::add_linear_constraints(
            x,
            threshold = floor(targets$target[[i]] * 1e+3) / 1e+3,
            sense = ">=",
            data = as.numeric(rij_data[i, initial_pu_idx])
          )
        }
      } else if (initial_min_budget > 1e-5) {
        x <- prioritizr::add_cost_constraints(
          x, budget = initial_min_budget, sense = ">="
        )
      }
      x
    }

    ## add locked in/out constraints
    initial_problem <- add_locked_constraints(initial_problem)

    ## if no weights are enabled, then just solve the primary problem
    if (length(wn_idx) == 0) {
      initial_problem <- add_wtw_solver(
        initial_problem, try_gurobi = try_gurobi, verbose = verbose,
        gap = gap_1, time_limit = time_limit_1
      )
      initial_raw_solution <-
        prioritizr::solve.ConservationProblem(initial_problem, run_checks = FALSE)
    } else {
      ### solve primary problem for its best value,
      ### cached since it does not depend on the weights
      primary_key <- digest::digest(
        list(
          "primary", targets, initial_pu_idx, locked_in[initial_pu_idx],
          locked_out[initial_pu_idx], initial_budget
        )
      )
      if (!isTRUE(cache$exists(primary_key))) {
        primary_solution <- prioritizr::solve.ConservationProblem(
          add_wtw_solver(
            initial_problem, try_gurobi = try_gurobi, verbose = verbose,
            gap = gap_1, time_limit = time_limit_1
          ),
          run_checks = FALSE
        )
        cache$set(primary_key, attr(primary_solution, "objective")[[1]])
      }
      primary_best <- cache$get(primary_key)
      ### if all goals can be met, then require them (and no minimum budget),
      ### otherwise require the minimum budget
      goals_met <- primary_best < 1e-5
      initial_problem <- add_goal_or_budget_constraints(
        initial_problem, goals_met
      )
      ### create an objective for each weight, without decisions so that
      ### they can be used with binary or proportion decisions
      weight_objectives <- lapply(seq_along(wn_idx), function(i) {
        suppressMessages(suppressWarnings(
          prioritizr::problem(
            x = cost[initial_pu_idx],
            features = data.frame(id = 1, name = weight_settings$id[wn_idx[i]]),
            rij_matrix = wn_sign[i] * wn_data[i, initial_pu_idx, drop = FALSE]
          ) %>%
          prioritizr::add_max_wtd_sum_objective(budget = NULL)
        ))
      })
      ### create a problem for each weight (constraints are in the primary)
      weight_problems <- lapply(
        weight_objectives, prioritizr::add_binary_decisions
      )
      names(weight_problems) <- paste0("weight_", seq_along(wn_idx))
      ### calculate the best value for each weight, subject to the same
      ### constraints as the initial problem, using proportion decisions
      ### so that the value is never lower than the best value with binary
      ### decisions (which would bias the solution against the weight), #446
      weight_best <- vapply(
        seq_along(weight_objectives), FUN.VALUE = numeric(1), function(i) {
          p <-
            weight_objectives[[i]] %>%
            prioritizr::add_cost_constraints(
              budget = initial_budget, sense = "<="
            ) %>%
            add_locked_constraints() %>%
            add_goal_or_budget_constraints(goals_met) %>%
            prioritizr::add_proportion_decisions() %>%
            add_wtw_solver(
              try_gurobi = try_gurobi, verbose = verbose,
              gap = 0, time_limit = time_limit_1
            )
          s <- prioritizr::solve.ConservationProblem(p, run_checks = FALSE)
          attr(s, "objective")[[1]]
        }
      )
      ### worst primary value is a 100% shortfall for every target, and the
      ### worst value for each weight is set so that best - worst is its
      ### total, so that each weight is rescaled by its total
      weight_total <- Matrix::rowSums(abs(wn_data))
      mo_best <- c(primary_best, weight_best)
      mo_worst <- c(nrow(targets), weight_best - weight_total)
      ### combine problems using the reference point approach
      mo_problem <-
        do.call(
          prioritizr::multi_problem,
          append(list(primary = initial_problem), weight_problems)
        ) %>%
        prioritizr::add_ref_point_approach(
          weights = c(1, wn_importance),
          best = mo_best,
          worst = mo_worst,
          verbose = verbose
        ) %>%
        add_wtw_solver(
          try_gurobi = try_gurobi, verbose = verbose,
          gap = gap_1, time_limit = time_limit_1
        )
      ### solve problem
      initial_raw_solution <- prioritizr::solve.MultiConservationProblem(
        mo_problem, run_checks = FALSE
      )
    }
    ## generate solution with all planning units
    initial_solution <- rep(0, length(cost))
    initial_solution[initial_pu_idx] <- c(initial_raw_solution)
    ## store solution in cache
    cache$set(key, initial_solution)
  }

  # extract/prepare solution and problem for for subsequent analysis
  ## extract solution from cache
  initial_solution <- cache$get(key)
  ## initial problem formulation with all planning units
  initial_problem <- suppressWarnings(prioritizr::problem(
    x = cost, features = features, rij_matrix = rij_data
  ))

  # generate second prioritization
  ## this formulation aims to minimize fragmentation,
  ## whilst ensuring that total cost does do not exceed the budget
  if (boundary_gap >= 1e-5) {
    ### calculate targets based on feature representation in initial solution
    main_targets <- targets
    main_targets$target <- Matrix::rowSums(
      matrix(
        initial_solution,
        byrow = TRUE,
        ncol = ncol(rij_data), nrow = nrow(rij_data)
      ) *
      rij_data
    )
    ### prepare adjacency matrix for connectivity penalties
    ### note we use connectivity penalties because we want the solution
    ### to be as near as possible to the budget, even if the result has
    ### high perimeter because we included planning units with high perimeter
    adj_data <- boundary_data
    adj_data@x <- rep(1, length(adj_data@x))
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
          rbind(rij_data, cost)
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
      add_wtw_solver(
        try_gurobi = try_gurobi, verbose = verbose,
        gap = gap_2, time_limit = time_limit_2,
        start_solution = initial_solution
      )

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
    ### add constraints so spatial clustering does not compromise the weights
    for (i in seq_along(wn_idx)) {
      wn_initial <- sum(initial_solution * wn_data[i, ])
      if (wn_sign[i] > 0) {
        main_problem <- prioritizr::add_linear_constraints(
          main_problem,
          threshold = floor(wn_initial * 1e+3) / 1e+3,
          sense = ">=",
          data = wn_data[i, ]
        )
      } else if (initial_budget > 1e-5) {
        main_problem <- prioritizr::add_linear_constraints(
          main_problem,
          threshold =
            ceiling(wn_initial * (total_budget / initial_budget) * 1e+3) / 1e+3,
          sense = "<=",
          data = wn_data[i, ]
        )
      }
    }
    ### generate solution
    main_solution <-
      c(prioritizr::solve.ConservationProblem(main_problem, run_checks = FALSE))
  } else {
    ### if the boundary_gap setting is very low,
    ### then we will just use the initial solution because the
    ### second prioritization is unlikely to be very different from the first
    main_solution <- initial_solution
    main_problem <- initial_problem
  }

  # calculate spatial variables
  total_area <- sum(main_solution * area_data)
  total_perimeter <- prioritizr::eval_boundary_summary(
    x = main_problem,
    solution = main_solution,
    data = boundary_data
  )$boundary[[1]]

  # generate results object
  new_result(
    values = main_solution,
    area = total_area,
    perimeter = total_perimeter,
    theme_coverage = calculate_coverage(main_solution, theme_data),
    weight_coverage = calculate_coverage(main_solution, weight_data),
    include_coverage = calculate_coverage(main_solution, include_data),
    exclude_coverage = calculate_coverage(main_solution, exclude_data),
    theme_settings = theme_settings,
    weight_settings = weight_settings,
    include_settings = include_settings,
    exclude_settings = exclude_settings,
    parameters = parameters
  )
}
