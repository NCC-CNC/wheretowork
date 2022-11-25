#' @include internal.R
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
#' @inherit min_set_result return
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
                                 id = uuid::UUIDgenerate()) {

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
    inherits(cache, "cachem")
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

  # calculate locked in values
  if (nrow(include_data) > 0) {
    ## if includes present, then use data and settings
    locked_in <- matrix(
      include_settings$status,
      byrow = FALSE,
      nrow = nrow(include_data), ncol = ncol(include_data)
    )
    locked_in <- as.logical(colSums(locked_in * include_data) > 0)
  } else {
    ## if no includes present, then lock nothing in
    locked_in <- rep(FALSE, ncol(include_data))
  }
  
  # calculate locked out values
  if (nrow(exclude_data) > 0) {
    ## if excludes present, then use data and settings
    locked_out <- matrix(
      exclude_settings$status,
      byrow = FALSE,
      nrow = nrow(exclude_data), ncol = ncol(exclude_data)
    )
    locked_out <- as.logical(colSums(locked_out * exclude_data) > 0)
  } else {
    ## if no excludes present, then lock nothing out
    locked_out <- rep(FALSE, ncol(exclude_data))
  }
  
  ### locked-out takes precedence if overlap is TRUE
  idx <- which(locked_in & locked_out)
  if (!overlap) {
    locked_out[idx] <- FALSE
  } else {
    locked_in[idx] <- FALSE
  }   
  
  # calculate weight data
  ## process weights with positive factors
  wn_pos_idx <- which(weight_settings$status & (weight_settings$factor > 0))
  if (length(wn_pos_idx) > 0) {
    ### add these weights as features
    features <- rbind(
      features,
      data.frame(
        id = max(features$id) + seq_along(wn_pos_idx),
        name = weight_settings$id[wn_pos_idx],
        stringsAsFactors = FALSE
      )
    )
    ### add these weights to rij data
    wn_pos_data <- weight_data[wn_pos_idx, , drop = FALSE]
    for (i in seq_len(nrow(wn_pos_data))) {
      wn_pos_data[i, ] <- scales::rescale(wn_pos_data[i, ], to = c(0.01, 100))
    }
    rij_data <- rbind(rij_data, wn_pos_data)
    ### add these weights to targets
    targets <- dplyr::bind_rows(
      targets,
      tibble::tibble(
        feature = weight_settings$id[wn_pos_idx],
        type = "absolute",
        sense = ">=",
        target = c(
          Matrix::rowSums(wn_pos_data) *
          (weight_settings$factor[wn_pos_idx] / 100)
        )
      )
    )
  }
  ## process weights with negative factors
  wn_neg_idx <- which(weight_settings$status & (weight_settings$factor < 0))
  if (length(wn_neg_idx) > 0) {
    ### add these weights to rij data
    wn_neg_data <- weight_data[wn_neg_idx, , drop = FALSE]
    for (i in seq_len(nrow(wn_neg_data))) {
      wn_neg_data[i, ] <- scales::rescale(wn_neg_data[i, ], to = c(0.01, 100))
    }
    wn_neg_thresholds <- c(
      Matrix::rowSums(wn_neg_data) *
      (1 - abs(weight_settings$factor[wn_neg_idx] / 100))
    )
  }

  # calculate cost values
  cost <- scales::rescale(area_data, to = c(0.01, 1))

  # calculate budgets for multi-objective optimization
  total_budget <- sum(cost) * area_budget_proportion
  if (boundary_gap >= 1e-5) {
    initial_budget <- (1 - boundary_gap) * total_budget
  } else {
    initial_budget <- total_budget
  }

  # verify that problem if feasible with locked in planning units
  if (sum(cost[locked_in]) > min(initial_budget, total_budget)) {
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
      area_budget_proportion = area_budget_proportion
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
      prioritizr::add_binary_decisions() %>%
      prioritizr::add_cbc_solver(
        verbose = verbose, gap = gap_1, time_limit = time_limit_1
      )
    ## add locked in constraints if needed
    if (any(locked_in[initial_pu_idx])) {
      initial_problem <-
        initial_problem %>%
        prioritizr::add_locked_in_constraints(locked_in[initial_pu_idx])
    }
    ## add locked out constraints if needed
    if (any(locked_out[initial_pu_idx])) {
      initial_problem <-
        initial_problem %>%
        prioritizr::add_locked_out_constraints(locked_out[initial_pu_idx])
    }    
    ### add linear constraints if needed
    if (length(wn_neg_idx) > 0) {
      for (i in seq_len(nrow(wn_neg_data))) {
        initial_problem <- prioritizr::add_linear_constraints(
          initial_problem,
          threshold = wn_neg_thresholds[i],
          sense = "<=",
          data = wn_neg_data[i, initial_pu_idx]
        )
      }
    }
    ## solve problem to generate solution if needed
    initial_solution <- rep(0, length(cost))
    initial_solution[initial_pu_idx] <- c(
      prioritizr::solve(initial_problem, run_checks = FALSE)
    )
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
    main_targets$target <- rowSums(
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
      prioritizr::add_cbc_solver(
         verbose = verbose, gap = gap_2, time_limit = time_limit_2,
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
    ### add linear constraints if needed
    if (length(wn_neg_idx) > 0) {
      for (i in seq_len(nrow(wn_neg_data))) {
        main_problem <- prioritizr::add_linear_constraints(
          main_problem,
          threshold = wn_neg_thresholds[i],
          sense = "<=",
          data = wn_neg_data[i, ]
        )
      }
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
