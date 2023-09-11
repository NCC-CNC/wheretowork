#' @include internal.R
NULL

#' Generate results using minimum set formulation
#'
#' Create a new [Result] object by generating a prioritization
#' using the minimum set formulation of the reserve selection problem.
#'
#' @param id `character` identifier for new result.
#'
#' @param area_data `numeric` vector containing the area of each planning unit.
#'
#' @param boundary_data [Matrix::sparseMatrix()] containing the boundary data,
#' or or `NA`. 
#'
#' @param theme_data [Matrix::sparseMatrix()] containing the theme data.
#'
#' @param weight_data [Matrix::sparseMatrix()] containing the weight data.
#'
#' @param include_data [Matrix::sparseMatrix()] containing the include data.
#' 
#' @param exclude_data [Matrix::sparseMatrix()] containing the exclude data.
#'
#' @param theme_settings `data.frame` containing the theme settings.
#'
#' @param weight_settings `data.frame` containing the weight settings.
#'
#' @param include_settings  `data.frame` containing the include settings.
#' 
#' @param exclude_settings  `data.frame` containing the exclude settings.
#'
#' @param parameters  `list` of [Parameter] objects.
#' 
#' @param overlap  `logical` if `TRUE`, excludes takes precedence over includes.
#'
#' @param gap_1 `numeric` relative optimality gap value for initial
#'   optimization.
#'   Defaults to 0.
#'
#' @param gap_2 `numeric` relative optimality gap value for spatial clustering.
#'   Defaults to 0.
#'
#' @param boundary_gap `numeric` value used to control
#'   the level of spatial clustering in the solution. Defaults to 0.1.
#'
#' @param cache [cachem::cache_mem()] object used to cache intermediate
#'  calculations.
#'  Defaults to an empty cache such that (effectively) no cache is used.
#'
#' @param time_limit_1 `numeric` time limit (seconds) for initial
#'   optimization run.
#'  Defaults to the maximum integer.
#'
#' @param time_limit_2 `numeric` time limit (seconds) for spatial clustering
#'   optimization run.
#'  Defaults to the maximum integer.
#'
#' @param verbose `logical` value indicating if information should be
#'  displayed when generating solutions. Defaults to `FALSE`.
#'  
#' @param try_gurobi `logical` value indicating if the Gurobi solver should be
#'  used when generating solutions. Defaults to `FALSE`.
#'
#' @return A [Solution] object containing the solution.
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
#' x <- min_set_result(
#'  id = "R1",
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
#'  overlap = TRUE,
#'  gap_1 = p2$value * p2$status,
#'  boundary_gap = p1$value * p1$status
#' )
#'
#' # print object
#' print(x)
#' @export
min_set_result <- function(area_data,
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
                           try_gurobi = FALSE) {
  
  # validate arguments
  assertthat::assert_that(
    ## id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ## area_data
    is.numeric(area_data),
    assertthat::noNA(area_data),
    ## boundary_data
    inherits(boundary_data, c("dsCMatrix", "dgCMatrix", "logical")),
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
    assertthat::is.flag(overlap),
    assertthat::noNA(overlap),    
    ## gap_1
    assertthat::is.number(gap_1),
    assertthat::noNA(gap_1),
    ## gap_2
    assertthat::is.number(gap_2),
    assertthat::noNA(gap_2),
    ## time_limit_1
    assertthat::is.count(time_limit_1),
    assertthat::noNA(time_limit_1),
    ## time_limit_2
    assertthat::is.count(time_limit_2),
    assertthat::noNA(time_limit_2),
    ## boundary_gap
    assertthat::is.number(boundary_gap),
    assertthat::noNA(boundary_gap),
    ## cache
    inherits(cache, "cachem"),
    ## try_gurobi
    assertthat::is.flag(try_gurobi)
  )
  
  if (inherits(boundary_data, c("dsCMatrix", "dgCMatrix"))) {
    assertthat::assert_that(
      ncol(boundary_data) == length(area_data)
    )
  }
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

  # calculate targets
  ## extract values
  targets <- tibble::tibble(
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
    ## if exclude present, then use data and settings
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
  
  # verify that problem if feasible with locked out planning units
  if (!all(Matrix::rowSums(theme_data[,!locked_out]) >= targets$target)) {
    # get features that make solution impossible to meet
    fidx <- which(Matrix::rowSums(theme_data[,!locked_out]) < targets$target)
    theme_names <- theme_settings[fidx, ]$name
    theme_names <- paste(paste0('"', theme_names, '"'), collapse = ", ")
    
    stop("WtW: Exclude error: the following features can not have their goal", 
         " met: ", theme_names, ". Try reducing the goals on these features, ", 
         " deselecting them or turn off exclude layers from the scenario.")
  } 
  
  # calculate cost values
  if (nrow(weight_data) > 0) {
    ## if weights present, then use data and settings
    ### normalize cost values
    wn_idx <- which(weight_settings$status > 0)
    wn <- weight_data[wn_idx, , drop = FALSE]
    for (i in seq_len(nrow(wn))) {
      #### z-score weight data
      wn[i, ] <- zscale(wn[i, ])
      ### determine if zeros present in data
      has_zero <- abs(min(wn[i, ], na.rm = TRUE)) < 1e-10
      if (weight_settings$factor[wn_idx[i]] > 0) {
        ### if positive weight,
        ### then re-scale so that larger values mean lower costs
        wn[i, ] <- scales::rescale(
          wn[i, ], to = c(1, ifelse(has_zero, 0, 0.01))
        )
      } else {
        ### if negative weight,
        ### then re-scale so that larger values mean higher costs
        wn[i, ] <- scales::rescale(
          wn[i, ], to = c(ifelse(has_zero, 0, 0.01), 1)
        )
      }
    }

    ### apply factors and status settings
    cost <- matrix(
      rep(abs(weight_settings$factor[wn_idx]), each = ncol(wn)),
      byrow = TRUE,
      nrow = nrow(wn),
      ncol = ncol(wn)
    )
    ### calculate total cost by summing together all weight values
    cost <- colSums(cost * wn)
    ### re-scale cost values to avoid numerical issues
    cost <- scales::rescale(cost, to = c(0.01, 1000))
  } else {
    ## if no weights present, then set cost values as constant
    cost <- rep(1, ncol(weight_data))
  }

  # calculate feature data
  features <- data.frame(
    id = seq_len(nrow(theme_settings)),
    name = theme_settings$id
  )

  # generate cache key based on settings
  key <- digest::digest(
    list(
      themes = theme_settings,
      weights = weight_settings,
      includes = include_settings,
      excludes = exclude_settings,
      overlap = overlap
    )
  )

  ## add fake feature to avoid sparsity data crashing with CBC, #173
  fake_feature_name <- uuid::UUIDgenerate()
  features_inc_fake <- rbind(
    features,
    data.frame(id = nrow(features) + 1, name = fake_feature_name)
  )
  targets_inc_fake <- rbind(
    targets,
    tibble::tibble(
      feature = fake_feature_name,
      type = "absolute",
      sense = ">=",
      target = 0
    )
  )
  theme_data_inc_fake <- rbind(
    theme_data,
    round(stats::runif(ncol(theme_data)), 3)
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
    ### this problem just aims to minimize cost
    initial_problem <-
      suppressWarnings(prioritizr::problem(
        x = cost[initial_pu_idx],
        features = features_inc_fake,
        rij_matrix = theme_data_inc_fake[, initial_pu_idx, drop = FALSE])
      ) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_manual_targets(targets_inc_fake) %>%
      prioritizr::add_binary_decisions() %>%
      #### set solver
      {if (try_gurobi) 
        prioritizr::add_gurobi_solver(
          ., verbose = verbose, gap = gap_1, time_limit = time_limit_1
        ) 
       else
         prioritizr::add_cbc_solver(
           ., verbose = verbose, gap = gap_1, time_limit = time_limit_1
        )
      }
    
    ### add locked in constraints if needed
    if (any(locked_in[initial_pu_idx])) {
      initial_problem <-
        initial_problem %>%
        prioritizr::add_locked_in_constraints(locked_in[initial_pu_idx])
    }
    ### add locked out constraints if needed
    if (any(locked_out[initial_pu_idx])) {
      initial_problem <-
        initial_problem %>%
        prioritizr::add_locked_out_constraints(locked_out[initial_pu_idx])
    }    
    ### solve problem to generate solution if needed
    initial_solution <- rep(0, length(cost))
    initial_solution[initial_pu_idx] <- c(
      prioritizr::solve(initial_problem, run_checks = FALSE)
    )
    ### store solution in cache
    cache$set(key, initial_solution)
  }

  # extract/prepare solution and problem for for subsequent analysis
  ## extract solution from cache
  initial_solution <- cache$get(key)
  ## initial problem formulation with all planning units
  initial_problem <- suppressWarnings(prioritizr::problem(
    x = cost, features = features, rij_matrix = theme_data
  ))

  # generate second prioritization
  ## this formulation aims to minimize fragmentation,
  ## whilst ensuring that total cost does do not exceed a threshold
  if ((boundary_gap >= 1e-5) && inherits(boundary_data, c("dsCMatrix", "dgCMatrix"))) {
    ### calculate cost constraint for new prioritization
    max_cost <- sum(initial_solution * cost) * (boundary_gap + 1)
    ### identify "important" planning units to lock in to speed up process
    rwr <- prioritizr::eval_rare_richness_importance(
      initial_problem, initial_solution
    )
    if (any(rwr >= 1e-5)) { 
      rwr_threshold <- stats::median(rwr[rwr > 1e-5])
      locked_in <- locked_in | ((rwr >= rwr_threshold) & (rwr > 1e-5))
    }
    locked_in <- locked_in & !locked_out
    ### prepare boundary data as connectivity data based on "importance"
    #### calculate importance
    rwr_raw <- prioritizr_internal_eval_rare_richness_importance(
      initial_problem, seq_along(cost), TRUE
    )
    #### create matrix
    con_data <- boundary_data
    Matrix::diag(con_data) <- 0
    con_data <- Matrix::drop0(con_data)
    con_data <- methods::as(con_data, "dgTMatrix")
    con_data@x <- rwr_raw[con_data@i + 1] + cost[con_data@j + 1]
    con_data@x <- scales::rescale(con_data@x, to = c(1, 0.01))
    con_data <- Matrix::drop0(con_data)
    ### generate prioritization
    main_problem <-
      suppressWarnings(
        prioritizr::problem(
          rep(0, length(cost)),
          rbind(features, data.frame(id = nrow(targets) + 1, name = "cost")),
          rbind(theme_data, cost)
        )
      ) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_connectivity_penalties(
        penalty = 1, data = con_data
      ) %>%
      prioritizr::add_manual_targets(
        rbind(
          targets,
          tibble::tibble(
            feature = "cost",
            type = "absolute",
            sense = "<=",
            target = max_cost
          )
        )
      ) %>%
      prioritizr::add_binary_decisions() %>%
      #### set solver
      {if (try_gurobi) 
        prioritizr::add_gurobi_solver(
          ., verbose = verbose, gap = gap_2, time_limit = time_limit_2,
          start_solution = pmax(initial_solution, locked_in)
        ) 
       else
         prioritizr::add_cbc_solver(
           ., verbose = verbose, gap = gap_2, time_limit = time_limit_2,
           start_solution = pmax(initial_solution, locked_in)
        )
      }
    
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

  # calculate spatial variables
  total_area <- sum(main_solution * area_data)
  if (inherits(boundary_data, c("dsCMatrix", "dgCMatrix"))) {
    total_perimeter <- prioritizr::eval_boundary_summary(
      x = main_problem,
      solution = main_solution,
      data = boundary_data
    )$boundary[[1]]
  } else {
    total_perimeter <- NA_real_
  }

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
