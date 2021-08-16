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
#' @param boundary_data [Matrix::sparseMatrix()] containing the boundary data.
#'
#' @param theme_data [Matrix::sparseMatrix()] containing the theme data.
#'
#' @param weight_data [Matrix::sparseMatrix()] containing the weight data.
#'
#' @param include_data [Matrix::sparseMatrix()] containing the include data.
#'
#' @param theme_settings `data.frame` containing the theme settings.
#'
#' @param weight_settings `data.frame` containing the weight settings.
#'
#' @param include_settings  `data.frame` containing the include settings.
#'
#' @param parameters  `list` of [Parameter] objects.
#'
#' @param gap `numeric` relative optimality gap value. Defaults to 0.
#'
#' @param boundary_gap `numeric` value used to control
#'   the level of spatial clustering in the solution. Defaults to 0.1.
#'
#' @param cache [cachem::cache_mem()] object used to cache intermediate
#'  calculations.
#'  Defaults to an empty cache such that (effectively) no cache is used.
#'
#' @param time_limit `numeric` time limit (seconds) for generating solutions.
#'  Defaults to the maximum integer.
#'
#' @param verbose `logical` value indicating if information should be
#'  displayed when generating solutions. Defaults to `FALSE`.
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
#'
#' # create a weight using a variable
#' w <- new_weight(
#'   name = "Human Footprint Index", variable = v1,
#'   factor = 90, status = FALSE, id = "W1"
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
#' # create parameters
#' p1 <- new_parameter(name = "Spatial clustering")
#' p2 <- new_parameter(name = "Optimality gap")
#'
#' # create solution settings using the themes and weight
#' ss <- new_solution_settings(
#'   themes = list(t1, t2), weights = list(w), includes = list(i),
#'   parameters = list(p1, p2)
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
#'  theme_settings = ss$get_theme_settings(),
#'  weight_settings = ss$get_weight_settings(),
#'  include_settings = ss$get_include_settings(),
#'  parameters = ss$parameters,
#'  gap = p2$value * p2$status,
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
                           theme_settings,
                           weight_settings,
                           include_settings,
                           parameters,
                           gap = 0,
                           boundary_gap = 0.1,
                           cache = cachem::cache_mem(),
                           time_limit = .Machine$integer.max,
                           verbose = FALSE,
                           id = uuid::UUIDgenerate()) {
  # validate arguments
  assertthat::assert_that(
    ## id
    assertthat::is.string(id),
    assertthat::noNA(id),
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
    ## parameters
    is.list(parameters),
    all_list_elements_inherit(parameters, "Parameter"),
    ## gap
    assertthat::is.number(gap),
    assertthat::noNA(gap),
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

  # calculate cost values
  if (nrow(weight_data) > 0) {
    ## if weights present, then use data and settings
    ### normalize cost values
    wn <- weight_data
    for (i in seq_along(nrow(wn))) {
      wn[i, ] <- zscale(wn[i, ])
      if (abs(min(weight_data[i, ], na.rm = TRUE)) < 1e-10) {
        wn[i, ] <- scales::rescale(wn[i, ], to = c(0, 1))
      } else {
        wn[i, ] <- scales::rescale(wn[i, ], to = c(0.01, 1))
      }
    }
    ### apply factors and status settings
    cost <- matrix(
      rep(weight_settings$factor * weight_settings$status, each = ncol(wn)),
      byrow = TRUE,
      nrow = nrow(wn), ncol = ncol(wn)
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
  features <-
    data.frame(id = seq_len(nrow(theme_settings)), name = theme_settings$id)

  # generate cache key based on settings
  key <- digest::digest(
    list(
      themes = theme_settings,
      weights = weight_settings,
      includes = include_settings
    )
  )

  ## generate initial prioritization problem
  ### this problem just aims to minimize cost
  initial_problem <-
    suppressWarnings(prioritizr::problem(cost, features, theme_data)) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_manual_targets(targets) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_cbc_solver(
      gap = gap, verbose = verbose, time_limit = time_limit
    )
  ### add locked in constraints if needed
  if (any(locked_in)) {
    initial_problem <-
      initial_problem %>%
      prioritizr::add_locked_in_constraints(locked_in)
  }

  ## generate solution
  if (!isTRUE(cache$exists(key))) {
    ### solve problem to generate solution if needed
    initial_solution <- c(
      prioritizr::solve(initial_problem, run_checks = FALSE)
    )
    ### store solution in cache
    cache$set(key, initial_solution)
  }
  ## extract solution from cache
  initial_solution <- cache$get(key)

  # generate second prioritization
  ## this formulation aims to minimize fragmentation,
  ## whilst ensuring that total cost does do not exceed a threshold
  if (boundary_gap >= 1e-5) {
    ### calculate cost constraint for new prioritization
    max_cost <- sum(initial_solution * cost) * (boundary_gap + 1)
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
      prioritizr::add_boundary_penalties(
        penalty = 1, data = boundary_data
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
      prioritizr::add_cbc_solver(
        gap = gap, verbose = verbose, time_limit = time_limit
      )
    ### add locked in constraints if needed
    if (any(locked_in)) {
      main_problem <-
        main_problem %>%
        prioritizr::add_locked_in_constraints(locked_in)
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
    theme_settings = theme_settings,
    weight_settings = weight_settings,
    include_settings = include_settings,
    parameters = parameters
  )
}
