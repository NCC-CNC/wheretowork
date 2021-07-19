#' @include internal.R
NULL

#' Generate minimum set solution
#'
#' Create a new [Solution] object by generating a prioritization
#' using the minimum set formulation of the reserve selection problem.
#'
#' @param name `character` name for new solution.
#'
#' @param dataset [Dataset] object to store the solution.
#'
#' @param settings [SolutionSettings] object with settings.
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
#' @param boundary_gap `numeric` gap value used to control
#'   the level of spatial clustering in the solution. Defaults to 0.1
#'
#' @param legend_color `character` legend color.
#'
#' @return A [Solution] object containing the solution.
#'
#' @examples
#' #TODO.
#'
#' @export
generate_min_set_solution <- function(
  name, dataset, settings,
  theme_data = settings$get_theme_data(),
  weight_data = settings$get_weight_data(),
  include_data = settings$get_include_data(),
  boundary_data = dataset$get_boundary_data(),
  gap = 0,
  boundary_gap = 0.1,
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
    ## theme_data
    inherits(theme_data, "dgCMatrix"),
    ## weight_data
    inherits(weight_data, "dgCMatrix"),
    ## include_data
    inherits(include_data, "dgCMatrix"),
    ## gap
    assertthat::is.number(gap),
    assertthat::noNA(gap),
    ## boundary_gap
    assertthat::is.number(boundary_gap),
    assertthat::noNA(boundary_gap),
    ## legend_color
    assertthat::is.string(legend_color),
    assertthat::noNA(legend_color))

  # prepare settings
  ## extract values
  theme_settings <- settings$get_theme_settings()
  weight_settings <- settings$get_weight_settings()
  include_settings <- settings$get_include_settings()
  ## ensure data match settings
  assertthat::assert_that(
    identical(theme_settings$id, rownames(theme_data)),
    identical(weight_settings$id, rownames(weight_data)),
    identical(include_settings$id, rownames(include_data)))

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
        theme_settings$total * theme_settings$limit)
    )
  ## round values down to account for floating point issues
  targets$target <- floor(targets$target * 1e+3) / 1e+3

  # calculate locked in values
  locked_in <- matrix(
    include_settings$status, byrow = TRUE,
    nrow = nrow(include_data), ncol = ncol(include_data)
  )
  locked_in <- as.logical(colSums(locked_in * include_data) > 0)

  # calculate cost values
  ## normalize cost values
  wn <- weight_data
  for (i in seq_along(nrow(wn))) {
    wn[i, ] <- zscale(wn[i, ])
  }
  wn@x <- wn@x + abs(min(wn@x)) + 1
  ## apply factors and status settings
  cost <- matrix(
    weight_settings$factor * weight_settings$status, byrow = TRUE,
    nrow = nrow(wn), ncol = ncol(wn)
  )
  ## calculate total cost by summing together all weight values
  cost <- colSums(cost * wn)
  ## re-scale cost values to avoid numerical issues
  cost <- scales::rescale(cost, to = c(0.01, 1000))

  # calculate feature data
  features <-
    data.frame(id = seq_len(nrow(theme_settings)), name = theme_settings$id)

  # generate initial prioritization
  ## this simply just aims to minimize cost
  initial_problem <-
    suppressWarnings(prioritizr::problem(cost, features, theme_data)) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_manual_targets(targets) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_cbc_solver(gap = gap, verbose = FALSE)
  ### add locked in constraints if needed
  if (any(locked_in)) {
    initial_problem <-
      initial_problem %>%
      prioritizr::add_locked_in_constraints(locked_in)
  }
  ### generate solution
  initial_solution <-
    c(prioritizr::solve(initial_problem, run_checks = FALSE))

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
        penalty = 1, data = boundary_data) %>%
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
      prioritizr::add_cbc_solver(gap = gap, verbose = FALSE)
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

  # calculate results
  ## summary statistics
  ### preliminary calculations
  pu_areas <- dataset$get_planning_unit_areas()
  area_m <- sum(main_solution * pu_areas)
  perimeter_m <-
    prioritizr::eval_boundary_summary(
      x = main_problem,
      solution = main_solution,
      data = boundary_data)$boundary[[1]]
  reserve_sizes_m <-
    reserve_sizes(
      x = main_solution,  areas = pu_areas, boundary_matrix = boundary_data)
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
    main_solution, byrow = TRUE,
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
    main_solution, byrow = TRUE,
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
      labels = c("not selected", "selected")))

  # return solution object
  new_solution(
    name = name,
    variable = v,
    initial_visible = TRUE,
    statistics = statistics_results,
    theme_results = theme_results,
    weight_results = weight_results,
    id = uuid::UUIDgenerate())
}
