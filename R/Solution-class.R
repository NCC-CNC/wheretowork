#' @include internal.R
NULL


#' Solution class
#'
#' Definition for the Solution class.
#'
#' @seealso [new_solution()].
Solution <- R6::R6Class(
  "Solution",
  public = list(

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field name `character` name of solution.
    name = NA_character_,

    #' @field initial_visible `logical` value.
    initial_visible = NA,

    #' @field variable [Variable] object.
    variable = NULL,

    #' @field visible `logical` value.
    visible = NA,

    #' @field statistics `list` of [Statistic] objects
    statistics = NULL,

    #' @field theme_results `list` of [ThemeResults] objects.
    theme_results = NULL,

    #' @field weight_results `list` of [WeightResults] objects.
    weight_results = NULL,

    #' @description
    #' Create a Solution object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param initial_visible `logical` value.
    #' @param statistics `list` of [Statistic] objects.
    #' @param theme_results `list` of [ThemeResults] objects.
    #' @param weight_results `list` of [WeightResults] objects.
    #' @return A new Solution object.
    initialize = function(
      id, name, variable, initial_visible, statistics, theme_results,
      weight_results) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.string(id),
        assertthat::noNA(id),
        assertthat::is.string(name),
        assertthat::noNA(name),
        inherits(variable, "Variable"),
        assertthat::is.flag(initial_visible),
        assertthat::noNA(initial_visible),
        is.list(statistics),
        all_list_elements_inherit(statistics, "Statistic"),
        is.list(theme_results),
        all_list_elements_inherit(theme_results, "ThemeResults"),
        is.list(weight_results),
        all_list_elements_inherit(weight_results, "WeightResults"))
      # assign fields
      self$id <- id
      self$name <- name
      self$variable <- variable
      self$visible <- initial_visible
      self$initial_visible <- initial_visible
      self$statistics  <- statistics
      self$theme_results <- theme_results
      self$weight_results <- weight_results
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param start `character` symbol used to start the parameter list.
    #'   Defaults to `"["`.
    #' @param end `character` symbol used to start the parameter list.
    #'   Defaults to `"]"`.
    #' @return `character` value.
    repr = function(start = "[", end = "]") {
      paste0(
        self$name,
        " ",
        start,
        paste(
          vapply(self$statistics, function(x) x$repr(), character(1)),
          collapse = ", "),
        end,
        nl(),
        "  variable: ", self$variable$repr())
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Solution")
      message("  id:      ", self$id)
      message("  name:    ", self$name)
      invisible(self)
    },

    #' @description
    #' Get visible.
    #' @return `logical` value.
    get_visible = function() {
      self$visible
    },

    #' @description
    #' Get parameter.
    #' @param name `character` parameter name.
    #' Available options are `"visible"`.
    #' @return Value.
    get_parameter = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("visible"))
      if (identical(name, "visible")) {
        out <- self$get_visible()
      } else {
        stop(paste0("\"", name, "\" is not a parameter"))
      }
      out
    },

    #' @description
    #' Set visible.
    #' @param value `logical` new value.
    set_visible = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value))
      self$visible <- value
      invisible(self)
    },

    #' @description
    #' Set parameter.
    #' @param name `character` parameter name.
    #' Available options are `"visible"``.
    #' @param value `ANY` new value.
    set_parameter = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("visible"))
      if (identical(name, "visible")) {
        self$set_visible(value)
      } else {
        stop(paste0("\"", name, "\" is not a parameter"))
      }
      invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_solution_results_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        statistics = lapply(
          self$statistics,
          function(x) x$get_widget_data()),
        theme_results = lapply(
          self$theme_results,
          function(x) x$get_widget_data()),
        weight_results = lapply(
          self$weight_results,
          function(x) x$get_widget_data()))
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        visible = self$visible,
        legend = self$variable$legend$get_widget_data(),
        units = self$variable$units,
        type = "solution"
      )
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leaflet()] object.
    render_on_map = function(x, zindex) {
      self$variable$render(x, self$id, zindex, self$visible)
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leafletProxy()] object.
    update_on_map = function(x, zindex) {
      self$variable$update_render(x, self$id, zindex, self$visible)
    }

  )
)

#' New solution
#'
#' Create a new [Solution] object.
#'
#' @param name `character` name for new solution.
#'
#' @param variable [Variable] object with the solution.
#'
#' @param initial_visible `logical` should the solution be visible on a map?
#'
#' @param statistics `list` of [Statistic] objects.
#'
#' @param theme_results `list` of [ThemeResults] objects.
#'
#' @param weight_results `list` of [WeightResults] objects.
#'
#' @inheritParams new_multi_theme
#'
#' @examples
#' #TODO
#'
#' @export
new_solution <- function(
  name, variable, initial_visible, statistics,
  theme_results, weight_results, id = uuid::UUIDgenerate()) {
  Solution$new(
    name = name,
    variable = variable,
    initial_visible = initial_visible,
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results,
    id = id)
}

#' New solution from analysis
#'
#' Create a new [Solution] object by generating a prioritization and
#' calculating performance statistics.
#'
#' @param name `character` name for new solution.
#'
#' @param dataset [Dataset] object to store the solution.
#'
#' @param solution_settings [SolutionSettings] object.
#'
#' @param rij `matrix` object.
#'  This matrix specifies the amount of each feature associated with
#'  each planning unit.
#'
#' @param wij `matrix` object.
#'  This matrix specifies the value of each weight associated with each
#'  planning unit.
#'
#' @param boundary_data [Matrix::sparseMatrix()] object.
#'  This matrix specifies the boundary length data for the planning units.
#'
#' @param gap `numeric` relative optimality gap value. Defaults to 0.
#'
#' @param boundary_penalty_gap `numeric` gap value used to control
#'   the level of spatial fragmentation in the solution. Defaults to 0.1
#'
#' @return A `SolutionResults` object containing the solution.
#'
#' @examples
#' #TODO.
#'
#' @export
new_solution_from_prioritization <- function(
  name, dataset, solution_settings,
  rij, wij, boundary_data,
  gap = 0, boundary_penalty_gap = 0.1) {
  # validate arguments
  assertthat::assert_that(
    ## name
    assertthat::is.string(name),
    assertthat::noNA(name),
    ## dataset
    inherits(dataset, "Dataset"),
    ## solution_settings
    inherits(solution_settings, "SolutionSettings"),
    ## rij
    is.matrix(rij),
    ## wij
    is.matrix(wij),
    ## gap
    assertthat::is.number(gap),
    assertthat::noNA(gap),
    ## boundary_penalty_gap
    assertthat::is.number(boundary_penalty_gap),
    assertthat::noNA(boundary_penalty_gap))

  # extract data
  ## create table with feature settings
  feature_data <- solution_settings$get_theme_settings()
  ## create table with weight settings
  weight_data <- solution_settings$get_weight_settings()
  ## validate data
  assertthat::assert_that(
    setequal(weight_data$id, rownames(wij)),
    setequal(feature_data$id, rownames(rij)))
  ## ensure correct ordering
  feature_data <-
    feature_data[order(feature_data$id, rownames(rij)), , drop = FALSE]
  weight_data <-
    weight_data[order(weight_data$id, rownames(wij)), , drop = FALSE]

  # prepare for prioritization
  ## compute cost variable using weight data
  cost <- colSums(matrix(
      weight_data$factor * weight_data$status,
      nrow = nrow(wij), ncol = ncol(wij)) * wij)

  ## calculate absolute targets
  ## (and round them down to account for floating point issues)
  abs_targets <- feature_data$total * feature_data$goal
  abs_targets <- floor(abs_targets * 1e+3) / 1e+3
  abs_targets <- abs_targets * feature_data$status

  # generate initial prioritization
  ## this simply just aims to minimize cost
  initial_problem <-
    prioritizr::problem(
      x = cost,
      features = tibble::tibble(
        id = seq_along(abs_targets),
        name = feature_data$id),
      rij_matrix = rij) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_absolute_targets(abs_targets) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_default_solver(
      gap = gap, verbose = FALSE)
  initial_solution <-
    c(prioritizr::solve(initial_problem, run_checks = FALSE))

  # generate second prioritization
  ## this formulation aims to minimize fragmentation,
  ## whilst ensuring that total cost does do not exceed a threshold
  if (boundary_penalty_gap >= 1e-5) {
    ### calculate total cost of initial prioritization
    max_cost <- sum(initial_solution * cost) * (boundary_penalty_gap + 1)
    ### generate prioritization
    main_problem <-
      prioritizr::problem(
        x = rep(0, length(cost)),
        features = tibble::tibble(
          id = seq_along(c(abs_targets, 1)),
          name = c(feature_data$id, "cost")),
          rbind(
            feature_data[, c("id", "name")],
            tibble::tibble(id = "cost", name = "cost")),
        rij_matrix = rbind(rij, cost)) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_boundary_penalties(
        penalty = 1, data = boundary_data) %>%
      prioritizr::add_absolute_targets(
        c(abs_targets, max_cost)) %>%
      prioritizr::add_binary_decisions() %>%
      prioritizr::add_default_solver(
        gap = gap, verbose = FALSE)
    main_solution <-
      c(prioritizr::solve(main_problem, run_checks = FALSE))
  } else {
    ### if the boundary_penalty_gap parameter is very low,
    ### then we will just use the initial solution because the
    ### second prioritization is unlikely to be very different from the first
    main_solution <- initial_solution
    main_problem <- initial_problem
  }

  # calculate results
  ## summary statistics
  ### preliminary calculations
  pu_areas <- dataset$get_planning_unit_areas()

  ### calculate statistics
  statistics_results <-
    tibble::tibble(
      absolute_area = sum(main_solution * pu_areas),
      relative_area = sum(main_solution * pu_areas) / sum(pu_areas),
      absolute_perimeter =
      prioritizr::eval_boundary_summary(
        x = main_problem, solution = main_solution,
        data =  boundary_data)$boundary[[1]],
      perimeter_area_ratio = absolute_perimeter / absolute_area)

  ## theme representation
  ### preliminary calculations
  rij_absolute_held <-
    rowSums(matrix(main_solution, ncol = ncol(rij), nrow = nrow(rij)) * rij)
  names(rij_absolute_held) <- rownames(rij)
  ### calculate results for each theme separately
  theme_results <- lapply(seq_along(solution_settings$themes), function(i) {
    out <- solution_settings$themes[[i]]$get_solution_settings_widget_data()
    out$feature_held <-
      unname(rij_absolute_held[out$feature_id]) / out$feature_total
    out
  })

  ## weight coverage
  ### preliminary calculations
  wij_totals <- rowSums(wij)
  wij_absolute_held <-
    rowSums(matrix(main_solution, ncol = ncol(wij), nrow = nrow(wij) * wij))
  ### calculate results for each weight separately
  weight_results <- lapply(seq_along(solution_settings$weights), function(i) {
    out <- solution_settings$weights[[i]]$get_solution_settings_widget_data()
    out$total <- wij_totals[i]
    out$held <- wij_absolute_held[i] / wij_totals[i]
    out
  })

  # create variable to store solution
  idx <- dataset$max_index() + 1
  dataset$add_index(index = idx, values = main_solution)
  v <- new_variable(
    dataset = dataset, index = idx,
    total = sum(main_solution), units = "",
    legend = new_categorical_legend(
      values = c(0, 1),
      colors = sample(color_palette("Set1", NULL), 1)))

  # return solution object
  new_solution(
    name = name,
    variable = v,
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results,
    id = uuid::UUIDgenerate())
}
