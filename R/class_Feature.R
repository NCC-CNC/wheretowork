#' @include internal.R class_Variable.R
NULL

#' Feature class
#'
#' Definition for the Feature class.
#'
#' @seealso [new_feature()], [new_variable()].
Feature <- R6::R6Class(
  "Feature",
  public = list(
    #' @field id `character` unique identifier.
    id = NA_character_,

    #' @field name `character` name.
    name = NA_character_,

    #' @field variable [Variable] object.
    variable = NULL,

    #' @field visible `logical` value.
    visible = NA,

    #' @field status `logical` value.
    status = NA,

    #' @field current `numeric` value.
    current = NA_real_,

    #' @field  goal `numeric` value.
    goal = NA_real_,

    #' @field min_goal `numeric` minimum goal value.
    min_goal = NA_real_,

    #' @field max_goal `numeric` maximum goal value.
    max_goal = NA_real_,

    #' @field step_goal `numeric` step goal value.
    step_goal = NA_real_,

    #' @field limit_goal `numeric` limit goal value.
    limit_goal = NA_real_,

    #' @description
    #' Create a Feature object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] .
    #' @param visible `logical` value.
    #' @param status `logical` value.
    #' @param min_goal `numeric` value.
    #' @param max_goal `numeric` value.
    #' @param goal `numeric` value.
    #' @param limit_goal `numeric` value.
    #' @param step_goal `numeric` value.
    #' @param current `numeric` value.
    #' @return A new Feature object.
    initialize = function(
      id, name, variable, visible, status,
      goal, min_goal, max_goal, step_goal, limit_goal,
      current) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        #### variable
        inherits(variable, "Variable"),
        #### visible
        assertthat::is.flag(visible),
        assertthat::noNA(visible),
        #### status
        assertthat::is.flag(status),
        assertthat::noNA(status),
        #### goal
        assertthat::is.number(goal),
        assertthat::noNA(goal),
        goal >= min_goal,
        goal <= max_goal,
        goal >= limit_goal,
        #### min_goal
        assertthat::is.number(min_goal),
        assertthat::noNA(min_goal),
        min_goal <= max_goal,
        #### max_goal
        assertthat::is.number(max_goal),
        assertthat::noNA(max_goal),
        max_goal >= min_goal,
        #### step_goal
        assertthat::is.number(step_goal),
        assertthat::noNA(step_goal),
        step_goal <= max_goal,
        #### current
        assertthat::is.number(current),
        assertthat::noNA(current),
        isTRUE(current >= 0),
        isTRUE(current <= 1))
      ### set fields
      self$id <- id
      self$name <- name
      self$variable <- variable
      self$visible <- visible
      self$status <- status
      self$goal <- goal
      self$min_goal <- min_goal
      self$max_goal <- max_goal
      self$step_goal <- step_goal
      self$limit_goal <- limit_goal
      self$current <- current
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Feature")
      message("  id:      ", self$id)
      message("  name:    ", self$name)
      message("  variable:   ", self$variable$repr())
      message("  visible:  ", self$visible)
      message("  status:  ", self$status)
      message("  current: ", round(self$current, 2))
      message("  goal:    ", round(self$goal, 2))
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param start `character` symbol used to start the setting list.
    #'   Defaults to `"["`.
    #' @param end `character` symbol used to start the setting list.
    #'   Defaults to `"]"`.
    #' @return `character` value.
    repr = function(start = "[", end = "]") {
      paste0(
        self$name,
        " ", start, "status: ", self$status,
        ", current: ", round(self$current, 2),
        ", goal: ", round(self$goal, 2), end, nl(),
        "  variable: ", self$variable$repr())
    },

    #' @description
    #' Get visible.
    #' @return `numeric` value.
    get_visible = function() {
      self$visible
    },

    #' @description
    #' Get current.
    #' @return `numeric` value.
    get_current = function() {
      self$current
    },

    #' @description
    #' Get status.
    #' @return `logical` value.
    get_status = function() {
      self$status
    },

    #' @description
    #' Get goal.
    #' @return `numeric` value.
    get_goal = function() {
      self$goal
    },

    #' @description
    #' Get the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_data = function() {
      self$variable$get_data()
    },

    #' @description
    #' Set visible.
    #' @param value `logical` new value.
    set_visible = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value))
      self$visible = value
      invisible(self)
    },

    #' @description
    #' Set status.
    #' @param value `logical` new value.
    set_status = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value))
      self$status = value
      invisible(self)
    },

    #' @description
    #' Set goal.
    #' @param value `numeric` new value.
    set_goal = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value),
        value >= self$min_goal,
        value <= self$max_goal,
        value >= self$limit_goal)
      self$goal <- value
      invisible(self)
    },

    #' @description
    #' Set current.
    #' @param value `numeric` new value.
    set_current = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value))
      self$current <- value
      invisible(self)
    },

    #' @description
    #' Export settings
    #' @return `list` object.
    export = function() {
      list(
        name = self$name,
        variable = self$variable$export(),
        status = self$status,
        visible = self$visible,
        goal = self$goal,
        min_goal = self$min_goal,
        max_goal = self$max_goal,
        step_goal = self$step_goal,
        limit_goal = self$limit_goal
      )
    }

  )
)

#' New feature
#'
#' Create a new [Feature] object.
#'
#' @param name `character` Name of the feature.
#'
#' @param variable [Variable] object.
#'
#' @param visible `logical` The initial visible value.
#'   This is used to determine if the feature is displayed (or not)
#'   or not the map.
#'   Defaults to `TRUE`
#'
#' @param status `logical` The initial status value.
#'   This is used to display information on whether the feature is
#'   selected (or not) for subsequent analysis.
#'   Defaults to `TRUE`
#'
#' @param goal `numeric` The initial goal for the feature.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 0.3 (i.e. 30%).
#'
#' @param min_goal `numeric` The minimum goal (inclusive)
#'   shown for the feature.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 0 (i.e. 0%).
#'
#' @param max_goal `numeric` The maximum goal (inclusive) shown
#'   for the feature.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 1 (i.e. 100%).
#'
#' @param limit_goal `numeric` The minimum goal
#'   (inclusive) that can be selected for the feature.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 0 (i.e. 0%).
#'
#' @param step_goal `numeric` The minimum increment for setting goals.
#'   Note that goal are specified as proportions, such that a
#'   value of 0.01 corresponds to 1%.
#'   Defaults to 0.01 (i.e. 1%).
#'
#' @param current `numeric` current proportion of values held in existing
#'   conservation areas (e.g. 0.1 = 10%).
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [Feature] object.
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'  "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'  package = "wheretowork"
#' )
#' f3 <- system.file(
#'  "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'  package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # create new variable
#' v <- new_variable_from_auto(dataset = d, index = 1)
#'
#' # create feature using the variable
#' f <- new_feature(name = "Intact Alvar", variable = v)
#'
#' # print object
#' print(f)
#'
#' @export
new_feature <- function(
    name,
    variable,
    visible = TRUE,
    status = TRUE,
    goal = 0.1,
    min_goal = 0,
    max_goal = 1.0,
    step_goal = 0.01,
    limit_goal = 0.1,
    current = 0,
    id = uuid::UUIDgenerate()) {
  # return new feature
  Feature$new(
    id = id,
    name = name,
    variable = variable,
    visible = visible,
    status = status,
    goal = goal,
    min_goal = min_goal,
    max_goal = max_goal,
    step_goal = step_goal,
    limit_goal = limit_goal,
    current = current)
}
