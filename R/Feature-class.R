#' @include internal.R Layer-class.R
NULL

#' Feature class
#'
#' Definition for the Feature class.
#'
#' @seealso [new_feature()], [new_layer()].
Feature <- R6Class(
  "Feature",
  public = list(
    #' @field id `character` unique identifier.
    id = NA_character_,

    #' @field name `character` name.
    name = NA_character_,

    #' @field layer [Layer] object.
    layer = NULL,

    #' @field status `logical` value.
    status = NA,

    #' @field initial_status `logical` value.
    initial_status = NA,

    #' @field  goal `numeric` value.
    goal = NA_real_,

    #' @field initial_goal `numeric` initial goal value.
    initial_goal = NA_real_,

    #' @field min_goal `numeric` minimum goal value.
    min_goal = NA_real_,

    #' @field max_goal `numeric` maximum goal value.
    max_goal = NA_real_,

    #' @field step_goal `numeric` step goal value.
    step_goal = NA_real_,

    #' @field limit_goal `numeric` limit goal value.
    limit_goal = NA_real_,

    #' @field  current_label `character` current label.
    current_label = NA_character_,

    #' @field icon `shiny.tag` object.
    icon = NULL,

    #' @description
    #' Create a Feature object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param layer [Layer] .
    #' @param initial_status `logical` value.
    #' @param min_goal `numeric` value.
    #' @param max_goal `numeric` value.
    #' @param initial_goal `numeric` value.
    #' @param limit_goal `numeric` value.
    #' @param step_goal `numeric` value.
    #' @param current_label `character` value.
    #' @param icon `shiny.tag` object.
    #' @return A new Feature object.
    initialize = function(
      id, name, layer, initial_status,
      initial_goal, min_goal, max_goal, step_goal, limit_goal,
      current_label, icon) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::is.noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::is.noNA(name),
        #### layer
        inherits(layer, "Layer"),
        #### initial_status
        assertthat::is.flag(initial_status),
        assertthat::is.noNA(initial_status),
        #### initial_goal
        assertthat::is.number(initial_goal),
        assertthat::is.noNA(initial_goal),
        initial_goal >= min_goal,
        initial_goal <= max_goal,
        initial_goal >= limit_goal,
        #### min_goal
        assertthat::is.number(min_goal),
        assertthat::is.noNA(min_goal),
        min_goal <= max_goal,
        #### max_goal
        assertthat::is.number(max_goal),
        assertthat::is.noNA(max_goal),
        max_goal >= min_goal,
        #### step_goal
        assertthat::is.number(step_goal),
        assertthat::is.noNA(step_goal),
        step_goal <= max_goal,
        #### current_label
        assertthat::is.string(current_label),
        assertthat::is.noNA(current_label),
        #### icon
        inherits(icon, "shiny.tag"))
      ### set fields
      self$id <- id
      self$name <- name
      self$layer <- layer
      self$status <- initial_status
      self$initial_status <- initial_status
      self$goal <- initial_goal
      self$initial_goal <- initial_goal
      self$min_goal <- min_goal
      self$max_goal <- max_goal
      self$step_goal <- step_goal
      self$current_label <- current_label
      self$icon <- icon
    },

    #' @description
    #' Get goal.
    #' @return `numeric` value.
    get_goal = function() {
      self$goal
    },

    #' @description
    #' Get status.
    #' @return `logical` value.
    get_status = function() {
      self$status
    },

    #' @description
    #' Set goal.
    #' @param value `numeric` new value.
    set_goal = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::is.noNA(value))
      self$goal <- value
      invisible(self)
    },

    #' @description
    #' Set status.
    #' @param value `logical` new value.
    set_status = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::is.noNA(value))
      self$status = value
      invisible(self)
    }
  )
)

#' New feature
#'
#' Create a new [Feature] object.
#'
#' @param name `character` Name of the theme.
#'
#' @param layer [Layer] object.
#'
#' @param initial_status `logical` The initial status.
#'   This is used to display information on whether the theme is
#'   selected (or not) for subsequent analysis.
#'   Defaults to `TRUE`
#'
#' @param initial_goal `numeric` The initial goal for the feature.
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
#' @param current_label `character` The display label for the current
#'  level of for the feature.
#'  Defaults to `"Current"`.
#'
#' @param icon `character` or `shiny.tag` Icon to display for the feature
#'  This icon should indicate the type of data that underpin the feature.
#'  If the argument to `icon` is a `character`, it is used with
#'  [shiny::icon()] to generate an `shiny.tag` icon.
#'  Defaults to `"map-marked-alt"`.

#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [Feature] object.
#'
#' @examples
#' # create new layer
#' l <- new_layer(source = tempfile(), current = 0.1, total = 12, units = "ha")
#'
#' # create feature using the layer
#' f <- new_feature(name = "Intact Alvar", layer = l)
#'
#' # print object
#' print(f)
#'
#' @export
new_feature <- function(
    name,
    layer,
    initial_status = TRUE,
    initial_goal = 0.1,
    min_goal = 0,
    max_goal = 1.0,
    step_goal = 0.01,
    limit_goal = 0.1,
    current_label = "Current",
    icon = "map-marked-alt",
    id = uuid::UUIDgenerate()) {
  # convert icon to shiny.tag if needed
  if (is.character(icon))
    icon <- shiny::icon(icon)
  # return new feature
  Feature$new(
    id = id,
    name = name,
    layer = layer,
    initial_status = initial_status,
    initial_goal = initial_goal,
    min_goal = min_goal,
    max_goal = max_goal,
    step_goal = step_goal,
    limit_goal = limit_goal,
    current_label = current_label,
    icon = icon)
}
