#' @include internal.R
NULL

#' Parameter class
#'
#' Definition for the Parameter class.
#'
#' @seealso [new_parameter()].
#'
#' @export
Parameter <- R6::R6Class(
  "Parameter",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field status `logical` value.
    status = NA,

    #' @field initial_status `logical` value.
    initial_status = NA,

    #' @field value `numeric` value.
    value = NA_real_,

    #' @field initial_value `numeric` initial value value.
    initial_value = NA_real_,

    #' @field min_value `numeric` minimum value.
    min_value = NA_real_,

    #' @field max_value `numeric` maximum value.
    max_value = NA_real_,

    #' @field step_value `numeric` step value.
    step_value = NA_real_,

    #' @description
    #' Create a new Parameter object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param initial_status `logical` value.
    #' @param initial_value `numeric` initial value.
    #' @param min_value `numeric` minimum value.
    #' @param max_value `numeric` maximum value.
    #' @param step_value `numeric` step value.
    #' @return A new Parameter object.
    ## constructor
    initialize = function(
      id, name, initial_status,
      initial_value, min_value, max_value, step_value) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        #### initial_status
        assertthat::is.flag(initial_status),
        assertthat::noNA(initial_status),
        #### initial_value
        assertthat::is.number(initial_value),
        assertthat::noNA(initial_value),
        initial_value >= min_value,
        initial_value <= max_value,
        #### min_value
        assertthat::is.number(min_value),
        assertthat::noNA(min_value),
        min_value <= max_value,
        #### max_value
        assertthat::is.number(max_value),
        assertthat::noNA(max_value),
        max_value >= min_value,
        #### step_value
        assertthat::is.number(step_value),
        assertthat::noNA(step_value),
        step_value <= max_value
      )
      ### set fields
      self$id <- id
      self$name <- name
      self$status <- initial_status
      self$initial_status <- initial_status
      self$value <- initial_value
      self$initial_value <- initial_value
      self$min_value <- min_value
      self$max_value <- max_value
      self$step_value <- step_value
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Parameter")
      message("  id:       ", self$id)
      message("  name:     ", self$name)
      message("  status:   ", self$status)
      message("  value:   ", round(self$value, 2))
      invisible(self)
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
        " ", start, "status: ", self$status,
        ", value: ", round(self$value, 2), end)
    },

    #' @description
    #' Get status.
    #' @return `logical` value.
    get_status = function() {
      self$status
    },

    #' @description
    #' Get value.
    #' @return `numeric` value.
    get_value = function() {
      self$value
    },

    #' @description
    #' Get parameter.
    #' @param name `character` parameter name.
    #' Available options are `"status"`, or `"value"`.
    #' @return Value.
    get_parameter = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "value"))
      if (identical(name, "status")) {
        out <- self$get_status()
      } else if (identical(name, "value")) {
        out <- self$get_value()
      } else {
        stop(paste0("\"", name, "\" is not a parameter"))
      }
      out
    },

    #' @description
    #' Set status.
    #' @param value `logical` new value.
    set_status = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value))
      self$status <- value
      invisible(self)
    },

    #' @description
    #' Set value.
    #' @param value `numeric` new value.
    set_value = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value),
        value >= self$min_value,
        value <= self$max_value)
      self$value <- value
      invisible(self)
    },

    #' @description
    #' Set parameter.
    #' @param name `character` parameter name.
    #' Available options are `"status"`, or `"value"`.
    #' @param value `ANY` new value.
    set_parameter = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "value"))
      if (identical(name, "status")) {
        self$set_status(value)
      } else if (identical(name, "value")) {
        self$set_value(value)
      } else {
        stop(paste0("\"", name, "\" is not a parameter"))
      }
      invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_solution_settings_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        min_value = self$min_value,
        max_value = self$max_value,
        value = self$value,
        step_value = self$step_value,
        status = self$status
      )
    },

    #' @description
    #' Export parameters.
    #' Export parameters.
    #' @return `list` object.
    export = function() {
      list(
        name = self$name,
        initial_status = self$status,
        initial_value = self$value,
        min_value = self$min_value,
        max_value = self$max_value,
        step_value = self$step_value
      )
    }

  )
)

#' New parameter
#'
#' Create a new [Parameter] object.
#'
#' @param min_value `numeric` minimum value.
#'   Defaults to 0.
#'
#' @param max_value `numeric` maximum value.
#    Defaults to 100
#'
#' @param initial_value `numeric` initial value.
#'   Defaults to 0.
#'
#' @param step_value `numeric` step value.
#'   Defaults to 1.
#'
#' @inheritParams new_multi_theme
#' @inheritParams new_feature
#'
#' @return A [Parameter] object.
#'
#' @examples
#' # create a new parameter
#' p <- new_parameter(name = "Spatial clumping")
#'
#' # print object
#' print(p)
#'
#' @export
new_parameter <- function(
  name, initial_status = TRUE,
  initial_value = 0, min_value = 0, max_value = 100, step_value = 1,
  id = uuid::UUIDgenerate()) {
  Parameter$new(
    id = id,
    name = name,
    initial_status = initial_status,
    min_value = min_value,
    max_value = max_value,
    initial_value = initial_value,
    step_value = step_value)
}
