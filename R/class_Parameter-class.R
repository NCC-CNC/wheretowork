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

    #' @field value `numeric` initial value value.
    value = NA_real_,

    #' @field min_value `numeric` minimum value.
    min_value = NA_real_,

    #' @field max_value `numeric` maximum value.
    max_value = NA_real_,

    #' @field step_value `numeric` step value.
    step_value = NA_real_,

    #' @field hide `logical` value.
    hide = NA,

    #' @field units `character` value.
    units = NA_character_,

    #' @description
    #' Create a new Parameter object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param status `logical` value.
    #' @param value `numeric` initial value.
    #' @param min_value `numeric` minimum value.
    #' @param max_value `numeric` maximum value.
    #' @param step_value `numeric` step value.
    #' @param hide `logical` value.
    #' @param units `character` value.
    #' @return A new Parameter object.
    ## constructor
    initialize = function(id, name, status,
                          value, min_value, max_value, step_value,
                          hide, units) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        #### status
        assertthat::is.flag(status),
        assertthat::noNA(status),
        #### value
        assertthat::is.number(value),
        assertthat::noNA(value),
        value >= min_value,
        value <= max_value,
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
        step_value <= max_value,
        #### hide
        assertthat::is.flag(hide),
        assertthat::noNA(hide),
        #### units
        assertthat::is.string(units),
        assertthat::noNA(units)
      )
      ### set fields
      self$id <- id
      self$name <- name
      self$status <- status
      self$value <- value
      self$min_value <- min_value
      self$max_value <- max_value
      self$step_value <- step_value
      self$hide <- hide
      self$units <- units
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Parameter")
      message("  id:       ", self$id)
      message("  name:     ", self$name)
      message("  status:   ", self$status)
      message("  value:   ", round(self$value, 2), " ", self$units)
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
        ", value: ", round(self$value, 2), " ", self$units, end
      )
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
    #' Get setting.
    #' @param name `character` setting name.
    #' Available options are `"status"`, or `"value"`.
    #' @return Value.
    get_setting = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "value")
      )
      if (identical(name, "status")) {
        out <- self$get_status()
      } else if (identical(name, "value")) {
        out <- self$get_value()
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      out
    },

    #' @description
    #' Set status.
    #' @param value `logical` new value.
    set_status = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
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
        value <= self$max_value
      )
      self$value <- value
      invisible(self)
    },

    #' @description
    #' Set setting.
    #' @param name `character` setting name.
    #' Available options are `"status"`, or `"value"`.
    #' @param value `ANY` new value.
    set_setting = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "value")
      )
      if (identical(name, "status")) {
        self$set_status(value)
      } else if (identical(name, "value")) {
        self$set_value(value)
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
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
        status = self$status,
        hide = self$hide,
        units = self$units
      )
    },

    #' @description
    #' Export settings.
    #' @return `list` object.
    export = function() {
      list(
        name = self$name,
        status = self$status,
        value = self$value,
        min_value = self$min_value,
        max_value = self$max_value,
        step_value = self$step_value,
        hide = self$hide,
        units = self$units
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
#' @param value `numeric` initial value.
#'   Defaults to 0.
#'
#' @param step_value `numeric` step value.
#'   Defaults to 1.
#'
#' @param units `character` units.
#'   Defaults to an empty `character` object.
#'
#' @param hide `logical` value indicating if the slider should be
#'   hidden when the parameter is disabled.
#'   Defaults to `FALSE`.
#'
#' @inheritParams new_multi_theme
#' @inheritParams new_feature
#'
#' @return A [Parameter] object.
#'
#' @examples
#' # create a new parameter
#' p <- new_parameter(name = "Spatial clustering")
#'
#' # print object
#' print(p)
#' @export
new_parameter <- function(name, status = TRUE, value = 0,
                          min_value = 0, max_value = 100, step_value = 1,
                          hide = FALSE, units = "",
                          id = uuid::UUIDgenerate()) {
  Parameter$new(
    id = id,
    name = name,
    status = status,
    min_value = min_value,
    max_value = max_value,
    value = value,
    step_value = step_value,
    hide = hide,
    units = units
  )
}
