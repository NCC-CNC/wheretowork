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
    
    #' @field disable `logical` value.
    disable = NA,

    #' @field units `character` value.
    units = NA_character_,

    #' @field reference_value `numeric` reference value.
    reference_value = NA_real_,

    #' @field reference_units `character` units for reference value.
    reference_units = NA_character_,

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
    #' @param disable `logical` value.
    #' @param units `character` value.
    #' @param reference_value `numeric` value.
    #' @param reference_units `character` value.
    #' @return A new Parameter object.
    ## constructor
    initialize = function(id, name, status,
                          value, min_value, max_value, step_value, hide, 
                          disable, units, reference_value, reference_units) {
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
        #### reference_value
        assertthat::is.number(reference_value),
        #### reference_units
        assertthat::is.string(reference_units),
        assertthat::noNA(reference_units),
        #### hide
        assertthat::is.flag(hide),
        assertthat::noNA(hide),
        #### disable
        assertthat::is.flag(disable),
        assertthat::noNA(disable),        
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
      self$reference_value <- reference_value
      self$reference_units <- reference_units
      self$hide <- hide
      self$disable <- disable
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
      message(
        "  reference_value:   ",  round(self$reference_value, 2), " ",
        self$reference_units)
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
    #' Get data for displaying the parameter in a widget.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        status = self$status,
        value = self$value,
        min_value = self$min_value,
        max_value = self$max_value,
        step_value = self$step_value,
        hide = self$hide,
        disable = self$disable,
        units = self$units,
        reference_value = self$reference_value,
        reference_units = self$reference_units
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
        disable = self$disable,
        units = self$units,
        reference_value = self$reference_value,
        reference_units = self$reference_units
      )
    },

    #' @description
    #' Get results data.
    #' @return `data.frame` object.
    get_results_data = function() {
      data.frame(
        name = self$name,
        value = self$value,
        units = self$units,
        hide = self$hide
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
#' @param reference_value `numeric` reference value.
#'   This parameter is useful if the `value` is a relative value,
#'   because the total expected amount (i.e. `reference_value * value`)
#'   can be reported.
#'   Defaults to `NA_real_` indicating that no reference value information
#'   should be reported.
#'
#' @param reference_units `character` units for the reference value.
#'   Defaults to "".
#'
#' @param units `character` units.
#'   Defaults to an empty `character` object.
#'
#' @param hide `logical` indicating if the slider should be
#'   hidden when the setting switch is on.
#'   Defaults to `FALSE`.
#'   
#' @param disable `logical` indicating if the setting should be disabled.
#'   Defaults to `FALSE`.  
#'
#' @inheritParams new_theme
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
                          hide = FALSE, disable = FALSE, units = "",
                          reference_value = NA_real_,
                          reference_units = "",
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
    disable = disable,
    reference_value = reference_value,
    reference_units = reference_units,
    units = units
  )
}
