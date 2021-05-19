#' @include internal.R Dataset-class.R
NULL

#' Weight class
#'
#' Definition for the Weight class.
#'
#' @seealso [new_weight()]
#'
#' @export
Weight <- R6::R6Class(
  "Weight",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field dataset [Dataset] object.
    dataset = NULL,

    #' @field status `logical` value.
    status = NA,

    #' @field initial_status `logical` value.
    initial_status = NA,

    #' @field factor `numeric` value.
    factor = NA_real_,

    #' @field initial_factor `numeric` initial factor value.
    initial_factor = NA_real_,

    #' @field min_factor `numeric` minimum factor value.
    min_factor = NA_real_,

    #' @field max_factor `numeric` maximum factor value.
    max_factor = NA_real_,

    #' @field step_factor `numeric` step factor value.
    step_factor = NA_real_,

    #' @description
    #' Create a new Weight object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param dataset [Dataset] object.
    #' @param initial_status `logical` value.
    #' @param initial_factor `numeric` initial factor value.
    #' @param min_factor `numeric` minimum factor value.
    #' @param max_factor `numeric` maximum factor value.
    #' @param step_factor `numeric` step factor value.
    #' @return A new Weight object.
    ## constructor
    initialize = function(
      id, name, dataset, initial_status,
      initial_factor, min_factor, max_factor, step_factor) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        ### dataset
        inherits(dataset, "Dataset"),
        #### initial_status
        assertthat::is.flag(initial_status),
        assertthat::noNA(initial_status),
        #### initial_factor
        assertthat::is.number(initial_factor),
        assertthat::noNA(initial_factor),
        initial_factor >= min_factor,
        initial_factor <= max_factor,
        #### min_factor
        assertthat::is.number(min_factor),
        assertthat::noNA(min_factor),
        min_factor <= max_factor,
        #### max_factor
        assertthat::is.number(max_factor),
        assertthat::noNA(max_factor),
        max_factor >= min_factor,
        #### step_factor
        assertthat::is.number(step_factor),
        assertthat::noNA(step_factor),
        step_factor <= max_factor
      )
      ### set fields
      self$id <- id
      self$dataset <- dataset
      self$name <- name
      self$status <- initial_status
      self$initial_status <- initial_status
      self$factor <- initial_factor
      self$initial_factor <- initial_factor
      self$min_factor <- min_factor
      self$max_factor <- max_factor
      self$step_factor <- step_factor
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Weight")
      message("  id:     ", self$id)
      message("  name:   ", self$name)
      message("  status: ", self$status)
      message("  factor: ", round(self$factor, 2))
      message("  dataset: ", self$dataset$repr())
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
        ", factor: ", round(self$factor, 2), end, nl(),
        "  dataset: ", self$dataset$repr())
    },

    #' @description
    #' Get factor.
    #' @return `numeric` value.
    get_factor = function() {
      self$factor
    },

    #' @description
    #' Get status.
    #' @return `logical` value.
    get_status = function() {
      self$status
    },

    #' @description
    #' Get parameter.
    #' @param name `character` parameter name.
    #' Available options are `"status"` or `"factor"`.
    #' @return Value.
    get_parameter = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "factor"))
      if (identical(name, "status")) {
        out <- self$get_status()
      } else if (identical(name, "factor")) {
        out <- self$get_factor()
      }
      out
    },

    #' @description
    #' Set factor.
    #' @param value `numeric` new value.
    set_factor = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value),
        value >= self$min_factor,
        value <= self$max_factor)
      self$factor <- value
      invisible(self)
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
    #' Set parameter.
    #' @param name `character` parameter name.
    #' Available options are `"status"` or `"factor"`.
    #' @param value `ANY` new value.
    set_parameter = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "factor"))
      if (identical(name, "status")) {
        self$set_status(value)
      } else if (identical(name, "factor")) {
        self$set_factor(value)
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
        min_factor = self$min_factor,
        max_factor = self$max_factor,
        initial_factor = self$factor,
        step_factor = self$step_factor,
        initial_status = self$status
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        units = self$dataset$units,
        legend = self$dataset$legend$get_widget_data(),
        type = "weight"
      )
    }

  )
)

#' New weight
#'
#' Create a new [Weight] object.
#'
#' @param min_factor `numeric` minimum factor value.
#'   Defaults to 0.
#'
#' @param max_factor `numeric` maximum factor value.
#    Defaults to 100
#'
#' @param initial_factor `numeric` initial factor value.
#'   Defaults to 0.
#'
#' @param step_factor `numeric` step factor value.
#'   Defaults to 1.
#'
#' @inheritParams new_multi_theme
#' @inheritParams new_feature
#'
#' @return A [Weight] object.
#'
#' @examples
#' # create a new dataset
#' l <- new_dataset(
#'  source = tempfile(), total = 12, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#1b9e77")))
#'
#' # create a new weight
#' w <- new_weight(name = "NDVI", dataset = l)
#'
#' # print object
#' print(w)
#'
#' @export
new_weight <- function(
  name, dataset,
  initial_status = TRUE, initial_factor = 0,
  min_factor = 0, max_factor = 100, step_factor = 1,
  id = uuid::UUIDgenerate()) {
  Weight$new(
    id = id,
    name = name,
    dataset = dataset,
    min_factor = min_factor,
    max_factor = max_factor,
    initial_factor = initial_factor,
    step_factor = step_factor,
    initial_status = initial_status)
}
