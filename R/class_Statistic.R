#' @include internal.R
NULL

#' Statistic class
#'
#' Definition for the Statistic class.
#'
#' @seealso [new_statistic()].
Statistic <- R6::R6Class(
  "Statistic",
  public = list(

    #' @field name `character` value.
    name = NA_character_,

    #' @field value `numeric` value
    value = NA_real_,

    #' @field units `character` value.
    units = FALSE,

    #' @field proportion `numeric` value
    proportion = NA_real_,

    #' @description
    #' Create a Statistic object.
    #' @param name `character` value.
    #' @param value `numeric` value
    #' @param units `character` value.
    #' @param proportion `numeric` value.
    #' @return A Statistic object.
    initialize = function(name, value, units, proportion) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value),
        assertthat::is.number(proportion),
        assertthat::is.string(name),
        assertthat::noNA(name),
        assertthat::is.string(units),
        assertthat::noNA(units)
      )
      self$name <- name
      self$value <- value
      self$units <- units
      self$proportion <- proportion
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message(self$repr())
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param ... not used.
    #' @return `character` value.
    repr = function(...) {
      x <- paste0(self$name, " ", round(self$value, 2), " ", self$units)
      if (!is.na(self$proportion)) {
        x <- paste0(x, " [", self$proportion * 100, "%]")
      }
      x
    },

    #' @description
    #' Get widget data.
    #' @return `list` object.
    get_widget_data = function() {
      list(
        name = self$name,
        value = self$value,
        units = self$units,
        proportion = self$proportion
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
        proportion = self$proportion
      )
    }

  )
)

#' New statistic
#'
#' Create a new [Statistic] object.
#'
#' @param name `character` name of statistic.
#'
#' @param value `numeric` value.
#'  This parameter describes the statistic in absolute terms (e.g. 30).
#'
#' @param units `character` value.
#'  This parameter contains the units for the statistic (e.g. `"km
#'
#' @param proportion `numeric` value (optional) .
#' This parameter describes the statistic in relative terms (e.g. 30%).
#' Note that values are expressed as a proportion (e.g. 0.3 indicates 30%).
#'
#' @return A [Statistic] object.
#'
#' @examples
#' # create a statistic
#' x <- new_statistic(name = "Area", value = 12, units = "ha", proportion = 0.4)
#'
#' # print object
#' print(x)
#' @export
new_statistic <- function(name, value, units, proportion = NA_real_) {
  Statistic$new(
    name = name,
    value = value,
    units = units,
    proportion = proportion
  )
}
