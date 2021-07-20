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

    #' @description
    #' Create a Statistic object.
    #' @param name `character` value.
    #' @param value `numeric` value
    #' @param units `character` value.
    #' @return A Statistic object.
    initialize = function(name, value, units) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value),
        assertthat::is.string(name),
        assertthat::noNA(name),
        assertthat::is.string(units),
        assertthat::noNA(units)
      )
      self$name <- name
      self$value <- value
      self$units <- units
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message(paste0(self$name, " ", round(self$value, 2), " ", self$units))
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param ... not used.
    #' @return `character` value.
    repr = function(...) {
      paste0(self$name, " ", round(self$value, 2), " ", self$units)
    },

    #' @description
    #' Get widget data.
    #' @return `list` object.
    get_widget_data = function() {
      list(name = self$name, value = self$value, units = self$units)
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
#'
#' @param units `character` units.
#'
#' @return A [Statistic] object.
#'
#' @examples
#' # create a statistic
#' x <- new_statistic(name = "Area", value = 12, units = "ha")
#'
#' # print object
#' print(x)
#' @export
new_statistic <- function(name, value, units) {
  Statistic$new(name = name, value = value, units = units)
}
