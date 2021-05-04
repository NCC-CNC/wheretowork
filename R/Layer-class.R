#' @include internal.R
NULL

#' Layer class
#'
#' Definition for the Layer class.
#'
#' @seealso [new_layer()]
Layer <- R6::R6Class(
  "Layer",
  public = list(

    #' @field source `character` value.
    source = NA_character_,

    #' @field current `numeric` value.
    current = NA_real_,

    #' @field total `numeric` value.
    total = NA_real_,

    #' @field units `character` value.
    units = NA_character_,

    #' @description
    #' Create a Layer object.
    #' @param source `character` value.
    #' @param current `numeric` value.
    #' @param total `numeric` value.
    #' @param units `character` value.
    #' @return A new Layer object.
    initialize = function(
      source, current, total, units) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### source
        assertthat::is.string(source),
        assertthat::noNA(source),
        #### current
        assertthat::is.number(current),
        assertthat::noNA(current),
        isTRUE(current >= 0),
        isTRUE(current <= 1),
        #### total
        assertthat::is.number(total),
        assertthat::noNA(total),
        isTRUE(total >= 0),
        #### units
        assertthat::is.string(units),
        assertthat::noNA(units))
      ### set fields
      self$source <- source
      self$current <- current
      self$total <- total
      self$units <- units
    }
  )
)

#' New layer
#'
#' Create a new [Layer] object.
#'
#' @param source `character` file path for the underlying data.
#'
#' @param current `numeric` current proportion of values held in existing
#'   conservation areas (e.g. 0.1 = 10%).
#'
#' @param total `numeric` total amount of all values in the underlying data.
#'
#' @param units `character` units for the values in the underlying data.
#'
#' @return A [Layer] object.
#'
#' @examples
#' # create new object
#' l <- new_layer(source = tempfile(), current = 0.1, total = 12, units = "ha")
#'
#' # print object
#' print(l)
#'
#' @export
new_layer <- function(source, current, total, units) {
  Layer$new(source = source, current = current, total = total, units = units)
}
