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

    #' @field total `numeric` value.
    total = NA_real_,

    #' @field units `character` value.
    units = NA_character_,

    #' @description
    #' Create a Layer object.
    #' @param source `character` value.
    #' @param total `numeric` value.
    #' @param units `character` value.
    #' @return A new Layer object.
    initialize = function(source, total, units) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### source
        assertthat::is.string(source),
        assertthat::noNA(source),
        #### total
        assertthat::is.number(total),
        assertthat::noNA(total),
        isTRUE(total >= 0),
        #### units
        assertthat::is.string(units),
        assertthat::noNA(units))
      ### set fields
      self$source <- source
      self$total <- total
      self$units <- units
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Layer")
      message("  source: ", self$source)
      message("  total:  ", round(self$total, 2))
      message("  units:  ", self$units)
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
        ".../", basename(self$source),
        " ", start, "total: ", round(self$total, 2), " ",
        self$units, end)
    }
  )
)

#' New layer
#'
#' Create a new [Layer] object.
#'
#' @param source `character` file path for the underlying data.
#'
#' @param total `numeric` total amount of all values in the underlying data.
#'
#' @param units `character` units for the values in the underlying data.
#'
#' @return A [Layer] object.
#'
#' @examples
#' # create new object
#' l <- new_layer(source = tempfile(), total = 12, units = "ha")
#'
#' # print object
#' print(l)
#'
#' @export
new_layer <- function(source, total, units) {
  Layer$new(source = source, total = total, units = units)
}
