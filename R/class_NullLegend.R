#' @include internal.R
NULL

#' NULL legend class
#'
#' Definition for the `NullLegend` class.
NullLegend <- R6::R6Class(
  "NullLegend",
  public = list(

    #' @field values `numeric` vector.
    values = NULL,

    #' @field colors `character` vector.
    colors = NULL,

    #' @description
    #' Create a `NullLegend` object.
    #' @param values `NULL` value.
    #' @param colors `NULL` vector of colors.
    #' @return A new `NullLegend` object.
    initialize = function(values, colors) {
      self$values <- NULL
      self$colors <- NULL
    },

    #' @description
    #' Get data for creating a widget.
    #' @return A new `CategoricalLegend` object.
    get_widget_data = function() {
      list(
        values = NULL,
        colors = NULL,
        type = "NullLegend"
      )
    }
  )
)

#' New categorical legend
#'
#' Create a new [NullLegend] object.
#'
#' @param values `NULL` 
#'
#' @param colors `NULL` 
#'
#' @return A [NullLegend] object.
#'
#' @examples
#' # create new object
#' l <- new_null_legend()
#'
#' # print object
#' print(l)
#' @export
new_null_legend <- function(values = NULL, colors = NULL) {
  NullLegend$new(values = values, colors)
}
