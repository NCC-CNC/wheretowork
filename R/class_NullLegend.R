#' @include internal.R
NULL

#' NULL legend class
#'
#' Definition for the `NullLegend` class.
NullLegend <- R6::R6Class(
  "NullLegend",
  public = list(
    #' @description
    #' Create a `NullLegend` object.
    #' @return A new `NullLegend` object.
    initialize = function() {
    },

    #' @description
    #' Get data for creating a widget.
    #' @return A new `NullLegend` object.
    get_widget_data = function() {
      list(
        type = "NullLegend"
      )
    },
    
    #' @description
    #' Export settings
    #' @return `list` object.
    export = function() {
      list(
        type = "null",
        colors = "NA",
        labels = "NA"
      )
    }    
  )
)

#' New null legend
#'
#' Create a new [NullLegend] object.
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
new_null_legend <- function() {
  NullLegend$new()
}
