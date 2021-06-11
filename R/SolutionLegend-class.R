#' @include internal.R
NULL

#' SolutionLegend class
#'
#' Definition for the SolutionLegend class.
SolutionLegend <- R6::R6Class(
  "SolutionLegend",
  public = list(

    #' @field colors `character` vector.
    colors = NA_character_,

    #' @description
    #' Create a CategoricalLegend object.
    #' @param colors `character` vector of colors.
    #' @return A new SolutionLegend object.
    initialize = function(colors) {
      assertthat::assert_that(
        is.character(colors),
        assertthat::noNA(colors),
        length(colors) == 2,
        all(nchar(colors) %in% c(7, 9)),
        all(substr(colors, 1, 1) == "#"))
      self$colors <- colors
    },

    #' @description
    #' Get data for creating a widget.
    #' @return A new CategoricalLegend object.
    get_widget_data = function() {
      list(
        values = c("not selected", "selected"),
        colors = self$colors,
        type = "CategoricalLegend"
      )
    }
  )
)

#' New solution legend
#'
#' Create a new [SolutionLegend] object.
#'
#' @param colors `character` Colors to show in the legend.
#'   These colors should be in hex format (e.g. `"#AABBCC"`).
#'   Arguments should contain two different colors.
#'
#' @return A [SolutionLegend] object.
#'
#' @examples
#' # create new object
#' l <- new_solution_legend(c("#000000", "#AAAAAA"))
#'
#' # print object
#' print(l)
#'
#' @export
new_solution_legend <- function(colors) {
  SolutionLegend$new(colors)
}
