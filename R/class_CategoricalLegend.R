#' @include internal.R
NULL

#' Categorical legend class
#'
#' Definition for the `CategoricalLegend` class.
CategoricalLegend <- R6::R6Class(
  "CategoricalLegend",
  public = list(

    #' @field values `numeric` vector.
    values = NA_real_,

    #' @field colors `character` vector.
    colors = NA_character_,

    #' @description
    #' Create a `CategoricalLegend` object.
    #' @param values `numeric` value.
    #' @param colors `character` vector of colors.
    #' @return A new `CategoricalLegend` object.
    initialize = function(values, colors) {
      assertthat::assert_that(
        is.numeric(values),
        assertthat::noNA(values),
        is.character(colors),
        assertthat::noNA(colors),
        all(nchar(colors) %in% c(7, 9)),
        all(substr(colors, 1, 1) == "#")
      )
      self$values <- values
      self$colors <- colors
    },

    #' @description
    #' Get data for creating a widget.
    #' @return A new `CategoricalLegend` object.
    get_widget_data = function() {
      list(
        values = self$values,
        colors = self$colors,
        type = "CategoricalLegend"
      )
    },

    #' @description
    #' Get resample method.
    #' @return `character` object.
    get_resample_method = function() {
      "ngb"
    },

    #' @description
    #' Get a function for mapping values to colors.
    #' @return A `function` object.
    get_color_map = function() {
      leaflet::colorFactor(
        palette = self$colors,
        domain = NULL,
        levels = self$values,
        alpha = TRUE,
        na.color = NA
      )
    },

    #' @description
    #' Export settings
    #' @return `list` object.
    export = function() {
      list(
        type = "categorical",
        colors = self$colors
      )
    }
  )
)

#' New categorical legend
#'
#' Create a new [CategoricalLegend] object.
#'
#' @param values `numeric` Values to show in the legend.
#'
#' @param colors `character` Colors to show in the legend.
#'   These colors should be in hex format (e.g. `"#AABBCC"`).
#'
#' @return A [CategoricalLegend] object.
#'
#' @examples
#' # create new object
#' l <- new_categorical_legend(c(0, 1, 2), c("#000000", "#444444", "#AAAAAA"))
#'
#' # print object
#' print(l)
#' @export
new_categorical_legend <- function(values, colors) {
  CategoricalLegend$new(values = values, colors)
}
