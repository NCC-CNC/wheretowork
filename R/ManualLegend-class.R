#' @include internal.R
NULL

#' ManualLegend class
#'
#' Definition for the ManualLegend class.
ManualLegend <- R6::R6Class(
  "ManualLegend",
  public = list(

    #' @field colors `character` vector.
    colors = NA_character_,

    #' @field labels `character` vector.
    labels = NA_character_,

    #' @description
    #' Create a ManualLegend object.
    #' @param colors `character` vector of colors.
    #' @param labels `character` vector of labels.
    #' @return A new ManualLegend object.
    initialize = function(colors, labels) {
      assertthat::assert_that(
        is.character(colors),
        assertthat::noNA(colors),
        all(nchar(colors) %in% c(7, 9)),
        all(substr(colors, 1, 1) == "#"),
        is.character(labels),
        assertthat::noNA(labels),
        length(colors) == length(labels))
      self$colors <- colors
      self$labels <- labels
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
        levels = c(0, 1),
        alpha = TRUE,
        na.color = NA
      )
    },

    #' @description
    #' Get data for creating a widget.
    #' @return A `list` object.
    get_widget_data = function() {
      list(
        values = self$labels,
        colors = self$colors,
        type = "CategoricalLegend"
      )
    },

    #' @description
    #' Export settings
    #' @return `list` object.
    export = function() {
      list(
        type = "manual",
        colors = self$colors,
        labels = self$labels
      )
    }

  )
)

#' New manual legend
#'
#' Create a new [ManualLegend] object.
#'
#' @param colors `character` Colors to show in the legend.
#'   These colors should be in hex format (e.g. `"#AABBCC"`).
#'   Arguments should contain two different colors.
#'
#' @param labels `character` Labels to show in the legend.
#'
#' @return A [ManualLegend] object.
#'
#' @examples
#' # create new object
#' l <- new_manual_legend(c("#000000", "#AAAAAA"), c("a", "b"))
#'
#' # print object
#' print(l)
#'
#' @export
new_manual_legend <- function(colors, labels) {
  ManualLegend$new(colors, labels)
}
