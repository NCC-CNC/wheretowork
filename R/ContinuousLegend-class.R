#' @include internal.R
NULL

#' ContinuousLegend class
#'
#' Definition for the ContinuousLegend class.
ContinuousLegend <- R6::R6Class(
  "ContinuousLegend",
  public = list(

    #' @field min_value `numeric` value.
    min_value = NA_real_,

    #' @field max_value `numeric` value.
    max_value = NA_real_,

    #' @field colors `character` vector.
    colors = NA_character_,

    #' @field n `numeric` value.
    n = NA_real_,

    #' @description
    #' Create a ContinuousLegend object.
    #' @param min_value `numeric` value.
    #' @param max_value `numeric` value.
    #' @param colors `character` vector of colors.
    #' @param n `numeric` value.
    #' @return A new ContinuousLegend object.
    initialize = function(min_value, max_value, colors, n) {
      assertthat::assert_that(
        assertthat::is.number(min_value),
        assertthat::noNA(min_value),
        assertthat::is.number(max_value),
        assertthat::noNA(max_value),
        assertthat::is.count(n),
        assertthat::noNA(n),
        max_value >= min_value,
        is.character(colors),
        assertthat::noNA(min_value),
        all(nchar(colors) %in% c(7, 9)),
        all(substr(colors, 1, 1) == "#"))
      self$min_value <- min_value
      self$max_value <- max_value
      self$colors <- colors
      self$n <- n
    },

    #' @description
    #' Get data for creating a widget.
    #' @return A new ContinuousLegend object.
    get_widget_data = function() {
      # calculate breaks
      br <- scales::breaks_extended(n = self$n)(
        c(self$min_value, self$max_value))
      list(
        min_value = min(self$min_value, min(br)),
        max_value = max(self$max_value, max(br)),
        colors = self$colors,
        values = br,
        type = "ContinuousLegend"
      )
    },

    #' @description
    #' Get resample method.
    #' @return `character` object.
    get_resample_method = function() {
      "bilinear"
    },

    #' @description
    #' Get a function for mapping values to colors.
    #' @return A `function` object.
    get_color_map = function() {
      # calculate data
      d <- self$get_widget_data()
      # return color map
      leaflet::colorNumeric(
        palette = self$colors,
        domain = c(d$min_value, d$max_value),
        alpha = TRUE,
        na.color = NA
      )
    },

    #' @description
    #' Export parameters
    #' @return `list` object.
    export = function() {
      list(
        type = "continuous",
        colors = self$colors
      )
    }

  )
)

#' New continuous legend
#'
#' Create a new [ContinuousLegend] object.
#'
#' @param min_value `numeric` Minimum value to show in the legend.
#'
#' @param max_value `numeric` Maximum value to show in the legend.
#'
#' @param colors `character` Colors to show in the color the legend.
#'   These colors should be in hex format (e.g. `"#AABBCC"`) and
#'   are used to generate the color bar.
#'
#' @param n `integer` Number of labels to display in the color bar.
#'   Default to 4.
#'
#' @return A [ContinuousLegend] object.
#'
#' @examples
#' # create new object
#' l <- new_continuous_legend(0, 1, c("#000000", "#DDDDDD"))
#'
#' # print object
#' print(l)
#'
#' @export
new_continuous_legend <- function(min_value, max_value, colors, n = 4) {
  ContinuousLegend$new(
    min_value = min_value, max_value = max_value, colors = colors, n = n)
}
