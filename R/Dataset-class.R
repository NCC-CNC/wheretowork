#' @include internal.R
NULL

#' Dataset class
#'
#' Definition for the Dataset class.
#'
#' @seealso [new_dataset()]
Dataset <- R6::R6Class(
  "Dataset",
  public = list(

    #' @field source `character` value.
    source = NA_character_,

    #' @field total `numeric` value.
    total = NA_real_,

    #' @field units `character` value.
    units = NA_character_,

    #' @field legend `Legend` object.
    legend = NULL,

    #' @description
    #' Create a Dataset object.
    #' @param source `character` value.
    #' @param total `numeric` value.
    #' @param units `character` value.
    #' @param legend `Legend` object.
    #' @return A new Dataset object.
    initialize = function(source, total, units, legend) {
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
        assertthat::noNA(units),
        #### legend
        inherits(legend, c("ContinuousLegend", "CategoricalLegend")))
      ### set fields
      self$source <- source
      self$total <- total
      self$units <- units
      self$legend <- legend
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Dataset")
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

#' New dataset
#'
#' Create a new [Dataset] object.
#'
#' @param source `character` file path for the underlying data.
#'
#' @param total `numeric` total amount of all values in the underlying data.
#'
#' @param units `character` units for the values in the underlying data.
#'
#' @param legend `Legend` object.
#'
#' @return A [Dataset] object.
#'
#' @examples
#' # create new object
#' l <- new_dataset(
#'   source = tempfile(), total = 12, units = "ha",
#'   legend = new_continuous_legend(1, 100, c("#000000", "#AAAAAA")))
#'
#' # print object
#' print(l)
#'
#' @export
new_dataset <- function(source, total, units, legend) {
  Dataset$new(source = source, total = total, units = units, legend = legend)
}
