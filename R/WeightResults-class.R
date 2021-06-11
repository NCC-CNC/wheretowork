#' @include internal.R Variable-class.R
NULL

#' WeightResults class
#'
#' Definition for the WeightResults class.
#'
#' @seealso [new_weight_results()].
#'
#' @export
WeightResults <- R6::R6Class(
  "WeightResults",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field weight [Weight] object.
    weight = NA_character_,

    #' @field status `logical` value.
    status = NA,

    #' @field factor `numeric` value.
    factor = NA_real_,

    #' @field held `numeric` value.
    held = NA_real_,

    #' @description
    #' Create a new WeightResults object.
    #' @param id `character` value.
    #' @param weight [Weight] object.
    #' @param held `numeric` value.
    #' @return A new WeightResults object.
    ## constructor
    initialize = function(id, weight, held) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ### weight
        inherits(weight, "Weight"),
        #### held
        assertthat::is.number(held),
        assertthat::noNA(held)
      )
      ### set fields
      self$id <- id
      self$weight <- weight
      self$status <- weight$status
      self$factor <- weight$factor
      self$held <- held
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("WeightResults")
      message("  id:      ", self$id)
      message("  status: ", self$status)
      message("  factor: ", round(self$factor, 2))
      message("  held: ", round(self$held, 2))
      message("  weight: ", self$weight$repr())
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
        self$name,
        " ", start, "status: ", self$status,
        ", factor: ", round(self$factor, 2), end, nl(),
        ", held: ", round(self$held, 2), end, nl(),
        "  weight: ", self$weight$repr())
    },

    #' @description
    #' Get data for displaying the object in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$weight$name,
        status = self$status,
        total = self$weight$variable$total,
        factor = self$factor,
        held = self$held,
        units = self$weight$variable$units,
        type = "weight_results"
      )
    }
  )
)

#' New weight results
#'
#' Create a new [WeightResults] object to store results for a solution.
#'
#' @param weight [Weight] object.
#'
#' @param held `numeric` proportion of the weight covered by the solution.
#'   (e.g. 0.1 = 10%).
#'
#' @inheritParams new_weight
#'
#' @return A [WeightResults] object.
#'
#' @examples
#' #TODO
#'
#' @export
new_weight_results <- function(
  weight, held, id = uuid::UUIDgenerate()) {
  WeightResults$new(
    id = id,
    weight = weight,
    held = held)
}
