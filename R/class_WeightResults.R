#' @include internal.R class_Variable.R
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

    #' @field current `numeric` value.
    current = NA_real_,

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
      self$current <- weight$current
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("WeightResults")
      message("  id:      ", self$id)
      message("  status: ", self$status)
      message("  factor: ", round(self$factor, 2))
      message("  current: ", round(self$current, 2))
      message("  held: ", round(self$held, 2))
      message("  weight: ", self$weight$repr())
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param start `character` symbol used to start the setting list.
    #'   Defaults to `"["`.
    #' @param end `character` symbol used to start the setting list.
    #'   Defaults to `"]"`.
    #' @return `character` value.
    repr = function(start = "[", end = "]") {
      paste0(
        self$weight$name,
        " ", start, "status: ", self$status,
        ", factor: ", round(self$factor, 2),
        ", held: ", round(self$held, 2), end, nl(),
        "  weight: ", self$weight$repr()
      )
    },

    #' @description
    #' Get results.
    #' @return [tibble::tibble()] object.
    get_results_data = function() {
      tibble::tibble(
        name = self$weight$name,
        status = self$status,
        total = self$weight$variable$total,
        current = self$current,
        factor = self$factor,
        held = self$held,
        units = self$weight$variable$units
      )
    },

    #' @description
    #' Get data for displaying the object in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$weight$name,
        status = self$status,
        factor = self$factor,
        total_amount = self$weight$variable$total,
        current_held = self$current,
        solution_held = self$held,
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
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # create new variable
#' v <- new_variable_from_auto(d, index = 1)
#'
#' # create a new weight
#' w <- new_weight(name = "NDVI", variable = v)
#'
#' # create a new weight results object to store results
#' wr <- new_weight_results(w, 80)
#'
#' # print object
#' print(w)
#' @export
new_weight_results <- function(weight, held, id = uuid::UUIDgenerate()) {
  WeightResults$new(
    id = id,
    weight = weight,
    held = held
  )
}
