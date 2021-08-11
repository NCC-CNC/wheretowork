#' @include internal.R class_Variable.R
NULL

#' Include results class
#'
#' Definition for the `IncludeResults` class.
#'
#' @seealso [new_include_results()].
#'
#' @export
IncludeResults <- R6::R6Class(
  "IncludeResults",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field include [Include] object.
    include = NA_character_,

    #' @field status `logical` value.
    status = NA,

    #' @field held `numeric` value.
    held = NA_real_,

    #' @description
    #' Create a new `IncludeResults` object.
    #' @param id `character` value.
    #' @param include [Include] object.
    #' @param held `numeric` value.
    #' @return A new `IncludeResults` object.
    ## constructor
    initialize = function(id, include, held) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ### include
        inherits(include, "Include"),
        #### held
        assertthat::is.number(held),
        assertthat::noNA(held)
      )
      ### set fields
      self$id <- id
      self$include <- include
      self$status <- include$status
      self$held <- held
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("IncludeResults")
      message("  id:      ", self$id)
      message("  status: ", self$status)
      message("  held: ", round(self$held, 2))
      message("  include: ", self$include$repr())
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
        self$include$name,
        " ", start, "status: ", self$status,
        ", held: ", round(self$held, 2), end, nl(),
        "  include: ", self$include$repr()
      )
    },

    #' @description
    #' Get results.
    #' @return [tibble::tibble()] object.
    get_results_data = function() {
      tibble::tibble(
        name = self$include$name,
        status = self$status,
        total = self$include$variable$total,
        held = self$held,
        units = self$include$variable$units
      )
    },

    #' @description
    #' Get data for displaying the object in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$include$name,
        status = self$status,
        total_amount = self$include$variable$total,
        solution_held = self$held,
        units = self$include$variable$units,
        type = "include_results"
      )
    }
  )
)

#' New include results
#'
#' Create a new [IncludeResults] object to store results for a solution.
#'
#' @param include [Weight] object.
#'
#' @param held `numeric` proportion of the include covered by the solution.
#'   (e.g. 0.1 = 10%).
#'
#' @inheritParams new_include
#'
#' @return A [IncludeResults] object.
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
#' # create a new include
#' i <- new_include(name = "NDVI", variable = v)
#'
#' # create a new include results object to store results
#' ir <- new_include_results(i, 80)
#'
#' # print object
#' print(ir)
#' @export
new_include_results <- function(include, held, id = uuid::UUIDgenerate()) {
  IncludeResults$new(
    id = id,
    include = include,
    held = held
  )
}
