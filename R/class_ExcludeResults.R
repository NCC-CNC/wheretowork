#' @include internal.R class_Variable.R
NULL

#' Exclude results class
#'
#' Definition for the `ExcludeResults` class.
#'
#' @seealso [new_exclude_results()].
#'
#' @export
ExcludeResults <- R6::R6Class(
  "ExcludeResults",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field exclude [Exclude] object.
    exclude = NA_character_,

    #' @field status `logical` value.
    status = NA,

    #' @field held `numeric` value.
    held = NA_real_,

    #' @description
    #' Create a new `ExcludeResults` object.
    #' @param id `character` value.
    #' @param exclude [Exclude] object.
    #' @param held `numeric` value.
    #' @return A new `ExcludeResults` object.
    ## constructor
    initialize = function(id, exclude, held) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ### exclude
        inherits(exclude, "Exclude"),
        #### held
        assertthat::is.number(held),
        assertthat::noNA(held)
      )
      ### set fields
      self$id <- id
      self$exclude <- exclude
      self$status <- exclude$status
      self$held <- held
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("ExcludeResults")
      message("  id:      ", self$id)
      message("  status: ", self$status)
      message("  held: ", round(self$held, 2))
      message("  exclude: ", self$exclude$repr())
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
        self$exclude$name,
        " ", start, "status: ", self$status,
        ", held: ", round(self$held, 2), end, nl(),
        "  exclude: ", self$exclude$repr()
      )
    },

    #' @description
    #' Get results.
    #' @return [tibble::tibble()] object.
    get_results_data = function() {
      tibble::tibble(
        name = self$exclude$name,
        status = self$status,
        total = self$exclude$variable$total,
        held = self$held,
        units = self$exclude$variable$units
      )
    },

    #' @description
    #' Get data for displaying the object in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$exclude$name,
        status = self$status,
        total_amount = self$exclude$variable$total,
        solution_held = self$held,
        units = self$exclude$variable$units,
        provenance = self$exclude$variable$provenance$get_widget_data(),
        type = "exclude_results"
      )
    }
  )
)

#' New exclude results
#'
#' Create a new [ExcludeResults] object to store results for a solution.
#'
#' @param exclude [Weight] object.
#'
#' @param held `numeric` proportion of the exclude covered by the solution.
#'   (e.g. 0.1 = 10%).
#'
#' @inheritParams new_exclude
#'
#' @return A [ExcludeResults] object.
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
#' # create a new exclude
#' excl <- new_exclude(name = "Highways", variable = v)
#'
#' # create a new exclude results object to store results
#' exclr <- new_exclude_results(excl, 80)
#'
#' # print object
#' print(er)
#' @export
new_exclude_results <- function(exclude, held, id = uuid::UUIDgenerate()) {
  ExcludeResults$new(
    id = id,
    exclude = exclude,
    held = held
  )
}
