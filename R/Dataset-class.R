#' @include internal.R
NULL

#' Dataset class
#'
#' Definition for the Dataset class.
#'
#' @seealso [new_dataset()].
Dataset <- R6::R6Class(
  "Dataset",
  public = list(

    #' @field source `character`, [sf::st_sf()], or [raster::stack()] object.
    source = NULL,

    #' @field data `NULL`, [sf::st_sf()], or [raster::stack()] object.
    data = NULL,

    #' @description
    #' Create a Dataset object.
    #' @param source `character`, [sf::st_sf()], or [raster::stack()] object.
    #' @return A new Dataset object.
    initialize = function(source) {
      ## assert that arguments are valid
      assertthat::assert_that(
        inherits(source, c("character", "sf", "Raster")))
      ## set fields
      if (inherits(source, "character")) {
        ### if a file has been supplied
        self$source <- source
        self$data <- NULL
      } else {
        ### if a spatial dataset has been supplied
        self$source <- "memory"
        self$data <- source
      }
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Dataset")
      message("  source: ", self$source)
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      if (identical(self$source, "memory")) {
        out <- self$source
      } else {
        out <- paste0(".../", basename(self$source))
      }
      out
    },

    #' @description
    #' Import the dataset into memory.
    import = function() {
      # if data are stored in memory, then no need to import it
      if (identical(self$source, "memory")) {
        return(self)
      }
      # otherwise, actually import data
      self$data <- read_spatial_data(self$source)
      invisible(self)
    },

    #' @description
    #' Clean the dataset from memory.
    #' @details
    #' Note that this method has no effect if the dataset does not have
    #' file path on disk.
    clean = function() {
      if (!identical(self$source, "memory")) {
        self$data <- NULL
      }
      invisible(self)
    },

    #' @description
    #' Get the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_data = function() {
      if (is.null(self$data)) {
        self$import()
      }
      self$data
    },

    #' @description
    #' Get a variable from the data.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_variable = function(index) {
      assertthat::assert_that(
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index))
      if (is.null(self$data)) {
        self$import()
      }
      if (inherits(self$data, "Raster")) {
        out <- self$data[[index]]
      } else {
        out <- self$data[, index]
      }
      out
    },

    #' @description
    #' Check if the dataset has a variable.
    #' @inheritParams get_variable
    #' @return `logical` indicating if the variable is present or not.
    has_variable = function(index) {
      assertthat::assert_that(
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index))
      if (is.null(self$data)) {
        self$import()
      }
      if (is.numeric(index)) {
        out <- index %in% seq_along(names(self$data))
      } else {
        out <- index %in% names(self$data)
      }
      out
    }

  )
)

#' New dataset
#'
#' Create a new [Dataset] object.
#'
#' @param source `character`, [sf::st_sf()], or [raster::stack()] object.
#'
#' @return A [Dataset] object.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, package = "prioritizr")
#' r <- sim_pu_raster
#'
#' # create new dataset
#' d <- new_dataset(r)
#'
#' # print object
#' print(d)
#'
#' @export
new_dataset <- function(source) {
  Dataset$new(source = source)
}
