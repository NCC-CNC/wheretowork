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

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field source `character`, [sf::st_sf()], or [raster::stack()] object.
    source = NULL,

    #' @field data `NULL`, [sf::st_sf()], or [raster::stack()] object.
    data = NULL,

    #' @field cells `integer` indices of grid cells.
    #'  This is used to cache calculations for [raster::stack()] data.
    cells = NULL,

    #' @description
    #' Create a Dataset object.
    #' @param id `character` value.
    #' @param source `character`, [sf::st_sf()], or [raster::stack()] object.
    #' @return A new Dataset object.
    initialize = function(id, source) {
      ## assert that arguments are valid
      assertthat::assert_that(
        assertthat::is.string(id),
        assertthat::noNA(id),
        inherits(source, c("character", "sf", "Raster")))
      ## set fields
      self$id <- id
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
    self$cells <- NULL
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
    #' Get the coordinate reference system.
    #' @return [sf::st_crs()] object.
    get_crs = function() {
      if (is.null(self$data)) {
        self$import()
      }
      sf::st_crs(self$data)
    },

    #' @description
    #' Get indices of cells with data.
    #' @details Note that this method only works for Raster data.
    #' @return `integer` vector of indices.
    get_finite_cells = function() {
      if (is.null(self$data)) {
        self$import()
      }
      if (!inherits(self$data, "Raster")) {
        stop("data is not Raster format.")
      }
      if (is.null(self$cells)) {
        self$cells <- raster::Which(!is.na(self$data[[1]]), cells = TRUE)
      }
      self$cells
    },

    #' @description
    #' Get area values.
    #' @return `numeric` vector of values.
    get_planning_unit_areas = function() {
      if (is.null(self$data)) {
        self$import()
      }
      if (inherits(self$data, "Raster")) {
        out <-
          rep(prod(raster::res(self$data)), length(self$get_finite_cells()))
      } else {
        out <- as.numeric(sf::st_area(self$data))
      }
      out
    },

    #' @description
    #' Get a data from the dataset at an index.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_index = function(index) {
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
    #' Get a data from the dataset at a set of indices.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_indices = function(index) {
      assertthat::assert_that(
        is.character(index) || is.numeric(index),
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
    #' Check if the dataset has an index.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @return `logical` indicating if data is present or not.
    has_index = function(index) {
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
    },

    #' @description
    #' Maximum index.
    #' @return `integer` largest index.
    max_index = function() {
      if (is.null(self$data)) {
        self$import()
      }
      length(names(self$data))
    },

    #' @description
    #' Add data at an index.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @param values `numeric` vector.
    add_index = function(index, values) {
      assertthat::assert_that(
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index))
      if (is.null(self$data)) {
        self$import()
      }
      if (inherits(self$data, "Raster")) {
        d <- raster::setValues(self$data[[1]], NA)
        d[self$get_finite_cells()] <- values
        self$data <- raster::addLayer(self$data, d)
      } else {
        self$data[[index]] <- values
      }
      invisible(self)
    }

  )
)

#' New dataset
#'
#' Create a new [Dataset] object.
#'
#' @param source `character`, [sf::st_sf()], or [raster::stack()] object.
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [Dataset] object.
#'
#' @examples
#' # find data path
#' f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
#'
#' # create new dataset
#' d <- new_dataset(f)
#'
#' # print object
#' print(d)
#'
#' @export
new_dataset <- function(source, id = uuid::UUIDgenerate()) {
  Dataset$new(id = id, source = source)
}
