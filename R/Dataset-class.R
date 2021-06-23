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

    #' @field source `character` file path.
    spatial_path = NA_character_,

    #' @field source `character` file path.
    attribute_path = NA_character_,

    #' @field source `character` file path.
    boundary_path = NA_character_,

    #' @field `NULL`, [sf::st_sf()], or [raster::raster()] object.
    spatial_data = NULL,

    #' @field `NULL`, or [tibble::tibble()] object.
    attribute_data = NULL,

    #' @field `NULL`, or [Matrix::sparseMatrix()] object.
    boundary_data = NULL,

    #' @description
    #' Create a Dataset object.
    #' @param id `character` value.
    #' @param spatial_path `character` file path.
    #' @param attribute_path `character` file path.
    #' @param boundary_path `character` file path.
    #' @param spatial_data [sf::st_sf()], or [raster::raster()] object.
    #' @param attribute_data [tibble::tibble()] object.
    #' @param boundary_data [tibble::tibble()] object.
    #' @return A new Dataset object.
    initialize = function(
      id, spatial_path, attribute_path, boundary_path,
      spatial_data, attribute_data, boundary_data) {
      ## assert that arguments are valid
      assertthat::assert_that(
        ## id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ## spatial_path
        assertthat::is.string(spatial_path),
        assertthat::noNA(spatial_path),
        ## attribute_path
        assertthat::is.string(attribute_path),
        assertthat::noNA(attribute_path),
        ## boundary_path
        assertthat::noNA(boundary_path),
        assertthat::is.string(boundary_path),
        ## spatial_data
        inherits(spatial_data, c("NULL", "sf", "Raster")),
        ## attribute_data
        inherits(attribute_data, c("NULL", "data.frame")),
        ## boundary_data
        inherits(boundary_data, c("NULL", "dgCMatrix"))
      )
      ## set fields
      self$id <- id
      self$spatial_path <- spatial_path
      self$attribute_path <- attribute_path
      self$boundary_path <- boundary_path
      self$spatial_data <- spatial_data
      self$attribute_data <- attribute_data
      self$boundary_data <- boundary_data
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Dataset")
      message("  paths:")
      message("    spatial: ", self$spatial_path)
      message("    attribute: ", self$attribute_path)
      message("    boundary: ", self$boundary_path)
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      if (identical(self$spatial_path, "memory")) {
        out <- "memory"
      } else {
        out <- paste0(".../", basename(self$spatial_path))
      }
      out
    },

    #' @description
    #' Import the data into memory.
    import = function() {
      # if data files are not stored in memory, then import them
      ## spatial data
      if (is.null(self$spatial_data)) {
        self$spatial_data <- read_spatial_data(self$spatial_path)
      }
      ## attribute data
      if (is.null(self$attribute_data)) {
        ### unzip data file if file path is a zip archive
        if (endsWith(self$attribute_path, ".zip")) {
          f1 <- unzip_file(self$attribute_path, ext = "csv")
        } else {
          f1 <- self$attribute_path
        }
        ### import data
        self$attribute_data <-
          tibble::as_tibble(data.table::fread(f1, data.table = FALSE))
      }
      ## boundary data
      if (is.null(self$boundary_data)) {
        ### unzip data file if file path is a zip archive
        if (endsWith(self$boundary_path, ".zip")) {
          f1 <- unzip_file(self$boundary_path, ext = "csv")
        } else {
          f1 <- self$boundary_path
        }
        ### import data
        bd <- data.table::fread(f1, data.table = FALSE)
        ### find dimensions
        if (inherits(self$spatial_data, "sf")) {
          n_total_units <- nrow(self$spatial_data)
        } else {
          n_total_units <- raster::ncell(self$spatial_data[[1]])
        }
        ### import matrix
        self$boundary_data <-
          Matrix::sparseMatrix(
            i = bd[[1]], j = bd[[2]], x = bd[[3]],
            index1 = TRUE, repr = "C",
            dims = rep(n_total_units, 2)
          )
      }
      invisible(self)
    },

    #' @description
    #' Clean the dataset from memory.
    #' @details
    #' Note that this method has no effect if the dataset does not have
    #' file path on disk.
    clean = function() {
      ## spatial data
      if (!identical(self$spatial_path, "memory")) {
        self$spatial_data <- NULL
      }
      ## attribute data
      if (!identical(self$attribute_path, "memory")) {
        self$attribute_data <- NULL
      }
      ## boundary data
      if (!identical(self$boundary_path, "memory")) {
        self$boundary_data <- NULL
      }
      invisible(self)
    },

    #' @description
    #' Get the spatial data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_spatial_data = function() {
      self$import()
      self$spatial_data
    },

    #' @description
    #' Get the attribute data.
    #' @return [tibble::tibble()] object.
    get_attribute_data = function() {
      self$import()
      self$attribute_data
    },

    #' @description
    #' Get the spatial data.
    #' @return [Matrix::sparseMatrix()] object.
    get_boundary_data = function() {
      self$import()
      self$boundary_data
    },

    #' @description
    #' Get the coordinate reference system.
    #' @return [sf::st_crs()] object.
    get_crs = function() {
      self$import()
      sf::st_crs(self$spatial_data)
    },

    #' @description
    #' Get planning unit indices.
    #' @return `integer` vector of indices.
    get_planning_unit_indices = function() {
      self$import()
      self$attribute_data[["_index"]]
    },

    #' @description
    #' Get area values.
    #' @return `numeric` vector of values.
    get_planning_unit_areas = function() {
      self$import()
      if (inherits(self$spatial_data, "Raster")) {
        out <-
          rep(prod(raster::res(self$spatial_data)), nrow(self$attribute_data))
      } else {
        out <- as.numeric(sf::st_area(self$spatial_data))
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
      self$import()
      if (inherits(self$data, "Raster")) {
        out <- raster::setValues(self$spatial_data, NA_real_)
        self$spatial_data[self$attribute_data[["_index"]]] <-
          self$attribute_data[[index]]
      } else {
        out <- sf::st_st(
          x = self$attribute_data[[index]],
          geometry = sf::st_geometry(self$spatial_data))
      }
      if (is.character(index)) {
        names(out)[[1]] <- index
      } else {
        names(out)[[1]] <- paste0("V", index)
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
      self$import

      ## TODO
      if (inherits(self$data, "Raster")) {
        out <- raster::setValues(self$spatial_data, NA_real_)
        self$spatial_data[self$attribute_data[["_index"]]] <-
          self$attribute_data[[index]]
      } else {
        out <- sf::st_st(
          x = self$attribute_data[[index]],
          geometry = sf::st_geometry(self$spatial_data))
      }
      if (is.character(index)) {
        names(out)[[1]] <- index
      } else {
        names(out)[[1]] <- paste0("V", index)
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
